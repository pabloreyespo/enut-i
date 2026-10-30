# Tests for the joint distribution inference (JDI) expenditure imputation.
# Run from enut-i: Rscript tests/test_jdi.R
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(testthat)})
source("data_processing/processing_functions.R")

# --- synthetic EPF (donors) ------------------------------------------------------------------------
# dimensions: quintil (1-2), zona (1-2), n15 (1-2), ntrab (0-1)
cuadricula <- expand.grid(quintil = 1:2, zona = 1:2, n15 = 1:2, ntrab = 0:1)

test_that("ipf_multivariado reproduces one- and two-way margins and keeps zero cells at zero", {
  set.seed(1)
  perfiles <- cuadricula[rep(seq_len(nrow(cuadricula)), each = 3), ]
  w <- runif(nrow(perfiles), 0.5, 3)
  # target: a joint distribution with the same cells; margins come from it, so they are feasible
  p <- runif(nrow(cuadricula)); p <- p / sum(p)
  objetivo <- cuadricula
  r <- ipf_multivariado(perfiles, w, objetivo, w_objetivo = p, tol = 1e-8)
  expect_true(r$convergio)
  expect_equal(sum(r$w), 1)
  key <- function(d, v) do.call(paste, c(d[v], sep = "|"))
  dims <- names(cuadricula)
  margenes <- c(as.list(dims), combn(dims, 2, simplify = FALSE))
  for (m in margenes) {
    est <- tapply(r$w, key(perfiles, m), sum)
    obj <- tapply(p, key(objetivo, m), sum)
    expect_equal(as.vector(est[names(obj)]), as.vector(obj), tolerance = 1e-8)
  }
  # an EPF record with weight zero stays at zero
  w0 <- w; w0[5] <- 0
  r0 <- ipf_multivariado(perfiles, w0, objetivo, w_objetivo = p, tol = 1e-8)
  expect_equal(r0$w[5], 0)
})

test_that("ipf_multivariado fails loudly when it cannot converge", {
  # two EPF cells only, but the target asks for margins that no reweighting of them can match
  perfiles <- data.frame(a = c(1, 2), b = c(1, 2))
  objetivo <- data.frame(a = c(1, 2, 1), b = c(1, 2, 2))
  r <- ipf_multivariado(perfiles, c(1, 1), objetivo, w_objetivo = c(0.4, 0.4, 0.2), max_iter = 50, error_si_falla = FALSE)
  expect_true(is.finite(r$masa_sin_soporte))
  expect_equal(r$masa_sin_soporte, 0.2)
})

test_that("chi-square flags an independent dimension and keeps a dependent one", {
  set.seed(2)
  n <- 4000
  clase <- sample(1:3, n, replace = TRUE)
  dependiente <- ifelse(runif(n) < 0.8, clase, sample(1:3, n, replace = TRUE))
  independiente <- sample(1:2, n, replace = TRUE)
  w <- runif(n, 0.5, 2)
  t_dep <- chi2_independencia(dependiente, clase, w)
  t_ind <- chi2_independencia(independiente, clase, w)
  expect_lt(t_dep$p, 0.05)
  expect_gt(t_dep$V, 0.3)
  expect_gt(t_ind$p, 0.05)
  expect_equal(t_dep$gl, 4)

  epf <- data.frame(quintil = sample(1:5, n, replace = TRUE), dep = dependiente, ind = independiente,
                    c1 = clase, c2 = clase, c3 = sample(1:3, n, replace = TRUE))
  tab <- pruebas_independencia(epf, c("dep", "ind"), c("c1", "c2", "c3"), w)
  expect_equal(nrow(tab), 6)
  sel <- dimensiones_retenidas(tab, c("dep", "ind"))
  expect_equal(sel$retenidas, "dep")
  expect_equal(names(sel$V), "dep")
  # quintil is kept even if it were independent
  sel2 <- dimensiones_retenidas(tab, c("dep", "ind"), fijas = "ind")
  expect_setequal(sel2$retenidas, c("dep", "ind"))
})

# --- draws -----------------------------------------------------------------------------------------
# donors: every profile has 12 households except the cell (quintil 2, zona 2, n15 2, ntrab 1)
donantes <- cuadricula[rep(seq_len(nrow(cuadricula)), each = 12), ]
rara <- with(donantes, quintil == 2 & zona == 2 & n15 == 2 & ntrab == 1)
donantes <- donantes[!rara | seq_len(nrow(donantes)) %in% which(rara)[1:3], ]  # only 3 donors in that cell
donantes$id <- seq_len(nrow(donantes))
Rdon <- cbind(a = donantes$id, b = donantes$id * 2)  # ratio matrix identifies the donor
wdon <- rep(1, nrow(donantes))
V <- c(zona = 0.5, n15 = 0.1, ntrab = 0.3)  # n15 weakest, then ntrab, then zona

test_that("a cell with enough donors uses only its own donors", {
  enut <- data.frame(quintil = 1, zona = 1, n15 = 1, ntrab = 0)
  s <- sorteo_jdi(enut, donantes[, c("quintil", "zona", "n15", "ntrab")], wdon, V, m = 5,
                  matrices = list(R = Rdon), min_donantes = 10, semilla = 7)
  expect_equal(s$nivel, "quintil+zona+n15+ntrab")
  ids <- donantes$id[donantes$quintil == 1 & donantes$zona == 1 & donantes$n15 == 1 & donantes$ntrab == 0]
  # the mean of 5 draws from the cell lies within the range of the cell's donors
  expect_gte(s$R[1, "a"], min(ids)); expect_lte(s$R[1, "a"], max(ids))
})

test_that("a sparse cell drops the weakest dimension first and never the quintile", {
  perf <- donantes[, c("quintil", "zona", "n15", "ntrab")]
  enut <- data.frame(quintil = 2, zona = 2, n15 = 2, ntrab = 1)
  s <- sorteo_jdi(enut, perf, wdon, V, m = 200, matrices = list(R = Rdon), min_donantes = 10, semilla = 7)
  # dropping n15 gives cell quintil 2, zona 2, ntrab 1 = 12 (n15 = 1) + 3 = 15 donors
  expect_equal(s$nivel, "quintil+zona+ntrab")
  ok <- donantes$id[donantes$quintil == 2 & donantes$zona == 2 & donantes$ntrab == 1]
  expect_gte(s$R[1, "a"], min(ok)); expect_lte(s$R[1, "a"], max(ok))
  # with a minimum that nobody meets the quintile alone is the last level
  s2 <- sorteo_jdi(enut, perf, wdon, V, m = 5, matrices = list(R = Rdon), min_donantes = 1000, semilla = 7)
  expect_equal(s2$nivel, "quintil")
  okq <- donantes$id[donantes$quintil == 2]
  expect_gte(s2$R[1, "a"], min(okq)); expect_lte(s2$R[1, "a"], max(okq))
})

test_that("draws are reproducible with a fixed seed", {
  perf <- donantes[, c("quintil", "zona", "n15", "ntrab")]
  enut <- cuadricula[rep(1:16, 3), ]
  a <- sorteo_jdi(enut, perf, wdon, V, m = rep(1:3, 16), matrices = list(R = Rdon), semilla = 2023)
  b <- sorteo_jdi(enut, perf, wdon, V, m = rep(1:3, 16), matrices = list(R = Rdon), semilla = 2023)
  d <- sorteo_jdi(enut, perf, wdon, V, m = rep(1:3, 16), matrices = list(R = Rdon), semilla = 1)
  expect_identical(a, b)
  expect_false(identical(a$R, d$R))
})

test_that("draws follow the raked weights", {
  perf <- donantes[, c("quintil", "zona", "n15", "ntrab")]
  enut <- data.frame(quintil = 1, zona = 1, n15 = 1, ntrab = 0)
  idx <- which(perf$quintil == 1 & perf$zona == 1 & perf$n15 == 1 & perf$ntrab == 0)
  w <- wdon; w[idx[-1]] <- 0  # all the mass on one donor
  s <- sorteo_jdi(enut, perf, w, V, m = 4, matrices = list(R = Rdon), min_donantes = 1, semilla = 3)
  expect_equal(unname(s$R[1, "a"]), donantes$id[idx[1]])
})

test_that("dimensiones_jdi bins household size and workers and collapses the ENUT macrozona to two zones", {
  d <- data.frame(quintil = 1:5, macrozona = c("metropolitana", "norte", "centro", "sur", "regiones"),
                  n_15_mas = c(0, 1, 3, 4, 9), n_trabajadores = c(0, 1, 2, 3, 7))
  p <- dimensiones_jdi(d)
  expect_equal(names(p), c("quintil", "zona", "n15", "ntrab"))
  expect_equal(p$zona, c("metropolitana", "regiones", "regiones", "regiones", "regiones"))
  expect_equal(p$n15, c(1, 1, 3, 4, 4))
  expect_equal(p$ntrab, c(0, 1, 2, 3, 3))
})

# --- imputar_jdi end to end ---------------------------------------------------------------------------
alts <- c("alimentos", "vestimenta", "cuentas", "hogar", "salud", "transporte",
          "comunicaciones", "recreacion", "educacion", "restaurantes")

test_that("imputar_jdi returns shares and ratios that add up and a level per household", {
  set.seed(10)
  n <- 600
  epf <- data.frame(
    quintil = sample(1:3, n, replace = TRUE), macrozona = sample(c("metropolitana", "regiones"), n, replace = TRUE),
    n_15_mas = sample(1:5, n, replace = TRUE), n_trabajadores = sample(0:4, n, replace = TRUE),
    fe = runif(n, 100, 400), ingreso_hogar = runif(n, 100, 500) + 100
  )
  g <- matrix(runif(n * 10, 1, 10), n, 10, dimnames = list(NULL, paste0("g.", alts)))
  epf <- cbind(epf, g)
  epf$total_expenses <- rowSums(g)
  epf$savings <- epf$ingreso_hogar - epf$total_expenses
  n_e <- 80
  enut <- data.frame(
    quintil = sample(1:3, n_e, replace = TRUE), macrozona = sample(c("metropolitana", "norte", "centro", "sur"), n_e, replace = TRUE),
    n_15_mas = sample(0:5, n_e, replace = TRUE), n_trabajadores = sample(0:4, n_e, replace = TRUE),
    ingreso_hogar = runif(n_e, 100, 500), fe_hogar = runif(n_e, 100, 400)
  )
  r <- imputar_jdi(enut, epf, semilla = 2023)
  expect_equal(dim(r$razones), c(n_e, 11))
  expect_equal(dim(r$shares), c(n_e, 10))
  expect_equal(unname(rowSums(r$shares)), rep(1, n_e), tolerance = 1e-9)
  expect_equal(unname(rowSums(r$razones)), rep(1, n_e), tolerance = 1e-9)
  expect_length(r$nivel, n_e)
  expect_equal(unname(rowSums(r$razones * enut$ingreso_hogar)), enut$ingreso_hogar, tolerance = 1e-9)
  r2 <- imputar_jdi(enut, epf, semilla = 2023)
  expect_identical(r$razones, r2$razones)
})

test_that("a single draw returns exactly the donor's ratios", {
  perf <- donantes[, c("quintil", "zona", "n15", "ntrab")]
  enut <- data.frame(quintil = 1, zona = 1, n15 = 1, ntrab = 0)
  idx <- which(perf$quintil == 1 & perf$zona == 1 & perf$n15 == 1 & perf$ntrab == 0)
  s <- sorteo_jdi(enut, perf, wdon, V, m = 1, matrices = list(R = Rdon), semilla = 5)
  expect_true(any(Rdon[idx, "a"] == s$R[1, "a"]))
  expect_equal(unname(s$R[1, "b"]), unname(2 * s$R[1, "a"]))
})

test_that("ipf_multivariado converges when some target households have a profile the EPF lacks in a two-way margin", {
  # the EPF has no household with (a = 2, b = 2); the target does, so its margins are only
  # consistent once those households are set aside
  set.seed(4)
  cel <- expand.grid(a = 1:2, b = 1:2, c = 1:3)
  cel <- cel[!(cel$a == 2 & cel$b == 2), ]
  perfiles <- cel[rep(seq_len(nrow(cel)), each = 4), ]
  w <- runif(nrow(perfiles), 1, 3)
  objetivo <- expand.grid(a = 1:2, b = 1:2, c = 1:3)
  p <- runif(nrow(objetivo)); p <- p / sum(p)
  r <- ipf_multivariado(perfiles, w, objetivo, w_objetivo = p, tol = 1e-8)
  expect_true(r$convergio)
  expect_equal(sum(r$w), 1)
  expect_equal(r$masa_sin_soporte, sum(p[objetivo$a == 2 & objetivo$b == 2]))
})
