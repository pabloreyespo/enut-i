# Tests for filtrar_salarios_extremos(). Run from REPO: Rscript tests/test_salarios.R
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(testthat)})
source("data_processing/processing_functions.R")

# 20 workers with w between 10 and 29 (log w IQR is well defined), plus non-workers
base_data <- function(extra_w = numeric(0)) {
  w <- c(10:29, extra_w)
  trab <- tibble(id = seq_along(w), t_to = 10, ing_trab = w * 10)
  nowork <- tibble(id = 1000 + 1:3, t_to = c(0, 0, 5), ing_trab = c(0, 500, 0))
  bind_rows(trab, nowork)
}

test_that("both tails of log wage are dropped", {
  d <- base_data(c(1e6, 1e-3))
  out <- filtrar_salarios_extremos(d, k = 3)
  expect_false(any(out$id %in% c(21, 22)))
  expect_equal(nrow(out), nrow(d) - 2)
})

test_that("non-workers are kept untouched", {
  d <- base_data(c(1e6))
  out <- filtrar_salarios_extremos(d, k = 3)
  expect_true(all(c(1001, 1002, 1003) %in% out$id))
})

test_that("nothing is dropped when there are no outliers", {
  d <- base_data()
  out <- filtrar_salarios_extremos(d, k = 3)
  expect_equal(out, d)
})

test_that("k controls the width of the fence", {
  d <- base_data(c(60))
  expect_equal(nrow(filtrar_salarios_extremos(d, k = 3)), nrow(d))
  expect_lt(nrow(filtrar_salarios_extremos(d, k = 0.5)), nrow(d))
})

test_that("input without workers is returned unchanged", {
  d <- tibble(id = 1:2, t_to = c(0, 0), ing_trab = c(0, 0))
  expect_equal(filtrar_salarios_extremos(d), d)
})
