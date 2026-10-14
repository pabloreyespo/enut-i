# Tests for repartir_ingreso() with ENUT 2015 data. Run from enut-i: Rscript tests/test_ingresos.R
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(testthat)})
source("data_processing/processing_functions.R")

test_that("household questionnaire income goes to members with own income", {
  d <- tibble(id_hogar = c(1, 1, 1), c14_1_1 = c(45, 40, 12), ing_trab = c(300, 100, 0),
              ing_jub_aps = 0, ing_g = c(80, 0, 0))
  d <- repartir_ingreso(d, "id_hogar", "c14_1_1")
  expect_equal(d$ing_gpp, c(60, 20, 0))
  expect_equal(d$ingreso_hogar, rep(480, 3))
})

test_that("household without own income is split among members aged 15+", {
  d <- tibble(id_hogar = c(1, 1, 1, 2), c14_1_1 = c(80, 30, 8, 50), ing_trab = c(0, 0, 0, 200),
              ing_jub_aps = 0, ing_g = c(120, 0, 0, 0))
  d <- repartir_ingreso(d, "id_hogar", "c14_1_1")
  expect_equal(d$ingreso_personal, c(60, 60, 0, 200))
  expect_equal(d$prop_ing_hogar, c(0.5, 0.5, 0, 1))
})

test_that("missing age does not break the split", {
  d <- tibble(id_hogar = c(1, 1), c14_1_1 = c(NA, 40), ing_trab = 0, ing_jub_aps = 0, ing_g = c(100, 0))
  expect_equal(repartir_ingreso(d, "id_hogar", "c14_1_1")$ingreso_personal, c(0, 100))
})
