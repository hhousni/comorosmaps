library(comorosmaps)

# comoros() ------------------------------------------------------------------

test_that("comoros() returns an sf object", {
  result <- comoros()
  expect_s3_class(result, "sf")
})

test_that("comoros() with x='country' returns at least 1 row", {
  result <- comoros(x = "country", pref = FALSE)
  expect_gte(nrow(result), 1L)
})

test_that("comoros() with x='island' returns at least 3 rows", {
  result <- comoros(x = "island", pref = FALSE)
  expect_gte(nrow(result), 3L)
})

test_that("comoros() with pref=TRUE returns more rows than pref=FALSE", {
  without_pref <- comoros(x = "country", pref = FALSE)
  with_pref    <- comoros(x = "country", pref = TRUE)
  expect_gt(nrow(with_pref), nrow(without_pref))
})

test_that("comoros() with invalid x throws an error", {
  expect_error(comoros(x = "invalid"))
})

# grandeComore() -------------------------------------------------------------

test_that("grandeComore() returns an sf object", {
  result <- grandeComore()
  expect_s3_class(result, "sf")
})

test_that("grandeComore() with pref=TRUE returns more rows than pref=FALSE", {
  without_pref <- grandeComore(pref = FALSE)
  with_pref    <- grandeComore(pref = TRUE)
  expect_gt(nrow(with_pref), nrow(without_pref))
})

# anjouan() ------------------------------------------------------------------

test_that("anjouan() returns an sf object", {
  result <- anjouan()
  expect_s3_class(result, "sf")
})

test_that("anjouan() with pref=TRUE returns more rows than pref=FALSE", {
  without_pref <- anjouan(pref = FALSE)
  with_pref    <- anjouan(pref = TRUE)
  expect_gt(nrow(with_pref), nrow(without_pref))
})

# moheli() -------------------------------------------------------------------

test_that("moheli() returns an sf object", {
  result <- moheli()
  expect_s3_class(result, "sf")
})

test_that("moheli() with pref=TRUE returns more rows than pref=FALSE", {
  without_pref <- moheli(pref = FALSE)
  with_pref    <- moheli(pref = TRUE)
  expect_gt(nrow(with_pref), nrow(without_pref))
})
