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

# commune() ------------------------------------------------------------------

test_that("commune() returns an sf object", {
  result <- commune()
  expect_s3_class(result, "sf")
})

test_that("commune() returns 55 communes for all islands", {
  result <- commune(island = "all")
  expect_equal(nrow(result), 55L)
})

test_that("commune() filters correctly by island", {
  gc <- commune(island = "grande comore")
  an <- commune(island = "anjouan")
  mo <- commune(island = "moheli")
  expect_gt(nrow(gc), 0L)
  expect_gt(nrow(an), 0L)
  expect_gt(nrow(mo), 0L)
  expect_equal(nrow(gc) + nrow(an) + nrow(mo), 55L)
})

test_that("commune() with city=TRUE returns more rows", {
  without_city <- commune(island = "grande comore", city = FALSE)
  with_city    <- commune(island = "grande comore", city = TRUE)
  expect_gt(nrow(with_city), nrow(without_city))
})

test_that("commune() with invalid island throws an error", {
  expect_error(commune(island = "invalid"))
})
