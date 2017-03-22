context("Fetching data from openFDA device events")

test_that("`openfda` raw fetch", {
  expect_warning(res0 <- openfda("", limit = 10))
  res <- as_tibble(res0)
  expect_equal(nrow(res), 10)
  expect_equal(ncol(res), 77)
  expect_is(res0, "RawDeviceEvent")
})

test_that("`openfda` counter fetch", {
  res0 <- openfda("", count_var = "date_facility_aware")
  res <- as_tibble(res0)
  expect_is(res0, "CountDeviceEvent")
  expect_equal(ncol(res), 2)
})

test_that("Methods on RawData", {
  expect_warning(res0 <- openfda("", limit = 10))
  res <- as_tibble(res0)
  expect_equal(nrow(res0), nrow(res))
  expect_equal(ncol(res0), ncol(res))
  expect_equal(dim(res0[1:3,]), c(3,77))
  expect_equal(dim(res0[1:3,2:4]), c(3,3))
  expect_equal(dim(res0[1:3,"date_manufacturer_received"]), c(3,1))
  expect_equal(dim(head(res0)), c(6,77))
  expect_equal(dim(tail(res0)), c(6,77))
  df <- as.data.frame(res0)
  expect_equal(dim(df), c(10,77))
  expect_is(df, "data.frame")
})

test_that("Methods on CountData", {
  res0 <- openfda("", count_var = "date_facility_aware")
  res <- as_tibble(res0)
  expect_equal(nrow(res0), nrow(res))
  expect_equal(ncol(res0), ncol(res))
  expect_equal(dim(res0[1:3,]), c(3,2))
  expect_equal(dim(res0[1:3,1]), c(3,1))
  expect_equal(dim(res0[1:3,"count"]), c(3,1))
  expect_equal(dim(head(res0)), c(6,2))
  expect_equal(dim(tail(res0)), c(6,2))
  df <- as.data.frame(res0)
  expect_equal(dim(df), c(11066,2))
  expect_is(df, "data.frame")
})

test_that("unfold methods", {
  expect_warning(res0 <- openfda("", limit = 10))
  expect_is(unfold(res0), class(res0))
})

test_that("subsetting methods", {
  expect_warning(res0 <- openfda("", limit = 10))
  expect_is(res0[,1]@result, class(tibble::tibble()))
  expect_is(res0[,"patient"]@result, class(tibble::tibble()))
  expect_is(res0[1,"patient"]@result, class(tibble::tibble()))
  expect_is(res0[1,1]@result, class(tibble::tibble()))
})
