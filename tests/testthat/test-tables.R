context("Making tables")

result_vpa    <- load_data("../../inst/extdata/res_vpa_pma.rda")
result_msy    <- load_data("../../inst/extdata/res_MSY_pma_pre.rda")
result_future <- load_data(".../../inst/extdata/res_future_Fcurrent_pma.rda")
yrs_bio       <- 2009:2011
assertthat::assert_that(
  head(colnames(result_vpa$ssb), 1) == "1982",
  tail(colnames(result_vpa$ssb), 1) == "2011"
)
yrs_pre_abc <- 2008:2011

test_colname <- function(tbl) {
  expect_equal(colnames(tbl), c("項目", "値", "備考"))
}
expect_df <- function(tbl) {
  expect_is(tbl, "data.frame")
}

test_that("make_stock_table() works", {
  future_new_object_ <- function() {
    data(res_vpa)
    data(res_sr_HSL2)
    dummy_yr    <- 2015:2017
    future_data <- make_future_data(res_vpa,
                     res_SR = res_sr_HSL2,
                     M_year = dummy_yr,
                     maa_year = dummy_yr,
                     waa_catch_year = dummy_yr,
                     waa_year = dummy_yr)
    force(future_vpa(future_data$data))
  }
  tbl <- make_stock_table(result_vpa        = result_vpa,
                          result_msy        = result_msy,
                          result_future     = future_new_object_(),
                          yr_future_start   = 2012)

  expect_df(tbl)
  expect_equal(colnames(tbl),
               c("Year", "Biomass", "SSB", "Catch", "F.Fmsy", "HarvestRate"))
  expect_equal(tbl$Year,
               2007:2012) # Six years including yr_future_start
  expect_true(all(!is.na(tbl$Biomass)))
})

test_that("table1() works", {
  tbl <- table1(result_vpa = result_vpa,
                yrs_preabc = yrs_pre_abc)
  expect_df(tbl)
  test_colname(tbl)

  expect_equal(tbl$項目, c("SB2011", "F2011"))
  expect_equal(tbl$値[1], as.character(colSums(result_vpa$ssb["2011"])))
})

test_that("table2() works", {
  tbl <- table2(result_vpa = result_vpa,
                yrs_preabc = yrs_pre_abc,
                data_future = generate_dummy_future_data(result_vpa),
                yr_biopar = yrs_bio)
  expect_df(tbl)
  test_colname(tbl)

  expect_equal(tbl$項目, c("%SPR(2011)", "%SPR(2008--2011)"))
})

test_that("make_table() works for fit.SR object", {
  obj <- fit.SR(load_data("../inst/extdata/SRdata_pma.rda"))
  tbl <- make_table(obj)
  expect_df(tbl)
  expect_equal(
    colnames(tbl),
    c("再生産関係式", "最適化法", "自己相関", "a", "b", "S.D.", "rho")
  )
})

test_that("make_msytable() works", {
  tbl <- make_msytable(result_msy)
  expect_df(tbl)
  test_colname(tbl)
  expect_equal(tbl$項目,
               c("SBtarget", "SBlimit", "SBban", "Fmsy", "%SPR (Fmsy)", "MSY"))
  expect_equal(tbl[tbl$項目 == "MSY", ]$値,
               paste0(substr(derive_RP_value(result_msy$summary, "Btarget0")$B, 1, 3), " 千トン"))
})

test_that("table4() works", {
  tbl <- table4(result_vpa = result_vpa,
                yrs_preabc = yrs_pre_abc,
                fmsy       = c(0.123, 0.234, 0.345),
                sbtarget   = 12345,
                result_msy = result_msy)
  expect_df(tbl)
  test_colname(tbl)
  expect_equal(tbl$項目, c("SB2011/ SBtarget(SBmsy)", "F2011/ Fmsy"))
  expect_equal(tbl[tbl$項目 == "F2011/ Fmsy", ]$値,
               make_kobe_ratio(result_vpa, result_msy) %>%
               dplyr::filter(year == 2011) %>%
               dplyr::pull(Fratio))
})

test_that("summary_of_summary() works", {
  tbl <- summary_of_summary(tbl_msy = make_msytable(result_msy),
                            tbl1    = table1(result_vpa = result_vpa,
                                             yrs_preabc = yrs_pre_abc),
                            tbl2    = table2(result_vpa = result_vpa,
                                             yrs_preabc = yrs_pre_abc,
                                             data_future = generate_dummy_future_data(result_vpa),
                                             yr_biopar = yrs_bio) ,
                            tbl4    = table4(result_vpa = result_vpa,
                                             yrs_preabc = yrs_pre_abc,
                                             fmsy = c(0.123, 0.234, 0.345),
                                             sbtarget = 12345,
                                             result_msy = result_msy))
  expect_df(tbl)
  test_colname(tbl)
})

context("Inner functions for making tables")

test_that("make_row() works", {

  row <- make_row(key = "foo", value = 1)

  expect_df(row)
  test_colname(row)
  expect_equal(row$項目, "foo")
  expect_equal(row$値,   1)
  expect_equal(row$備考, "")

  expect_equal(
    make_row(key   = "foo",
             value = "character is allowed for 'value'")$値,
    "character is allowed for 'value'"
  )
  expect_equal(
    make_row(key     = "foo",
             value   = "bar",
             remarks = "you can write remarks here")$備考,
    "you can write remarks here"
  )

  expect_error(
    make_row(key = "foo", value = 1:3),
    "Only one-length vector is allowed"
  )
})

test_that("format_x_at_age() to present vpa results", {
  # Usage in practice
  xaa_in_specific_year <- result_vpa$faa["2011"]
  expect_equal(
    format_x_at_age(xaa_in_specific_year),
    "(0歳, 1歳, 2歳, 3歳以上) = (0.64, 1.23, 1.3, 1.3)"
  )

  # Basic behavior
  expect_equal(
    format_x_at_age(data.frame("foo" = c(1.23, 2.34, 3.45))),
    "(0歳, 1歳, 2歳以上) = (1.23, 2.34, 3.45)"
  )
  expect_equal(
    format_x_at_age(data.frame("foo" = 1:10)),
    "(0歳, 1歳, 2歳, 3歳, 4歳, 5歳, 6歳, 7歳, 8歳, 9歳以上) = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)" # nolint
  )

  # Rounding values
  expect_equal(
    format_x_at_age(data.frame("foo" = c(1.234567, 2.345678, 3.456789))),
    "(0歳, 1歳, 2歳以上) = (1.23, 2.35, 3.46)"
  )
  expect_equal(
    format_x_at_age(
      data.frame("foo" = c(1.234567, 2.345678, 3.456789)),
      round = 1
    ),
    "(0歳, 1歳, 2歳以上) = (1.2, 2.3, 3.5)"
  )
})
