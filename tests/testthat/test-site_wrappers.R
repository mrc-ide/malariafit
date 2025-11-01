test_that("basic_parameters_site deals with dates properly", {
  params1 <- basic_parameters_site(example_site, as.Date("2000-01-01"), as.Date("2019-12-31")) #Does it accept date format?
  expect_equal(params1$n_days, 7300)
  params2 <- basic_parameters_site(example_site,"2000-01-01", "2019-12-31") #Does it accept character format?
  expect_equal(params2$n_days, 7300)
  expect_error(basic_parameters_site(examples_site, "01-01-2000", "31-12-2019")) #Doesn't accept weird looking dates
  expect_error(basic_parameters_site(examples_site, "01-01-2000", "2019-31-12")) #Doesn't accept weird looking dates
})
