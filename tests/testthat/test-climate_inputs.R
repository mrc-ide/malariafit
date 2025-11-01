test_that("get_gadm_code works for example site", {
  example_site <- get("example_site", envir = asNamespace("malariafit"))
  expect_equal(get_gadm_code(example_site), "BFA.12_1")
})

test_that("Rainfall models work as expected", {
  example_rainfall_data <- get("example_rainfall_data", envir = asNamespace("malariafit"))
  icl <- carrying_capacity_icl(rainfall_df = example_rainfall_data[1:100,], tau = 10, zsat = 2, zmax = 10)
  expect_equal(round(icl[100, "K"], 3), 0.059)
  white <- carrying_capacity_white(rainfall_df = example_rainfall_data[1:100,], tau = 4)
  expect_equal(round(white[100, "K"], 3), 0.018)
})
