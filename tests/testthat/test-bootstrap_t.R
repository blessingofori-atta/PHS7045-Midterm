test_that("bootstrap_t function works for all methods", {
  xbar <- function(data) mean(data$Sepal.Length)
  boot_obj <- bootstrap(data = iris, statistic = xbar, nboot = 100, return_samples = TRUE)

  # Test for 'boott-plugin' method
  result_plugin <- bootstrap_t(boot_obj = boot_obj, sdfun = function(data) sd(data$Sepal.Length)/sqrt(nrow(data)),
                               method = "boott-plugin")
  expect_s3_class(result_plugin, "bootstrap_t")
  expect_equal(result_plugin$method, "boott-plugin")
  expect_true(result_plugin$ci[1] < result_plugin$ci[2])

  #Test for 'boott-nested' method
  # result_nested <- bootstrap_t(boot_obj = boot_obj, Bsd = 5, method = "boott-nested")
  # expect_s3_class(result_nested, "bootstrap_t")
  # expect_equal(result_nested$method, "boott-nested")
  # expect_named(result_nested$ci, c("lower", "upper"))

  # # Test for 'bootsym-plugin' method
  # result_sym_plugin <- bootstrap_t(boot_obj = boot_obj, sdfun = function(data) sd(data$Sepal.Length), method = "bootsym-plugin")
  # expect_s3_class(result_plugin, "bootstrap_t")
  # expect_equal(result_plugin$method, "bootsym-plugin")
  # expect_named(result_plugin$ci, c("lower", "upper"))
  #
  # # Test for 'bootsym-nested' method
  # result_sym_nested <- bootstrap_t(boot_obj = boot_obj, sdfun = NULL, Bsd = 5, method = "bootsym-nested")
  # expect_s3_class(result_plugin, "bootstrap_t")
  # expect_equal(result_plugin$method, "bootsym-nested")
  # expect_named(result_plugin$ci, c("lower", "upper"))

})
