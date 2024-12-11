test_that("bootstrap_t function works for all methods", {
  xbar <- function(data) mean(data$Sepal.Length)
  boot_obj <- bootstrap(data = iris, statistic = xbar, nboot = 100, return_samples = TRUE)

  #Test for 'boott-plugin' method
  result_plugin <- bootstrap_t(boot_obj = boot_obj, sdfun = function(data) sd(data$Sepal.Length)/sqrt(nrow(data)),
                               method = "boott-plugin")
  expect_s3_class(result_plugin, "bootstrap_t")
  expect_equal(result_plugin$method, "boott-plugin")
  expect_true(result_plugin$ci[1] < result_plugin$ci[2])

  #Test for 'boott-nested' method
  result_nested <- bootstrap_t(boot_obj = boot_obj, Bsd = 10, method = "boott-nested")
  expect_s3_class(result_nested, "bootstrap_t")
  expect_equal(result_nested$method, "boott-nested")
  expect_true(result_nested$ci[1] < result_nested$ci[2])

  #Test for 'bootsym-plugin' method
  result_sym_plugin <- bootstrap_t(boot_obj = boot_obj,  sdfun = function(data) sd(data$Sepal.Length)/sqrt(nrow(data)),
                                   method = "bootsym-plugin")
  expect_s3_class(result_sym_plugin, "bootstrap_t")
  expect_equal(result_sym_plugin$method, "bootsym-plugin")
  expect_true(result_sym_plugin$ci[1] < result_sym_plugin$ci[2])

  #Test for 'bootsym-nested' method
  result_sym_nested <- bootstrap_t(boot_obj = boot_obj, Bsd = 10, method = "bootsym-nested")
  expect_s3_class(result_sym_nested, "bootstrap_t")
  expect_equal(result_sym_nested$method, "bootsym-nested")
  expect_true(result_sym_nested$ci[1] < result_sym_nested$ci[2])

})
