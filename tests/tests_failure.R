## Unit tests for the dataqualitycontrol package

#source("./R/batch_quality_check.R", chdir = T)

library("testthat")


test_that("does batch_issue_checks() fail correctly on bad inputs?", {


    expect_error(batch_issue_checks(NULL, NULL, NULL, NULL))
    expect_error(batch_issue_checks(df, NULL, NULL, NULL))
    expect_error(batch_issue_checks(df, conditions, NULL, NULL))
    expect_error(batch_issue_checks(df, NULL, tests, NULL))
    expect_error(batch_issue_checks(df, NULL, NULL, meta_to_keep = (NULL)))

})



test_that("does run_checks_from_dataframe() fails correctly on bad inputs?", {

   expect_error(run_checks_from_dataframe(NULL, NULL, NULL, NULL, NULL))
   expect_error(run_checks_from_dataframe(df, NULL, NULL, NULL, NULL))
   expect_error(run_checks_from_dataframe(df, conditions_df, NULL, NULL, NULL))
   expect_error(run_checks_from_dataframe(df, NULL, condition.column, NULL, NULL))
   expect_error(run_checks_from_dataframe(df, NULL, NULL, test.name.column, NULL))
   expect_error(run_checks_from_dataframe(df, NULL, condition.column, NULL, NULL))
   expect_error(run_checks_from_dataframe(df, NULL, NULL, NULL, meta_to_keep = (NULL)))

})



test_that("does quality_log_to_long_format() fails correctly on bad inputs?", {

   expect_error(quality_checks_log_to_long_format(NULL, NULL, NULL, NULL))
   expect_error(quality_checks_log_to_long_format(data, NULL, NULL, NULL))
   expect_error(quality_checks_log_to_long_format(df, variable_name, NULL, NULL))
   expect_error(quality_checks_log_to_long_format(df, NULL, value_name, NULL))
   expect_error(quality_checks_log_to_long_format(df, NULL, NULL, meta_not_to_transform = (NULL)))


})


test_that("does separate_on_multiple() fails correctly on bad inputs?", {

   expect_error(quality_checks_log_to_long_format(NULL, NULL, NULL, NULL, NULL))
   expect_error(quality_checks_log_to_long_format(data, NULL, NULL, NULL, NULL))
   expect_error(quality_checks_log_to_long_format(df, var_to_separate, NULL, NULL, NULL))
   expect_error(quality_checks_log_to_long_format(df, NULL, sep1, NULL, NULL))
   expect_error(quality_checks_log_to_long_format(df, NULL, NULL, sep2, NULL))
   expect_error(quality_checks_log_to_long_format(df, NULL, NULL, sep2, sep3))

})


test_that("does anonymise_dataset() fails correctly on bad inputs?", {

   expect_error(quality_checks_log_to_long_format(NULL, NULL))
   expect_error(quality_checks_log_to_long_format(data, NULL))
   expect_error(quality_checks_log_to_long_format(NULL, variables_to_remove))

})
