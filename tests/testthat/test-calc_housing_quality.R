test_that("calc_housing_quality works with valid input", {
  # Valid input with different housing issues reported
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 5),
    question_concept_id = c(40192402, 40192402, 40192402, 40192402, 40192402, 40192402),
    answer_concept_id = c(40192392, 40192479, 40192444, 40192495, 40192392, 40192434)
    # 40192392 = None of the above, 40192479, etc. = housing problems
  )

  # Expected output: correctly classified as having housing problems (TRUE) or not (FALSE)
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5),
    housing_quality = c(FALSE, TRUE, TRUE, TRUE, TRUE) # Person 5 has mixed responses but has at least one issue
  )

  # Call the function and compare with expected output
  result <- calc_housing_quality(survey_df)
  expect_equal(result$housing_quality, expected_output$housing_quality)
})



test_that("calc_housing_quality handles multiple responses for the same participant", {
  # One participant selects multiple housing problems
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 2, 2),
    question_concept_id = c(40192402, 40192402, 40192402, 40192402, 40192402),
    answer_concept_id = c(40192479, 40192444, 40192495, 40192392, 40192460)
    # Person 1 has multiple housing problems, person 2 has one "None of the above"
  )

  # Expected output: correctly categorized
  expected_output <- data.frame(
    person_id = c(1, 2),
    housing_quality = c(TRUE, TRUE) # Person 2 has one valid issue response
  )

  result <- calc_housing_quality(survey_df)
  expect_equal(result$housing_quality, expected_output$housing_quality)
})



test_that("calc_housing_quality handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192402, 40192402, 40192402, 40192402),
    answer_concept_id = c(40192392, 40192495, NA, NA)
  )

  # Expected output: missing responses should result in NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    housing_quality = c(FALSE, TRUE, NA, NA)
  )

  result <- calc_housing_quality(survey_df)
  expect_equal(result$housing_quality, expected_output$housing_quality)
})



test_that("calc_housing_quality handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_housing_quality(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_housing_quality handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192402, 40192402, 40192402),
    wrong_answer_id = c(40192479, 40192460, 40192392)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_housing_quality(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_housing_quality returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192402, 40192402, 40192402, 40192402),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999) # All invalid responses
  )

  result <- calc_housing_quality(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    housing_quality = c(NA, NA, NA, NA)
  )

  expect_equal(result$housing_quality, expected_output$housing_quality)
})

