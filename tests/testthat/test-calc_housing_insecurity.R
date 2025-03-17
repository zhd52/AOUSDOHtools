test_that("calc_housing_insecurity works with valid input", {
  # Valid input with different number of moves
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5),
    question_concept_id = c(40192441, 40192441, 40192441, 40192441, 40192441),
    answer = c("0", "1", "2", "3", "4")  # 0-1 moves = FALSE, 2+ moves = TRUE
  )

  # Expected output: correctly classified as housing insecure (TRUE) or not (FALSE)
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5),
    housing_insecurity = c(FALSE, FALSE, TRUE, TRUE, TRUE)
  )

  # Call the function and compare with expected output
  result <- calc_housing_insecurity(survey_df)
  expect_equal(result$housing_insecurity, expected_output$housing_insecurity)
})



test_that("calc_housing_insecurity handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192441, 40192441, 40192441, 40192441),
    answer = c("0", "2", "Skip", "Skip")
  )

  # Expected output: skipped responses should result in NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    housing_insecurity = c(FALSE, TRUE, NA, NA)
  )

  result <- calc_housing_insecurity(survey_df)
  expect_equal(result$housing_insecurity, expected_output$housing_insecurity)
})



test_that("calc_housing_insecurity handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192441, 40192441, 40192441, 40192441),
    answer = c("0", "3", NA, NA)
  )

  # Expected output: missing responses should remain NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    housing_insecurity = c(FALSE, TRUE, NA, NA)
  )

  result <- calc_housing_insecurity(survey_df)
  expect_equal(result$housing_insecurity, expected_output$housing_insecurity)
})



test_that("calc_housing_insecurity handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer = character(0)
  )

  result <- calc_housing_insecurity(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_housing_insecurity handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192441, 40192441, 40192441),
    wrong_answer = c("0", "2", "3")
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_housing_insecurity(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_housing_insecurity returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192441, 40192441, 40192441, 40192441),
    answer = c("Skip", "Skip", "Skip", "Skip") # All invalid answers
  )

  result <- calc_housing_insecurity(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    housing_insecurity = c(NA, NA, NA, NA)
  )

  expect_equal(result$housing_insecurity, expected_output$housing_insecurity)
})
