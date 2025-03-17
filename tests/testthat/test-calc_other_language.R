test_that("calc_other_language works with valid input", {
  # Valid input with different responses
  survey_df <- data.frame(
    person_id = c(1, 2, 3),
    question_concept_id = c(40192526, 40192526, 40192526),
    answer = c("Yes", "No", "Prefer Not To Answer")  # Different responses
  )

  # Expected output: correctly categorized language use
  expected_output <- data.frame(
    person_id = c(1, 2, 3),
    other_language = c("Yes", "No", "Prefer Not To Answer")
  )

  # Call the function and compare with expected output
  result <- calc_other_language(survey_df)
  expect_equal(result$other_language, expected_output$other_language)
})



test_that("calc_other_language handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192526, 40192526, 40192526, 40192526),
    answer = c("Yes", "No", "Skip", "Skip")
  )

  # Expected output: skipped responses should result in NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    other_language = c("Yes", "No", NA_character_, NA_character_)
  )

  result <- calc_other_language(survey_df)
  expect_equal(result$other_language, expected_output$other_language)
})



test_that("calc_other_language handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192526, 40192526, 40192526, 40192526),
    answer = c("Yes", "No", NA, NA)
  )

  # Expected output: missing responses should remain NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    other_language = c("Yes", "No", NA_character_, NA_character_)
  )

  result <- calc_other_language(survey_df)
  expect_equal(result$other_language, expected_output$other_language)
})



test_that("calc_other_language handles unexpected responses", {
  # Some participants enter unexpected responses
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192526, 40192526, 40192526, 40192526),
    answer = c("Yes", "No", "Maybe", "Sometimes")  # "Maybe" and "Sometimes" are not expected
  )

  # Expected output: unexpected responses should remain as they are
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    other_language = c("Yes", "No", "Maybe", "Sometimes")
  )

  result <- calc_other_language(survey_df)
  expect_equal(result$other_language, expected_output$other_language)
})



test_that("calc_other_language handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer = character(0)
  )

  result <- calc_other_language(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_other_language handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192526, 40192526, 40192526),
    wrong_answer = c("Yes", "No", "Prefer Not To Answer")
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_other_language(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_other_language returns NA for participants with no valid responses", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192526, 40192526, 40192526, 40192526),
    answer = c("Skip", "Skip", "Skip", "Skip")  # All invalid answers
  )

  result <- calc_other_language(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    other_language = c(NA_character_, NA_character_, NA_character_, NA_character_)
  )

  expect_equal(result$other_language, expected_output$other_language)
})
