test_that("calc_english_level works with valid input", {
  # Valid input with Yes/No responses
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192529, 40192529, 40192529, 40192529),
    answer = c("Very well", "Not well", "Very well", "Not well")
  )

  # Expected output with English proficiency levels
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    english_level = c("Very well", "Not well", "Very well", "Not well")
  )

  # Call the function and compare with expected output
  result <- calc_english_level(survey_df)
  expect_equal(result$english_level, expected_output$english_level)
})



test_that("calc_english_level handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192529, 40192529, 40192529, 40192529),
    answer = c("Very well", "Not well", "Skip", "Skip")
  )

  # Expected output: skipped responses should result in NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    english_level = c("Very well", "Not well", NA_character_, NA_character_)
  )

  result <- calc_english_level(survey_df)
  expect_equal(result$english_level, expected_output$english_level)
})



test_that("calc_english_level handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer = character(0)
  )

  result <- calc_english_level(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_english_level handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192529, 40192529, 40192529),
    wrong_answer = c("Very well", "Not well", "Skip")
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_english_level(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_english_level returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192529, 40192529, 40192529, 40192529),
    answer = c("Skip", "Skip", "Skip", "Skip") # All invalid
  )

  result <- calc_english_level(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    english_level = c(NA_character_, NA_character_, NA_character_, NA_character_)
  )

  expect_equal(result$english_level, expected_output$english_level)
})
