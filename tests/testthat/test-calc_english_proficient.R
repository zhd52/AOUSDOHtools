test_that("calc_english_proficient works with valid input", {
  # Valid input with different proficiency levels
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192529, 40192529, 40192529, 40192529),
    answer_concept_id = c(40192435, 40192510, 40192405, 40192387) # Very well, Well, Not well, Not at all
  )

  # Expected output: correct categorization
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    english_proficient = c("Proficient", "Proficient", "Not proficient", "Not proficient")
  )

  # Call the function and compare with expected output
  result <- calc_english_proficient(survey_df)
  expect_equal(result$english_proficient, expected_output$english_proficient)
})



test_that("calc_english_proficient handles 'Unknown' responses", {
  # Some participants have 'Unknown' responses
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192529, 40192529, 40192529, 40192529),
    answer_concept_id = c(40192435, 903087, 903079, 40192510) # Very well, Don't know, Prefer Not To Answer, Well
  )

  # Expected output: 'Unknown' responses should be labeled correctly
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    english_proficient = c("Proficient", "Unknown", "Unknown", "Proficient")
  )

  result <- calc_english_proficient(survey_df)
  expect_equal(result$english_proficient, expected_output$english_proficient)
})



test_that("calc_english_proficient handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192529, 40192529, 40192529, 40192529),
    answer_concept_id = c(40192435, 40192405, NA, NA) # Missing values
  )

  # Expected output: NA responses remain NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    english_proficient = c("Proficient", "Not proficient", NA_character_, NA_character_)
  )

  result <- calc_english_proficient(survey_df)
  expect_equal(result$english_proficient, expected_output$english_proficient)
})



test_that("calc_english_proficient handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_english_proficient(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_english_proficient handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192529, 40192529, 40192529),
    wrong_answer_id = c(40192435, 40192510, 40192405)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_english_proficient(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_english_proficient returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192529, 40192529, 40192529, 40192529),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999) # All invalid
  )

  result <- calc_english_proficient(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    english_proficient = c(NA_character_, NA_character_, NA_character_, NA_character_)
  )

  expect_equal(result$english_proficient, expected_output$english_proficient)
})
