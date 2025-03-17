test_that("calc_religious_attendance works with valid input", {
  # Valid input with different attendance frequencies
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5),
    question_concept_id = c(40192470, 40192470, 40192470, 40192470, 40192470),
    answer = c("Never or almost never", "Once a week", "Less than once per month", "More than once a week", "1 to 3 times per month")
  )

  # Expected output: correctly categorized religious attendance
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5),
    religious_attendance = c("Never or almost never", "Once a week", "Less than once per month", "More than once a week", "1 to 3 times per month")
  )

  # Call the function and compare with expected output
  result <- calc_religious_attendance(survey_df)
  expect_equal(result$religious_attendance, expected_output$religious_attendance)
})



test_that("calc_religious_attendance handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192470, 40192470, 40192470, 40192470),
    answer = c("Never or almost never", "Once a week", "Skip", "Skip")
  )

  # Expected output: skipped responses should result in NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    religious_attendance = c("Never or almost never", "Once a week", NA_character_, NA_character_)
  )

  result <- calc_religious_attendance(survey_df)
  expect_equal(result$religious_attendance, expected_output$religious_attendance)
})



test_that("calc_religious_attendance handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192470, 40192470, 40192470, 40192470),
    answer = c("Never or almost never", "Once a week", NA, NA)
  )

  # Expected output: missing responses should remain NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    religious_attendance = c("Never or almost never", "Once a week", NA_character_, NA_character_)
  )

  result <- calc_religious_attendance(survey_df)
  expect_equal(result$religious_attendance, expected_output$religious_attendance)
})



test_that("calc_religious_attendance handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer = character(0)
  )

  result <- calc_religious_attendance(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_religious_attendance handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192470, 40192470, 40192470),
    wrong_answer = c("Never", "Once a week", "More than once a week")
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_religious_attendance(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_religious_attendance returns NA for participants with no valid responses", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192470, 40192470, 40192470, 40192470),
    answer = c("Skip", "Skip", "Skip", "Skip")  # All invalid answers
  )

  result <- calc_religious_attendance(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    religious_attendance = c(NA_character_, NA_character_, NA_character_, NA_character_)
  )

  expect_equal(result$religious_attendance, expected_output$religious_attendance)
})
