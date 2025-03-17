test_that("calc_loneliness works with valid input", {
  # Valid input with different levels of loneliness
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1, 1,  # 8 responses for person 1
                  2, 2, 2, 2, 2, 2, 2, 2), # 8 responses for person 2
    question_concept_id = rep(c(40192390, 40192397, 40192398, 40192494, 40192501, 40192504, 40192507, 40192516), 2),
    answer_concept_id = c(40192481, 40192429, 40192429, 40192465, 40192481, 40192482, 40192481, 40192429,  # Mixed values, reverse-coded items adjusted
                          40192482, 40192482, 40192482, 40192482, 40192482, 40192465, 40192482, 40192465)  # High loneliness
  )

  # Expected output: correctly calculated sum scores (reverse-coded items adjusted)
  expected_output <- data.frame(
    person_id = c(1, 2),
    loneliness = c(16, 32)  # Sum scores calculated after adjusting reverse-coded items
  )

  # Call the function and compare with expected output
  result <- calc_loneliness(survey_df)
  expect_equal(result$loneliness, expected_output$loneliness)
})



test_that("calc_loneliness handles incomplete responses", {
  # Person 2 has fewer than 8 responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1, 1,  # 8 responses for person 1
                  2, 2, 2, 2, 2, 2),        # Only 6 responses for person 2
    question_concept_id = c(rep(c(40192390, 40192397, 40192398, 40192494, 40192501, 40192504, 40192507, 40192516), 1),
                            c(40192390, 40192397, 40192398, 40192494, 40192501, 40192504)), # Missing 2 for person 2
    answer_concept_id = c(rep(40192481, 8), rep(40192481, 6))  # All "Rarely" (2)
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    loneliness = c(18, NA_real_)
  )

  result <- calc_loneliness(survey_df)
  expect_equal(result$loneliness, expected_output$loneliness)
})



test_that("calc_loneliness handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1, 1,  # 8 responses for person 1
                  2, 2, 2, 2, 2, 2, 2, 2), # 8 responses for person 2
    question_concept_id = rep(c(40192390, 40192397, 40192398, 40192494, 40192501, 40192504, 40192507, 40192516), 2),
    answer_concept_id = c(40192481, 40192481, 40192481, 40192481, 40192481, 40192481, 40192481, 40192481, # All "Rarely" for person 1
                          40192481, 903096, 40192481, 40192481, 903096, 40192481, 40192481, 40192481)  # Skipped some for person 2
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    loneliness = c(18, NA_real_)
  )

  result <- calc_loneliness(survey_df)
  expect_equal(result$loneliness, expected_output$loneliness)
})



test_that("calc_loneliness handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    question_concept_id = c(40192390, 40192397, 40192398, 40192494, 40192501, 40192504, 40192507),
    answer_concept_id = c(NA, NA, NA, NA, NA, NA, NA)  # All NA responses
  )

  # Expected output: all should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    loneliness = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  result <- calc_loneliness(survey_df)
  expect_equal(result$loneliness, expected_output$loneliness)
})



test_that("calc_loneliness handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_loneliness(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_loneliness handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192390, 40192397, 40192398),
    wrong_answer_id = c(40192481, 40192481, 40192481)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_loneliness(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_loneliness returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    question_concept_id = c(40192390, 40192397, 40192398, 40192494, 40192501, 40192504, 40192507),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999)  # All invalid responses
  )

  result <- calc_loneliness(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    loneliness = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  expect_equal(result$loneliness, expected_output$loneliness)
})
