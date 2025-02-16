test_that("calc_spirit works with valid input", {
  # Valid input with different levels of spiritual experiences
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1,  # 6 responses for person 1
                  2, 2, 2, 2, 2, 2), # 6 responses for person 2
    question_concept_id = rep(c(40192401, 40192415, 40192443, 40192471, 40192475, 40192498), 2),
    answer_concept_id = c(40192403, 40192403, 40192403, 40192403, 40192403, 40192403,  # All "Many times a day" (6)
                          40192487, 40192487, 40192487, 40192487, 40192487, 40192487)  # All "I do not believe in God" (1)
  )

  # Expected output: correctly calculated sum scores
  expected_output <- data.frame(
    person_id = c(1, 2),
    spirit = c(36, 6)  # Maximum and minimum scores
  )

  # Call the function and compare with expected output
  result <- calc_spirit(survey_df)
  expect_equal(result$spirit, expected_output$spirit)
})



test_that("calc_spirit handles incomplete responses", {
  # Person 2 has fewer than 6 responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1,  # 6 responses for person 1
                  2, 2, 2, 2, 2),     # Only 5 responses for person 2
    question_concept_id = c(rep(c(40192401, 40192415, 40192443, 40192471, 40192475, 40192498), 1),
                            c(40192401, 40192415, 40192443, 40192471, 40192475)), # Missing 1 response for person 2
    answer_concept_id = c(rep(40192403, 6), rep(40192403, 5))  # All "Many times a day" (6)
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    spirit = c(36, NA_real_)
  )

  result <- calc_spirit(survey_df)
  expect_equal(result$spirit, expected_output$spirit)
})



test_that("calc_spirit handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1,  # 6 responses for person 1
                  2, 2, 2, 2, 2, 2), # 6 responses for person 2
    question_concept_id = rep(c(40192401, 40192415, 40192443, 40192471, 40192475, 40192498), 2),
    answer_concept_id = c(40192459, 40192459, 40192459, 40192459, 40192459, 40192459, # All "Once in a while" (2) for person 1
                          40192459, 903096, 40192459, 40192459, 903096, 40192459)  # Skipped some for person 2
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    spirit = c(12, NA_real_)
  )

  result <- calc_spirit(survey_df)
  expect_equal(result$spirit, expected_output$spirit)
})



test_that("calc_spirit handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6),
    question_concept_id = c(40192401, 40192415, 40192443, 40192471, 40192475, 40192498),
    answer_concept_id = c(NA, NA, NA, NA, NA, NA)  # All NA responses
  )

  # Expected output: all should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6),
    spirit = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  result <- calc_spirit(survey_df)
  expect_equal(result$spirit, expected_output$spirit)
})



test_that("calc_spirit handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_spirit(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_spirit handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192401, 40192415, 40192443),
    wrong_answer_id = c(40192403, 40192455, 40192408)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_spirit(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_spirit returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6),
    question_concept_id = c(40192401, 40192415, 40192443, 40192471, 40192475, 40192498),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999, 99999999, 99999999)  # All invalid responses
  )

  result <- calc_spirit(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6),
    spirit = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  expect_equal(result$spirit, expected_output$spirit)
})
