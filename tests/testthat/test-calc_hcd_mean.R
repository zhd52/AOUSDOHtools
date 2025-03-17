test_that("calc_hcd_mean works with valid input", {
  # Valid input with different levels of discrimination experiences
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1,  # 7 responses for person 1
                  2, 2, 2, 2, 2, 2, 2), # 7 responses for person 2
    question_concept_id = rep(c(40192383, 40192394, 40192423, 40192425, 40192497, 40192503, 40192505), 2),
    answer_concept_id = c(40192465, 40192481, 40192429, 40192382, 40192515, 40192465, 40192481,  # Person 1: Mean = 2.57
                          40192515, 40192515, 40192515, 40192515, 40192515, 40192515, 40192515)  # Person 2: Mean = 5.00
  )

  # Expected output: correctly calculated mean scores
  expected_output <- data.frame(
    person_id = c(1, 2),
    hcd_mean = c(2.57, 5.00)
  )

  # Call the function and compare with expected output
  result <- calc_hcd_mean(survey_df)
  expect_equal(result$hcd_mean, expected_output$hcd_mean)
})



test_that("calc_hcd_mean handles participants with no discrimination", {
  # All answers are 'Never' (40192465), meaning hcd_mean should be 1.0
  survey_df <- data.frame(
    person_id = rep(1, 7),
    question_concept_id = c(40192383, 40192394, 40192423, 40192425, 40192497, 40192503, 40192505),
    answer_concept_id = rep(40192465, 7)  # Never for all
  )

  # Expected output: hcd_mean = 1.0
  expected_output <- data.frame(
    person_id = c(1),
    hcd_mean = c(1.0)
  )

  result <- calc_hcd_mean(survey_df)
  expect_equal(result$hcd_mean, expected_output$hcd_mean)
})



test_that("calc_hcd_mean handles incomplete responses", {
  # Person 2 has fewer than 7 responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1,  # 7 responses for person 1
                  2, 2, 2, 2, 2),        # Only 5 responses for person 2
    question_concept_id = c(rep(c(40192383, 40192394, 40192423, 40192425, 40192497, 40192503, 40192505), 1),
                            c(40192383, 40192394, 40192423, 40192425, 40192497)), # Missing 2 for person 2
    answer_concept_id = c(rep(40192481, 7), rep(40192481, 5))  # All "Rarely" (2)
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    hcd_mean = c(2.0, NA_real_)
  )

  result <- calc_hcd_mean(survey_df)
  expect_equal(result$hcd_mean, expected_output$hcd_mean)
})



test_that("calc_hcd_mean handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1,  # 7 responses for person 1
                  2, 2, 2, 2, 2, 2, 2), # 7 responses for person 2
    question_concept_id = rep(c(40192383, 40192394, 40192423, 40192425, 40192497, 40192503, 40192505), 2),
    answer_concept_id = c(40192481, 40192481, 40192481, 40192481, 40192481, 40192481, 40192481, # All "Rarely" for person 1
                          40192481, 903096, 40192481, 40192481, 903096, 40192481, 40192481)  # Skipped some for person 2
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    hcd_mean = c(2.0, NA_real_)
  )

  result <- calc_hcd_mean(survey_df)
  expect_equal(result$hcd_mean, expected_output$hcd_mean)
})



test_that("calc_hcd_mean handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    question_concept_id = c(40192383, 40192394, 40192423, 40192425, 40192497, 40192503, 40192505),
    answer_concept_id = c(NA, NA, NA, NA, NA, NA, NA)  # All NA responses
  )

  # Expected output: all should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    hcd_mean = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  result <- calc_hcd_mean(survey_df)
  expect_equal(result$hcd_mean, expected_output$hcd_mean)
})



test_that("calc_hcd_mean handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_hcd_mean(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_hcd_mean handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192383, 40192394, 40192423),
    wrong_answer_id = c(40192481, 40192481, 40192481)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_hcd_mean(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_hcd_mean returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    question_concept_id = c(40192383, 40192394, 40192423, 40192425, 40192497, 40192503, 40192505),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999)  # All invalid responses
  )

  result <- calc_hcd_mean(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    hcd_mean = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  expect_equal(result$hcd_mean, expected_output$hcd_mean)
})

