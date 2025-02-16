test_that("calc_hcd_ever works with valid input", {
  # Valid input with different discrimination experiences
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1,  # 7 responses for person 1
                  2, 2, 2, 2, 2, 2, 2), # 7 responses for person 2
    question_concept_id = rep(c(40192383, 40192394, 40192423, 40192425, 40192497, 40192503, 40192505), 2),
    answer_concept_id = c(40192465, 40192465, 40192481, 40192429, 40192382, 40192515, 40192465, # Person 1: 4 positive, 3 negative
                          40192465, 40192465, 40192465, 40192465, 40192465, 40192465, 40192465)  # Person 2: All negative
  )

  # Expected output: person 1 has experienced discrimination (TRUE), person 2 has not (FALSE)
  expected_output <- data.frame(
    person_id = c(1, 2),
    hcd_ever = c(TRUE, FALSE)
  )

  # Call the function and compare with expected output
  result <- calc_hcd_ever(survey_df)
  expect_equal(result$hcd_ever, expected_output$hcd_ever)
})



test_that("calc_hcd_ever handles participants with no endorsed discrimination", {
  # All answers are 'Never' (40192465), meaning hcd_ever should be FALSE
  survey_df <- data.frame(
    person_id = rep(1, 7),
    question_concept_id = c(40192383, 40192394, 40192423, 40192425, 40192497, 40192503, 40192505),
    answer_concept_id = rep(40192465, 7)  # Never for all
  )

  # Expected output: hcd_ever = FALSE
  expected_output <- data.frame(
    person_id = c(1),
    hcd_ever = c(FALSE)
  )

  result <- calc_hcd_ever(survey_df)
  expect_equal(result$hcd_ever, expected_output$hcd_ever)
})



test_that("calc_hcd_ever handles incomplete responses", {
  # Person 2 has fewer than 7 responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1,  # 7 responses for person 1
                  2, 2, 2, 2, 2),        # Only 5 responses for person 2
    question_concept_id = c(rep(c(40192383, 40192394, 40192423, 40192425, 40192497, 40192503, 40192505), 1),
                            c(40192383, 40192394, 40192423, 40192425, 40192497)), # Missing 2 for person 2
    answer_concept_id = c(rep(40192481, 7), rep(40192481, 5))  # All positive responses
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    hcd_ever = c(TRUE, TRUE)
  )

  result <- calc_hcd_ever(survey_df)
  expect_equal(result$hcd_ever, expected_output$hcd_ever)
})



test_that("calc_hcd_ever handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1,  # 7 responses for person 1
                  2, 2, 2, 2, 2, 2, 2), # 7 responses for person 2
    question_concept_id = rep(c(40192383, 40192394, 40192423, 40192425, 40192497, 40192503, 40192505), 2),
    answer_concept_id = c(40192481, 40192481, 40192481, 40192481, 40192481, 40192481, 40192481, # All positive for person 1
                          40192481, 903096, 40192481, 40192481, 903096, 40192481, 40192481)  # Skipped some for person 2
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    hcd_ever = c(TRUE, TRUE)
  )

  result <- calc_hcd_ever(survey_df)
  expect_equal(result$hcd_ever, expected_output$hcd_ever)
})



test_that("calc_hcd_ever handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    question_concept_id = c(40192383, 40192394, 40192423, 40192425, 40192497, 40192503, 40192505),
    answer_concept_id = c(NA, NA, NA, NA, NA, NA, NA)  # All NA responses
  )

  # Expected output: all should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    hcd_ever = c(NA, NA, NA, NA, NA, NA, NA)
  )

  result <- calc_hcd_ever(survey_df)
  expect_equal(result$hcd_ever, expected_output$hcd_ever)
})



test_that("calc_hcd_ever handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_hcd_ever(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_hcd_ever handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192383, 40192394, 40192423),
    wrong_answer_id = c(40192481, 40192481, 40192481)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_hcd_ever(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_hcd_ever returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    question_concept_id = c(40192383, 40192394, 40192423, 40192425, 40192497, 40192503, 40192505),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999)  # All invalid responses
  )

  result <- calc_hcd_ever(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    hcd_ever = c(NA, NA, NA, NA, NA, NA, NA)
  )

  expect_equal(result$hcd_ever, expected_output$hcd_ever)
})

