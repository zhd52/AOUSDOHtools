test_that("calc_spa works with valid input", {
  # Valid input with different levels of SPA
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1,  # 7 responses for person 1
                  2, 2, 2, 2, 2, 2, 2), # 7 responses for person 2
    question_concept_id = rep(c(40192436, 40192440, 40192437, 40192431, 40192410,
                                40192492, 40192414), 2),
    answer_concept_id = c(40192514, 40192514, 40192514, 40192514, 40192514, 40192514, 40192514,  # All "Strongly agree" (4)
                          40192422, 40192422, 40192422, 40192422, 40192422, 40192422, 40192422)  # All "Strongly disagree" (1)
  )

  # Expected output: correctly calculated sum scores
  expected_output <- data.frame(
    person_id = c(1, 2),
    spa = c(22, 13)  # Maximum and minimum scores
  )

  # Call the function and compare with expected output
  result <- calc_spa(survey_df)
  expect_equal(result$spa, expected_output$spa)
})



test_that("calc_spa handles incomplete responses", {
  # Person 2 has fewer than 7 responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1,  # 7 responses for person 1
                  2, 2, 2, 2, 2),       # Only 5 responses for person 2
    question_concept_id = c(rep(c(40192436, 40192440, 40192437, 40192431, 40192410,
                                  40192492, 40192414), 1),
                            c(40192436, 40192440, 40192437, 40192431, 40192410)), # Missing 2 responses for person 2
    answer_concept_id = c(rep(40192514, 7), rep(40192514, 5))  # All "Strongly agree" (4)
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    spa = c(22, NA_real_)
  )

  result <- calc_spa(survey_df)
  expect_equal(result$spa, expected_output$spa)
})



test_that("calc_spa handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1,  # 7 responses for person 1
                  2, 2, 2, 2, 2, 2, 2), # 7 responses for person 2
    question_concept_id = rep(c(40192436, 40192440, 40192437, 40192431, 40192410,
                                40192492, 40192414), 2),
    answer_concept_id = c(40192478, 40192478, 40192478, 40192478, 40192478, 40192478, 40192478, # All "Somewhat agree" (3) for person 1
                          40192478, 903096, 40192478, 40192478, 903096, 40192478, 40192478)  # Skipped some for person 2
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    spa = c(19, NA_real_)
  )

  result <- calc_spa(survey_df)
  expect_equal(result$spa, expected_output$spa)
})



test_that("calc_spa handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    question_concept_id = c(40192436, 40192440, 40192437, 40192431, 40192410,
                            40192492, 40192414),
    answer_concept_id = c(NA, NA, NA, NA, NA, NA, NA)  # All NA responses
  )

  # Expected output: all should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    spa = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  result <- calc_spa(survey_df)
  expect_equal(result$spa, expected_output$spa)
})



test_that("calc_spa handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_spa(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_spa handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192436, 40192440, 40192437),
    wrong_answer_id = c(40192514, 40192455, 40192408)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_spa(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_spa returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    question_concept_id = c(40192436, 40192440, 40192437, 40192431, 40192410,
                            40192492, 40192414),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999)  # All invalid responses
  )

  result <- calc_spa(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    spa = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  expect_equal(result$spa, expected_output$spa)
})
