test_that("calc_physical_disorder works with valid input", {
  # Valid input with different levels of physical disorder
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1,  # 6 responses for person 1
                  2, 2, 2, 2, 2, 2), # 6 responses for person 2
    question_concept_id = rep(c(40192420, 40192522, 40192412, 40192469, 40192456, 40192386), 2),
    answer_concept_id = c(40192514, 40192514, 40192514, 40192514, 40192514, 40192514,  # 4
                          40192422, 40192422, 40192422, 40192422, 40192422, 40192422)  # 1
  )

  # Expected output: correctly calculated mean scores (reverse-coded items adjusted)
  expected_output <- data.frame(
    person_id = c(1, 2),
    physical_disorder = c(3.0, 2.0)  # Mean scores after adjusting reverse-coded items
  )

  # Call the function and compare with expected output
  result <- calc_physical_disorder(survey_df)
  expect_equal(result$physical_disorder, expected_output$physical_disorder)
})



test_that("calc_physical_disorder handles incomplete responses", {
  # Person 2 has fewer than 6 responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1,  # 6 responses for person 1
                  2, 2, 2, 2, 2),     # Only 5 responses for person 2
    question_concept_id = c(rep(c(40192420, 40192522, 40192412, 40192469, 40192456, 40192386), 1),
                            c(40192420, 40192522, 40192412, 40192469, 40192456)), # Missing 1 for person 2
    answer_concept_id = c(rep(40192514, 6), rep(40192514, 5))  # All "Strongly agree" (4)
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    physical_disorder = c(3.0, NA_real_)
  )

  result <- calc_physical_disorder(survey_df)
  expect_equal(result$physical_disorder, expected_output$physical_disorder)
})



test_that("calc_physical_disorder handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1,  # 6 responses for person 1
                  2, 2, 2, 2, 2, 2), # 6 responses for person 2
    question_concept_id = rep(c(40192420, 40192522, 40192412, 40192469, 40192456, 40192386), 2),
    answer_concept_id = c(40192422, 40192422, 40192422, 40192422, 40192422, 40192422, # All "Strongly Disagree - 1" for person 1
                          40192422, 903096, 40192422, 40192422, 903096, 40192422)  # Skipped some for person 2
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    physical_disorder = c(2.0, NA_real_)
  )

  result <- calc_physical_disorder(survey_df)
  expect_equal(result$physical_disorder, expected_output$physical_disorder)
})



test_that("calc_physical_disorder handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6),
    question_concept_id = c(40192420, 40192522, 40192412, 40192469, 40192456, 40192386),
    answer_concept_id = c(NA, NA, NA, NA, NA, NA)  # All NA responses
  )

  # Expected output: all should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6),
    physical_disorder = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  result <- calc_physical_disorder(survey_df)
  expect_equal(result$physical_disorder, expected_output$physical_disorder)
})



test_that("calc_physical_disorder handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_physical_disorder(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_physical_disorder handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192420, 40192522, 40192412),
    wrong_answer_id = c(40192514, 40192455, 40192408)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_physical_disorder(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_physical_disorder returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6),
    question_concept_id = c(40192420, 40192522, 40192412, 40192469, 40192456, 40192386),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999, 99999999, 99999999)  # All invalid responses
  )

  result <- calc_physical_disorder(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6),
    physical_disorder = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  expect_equal(result$physical_disorder, expected_output$physical_disorder)
})

