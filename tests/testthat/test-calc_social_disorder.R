test_that("calc_social_disorder works with valid input", {
  # Valid input with different levels of social disorder
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1,  # 7 responses for person 1
                  2, 2, 2, 2, 2, 2, 2), # 7 responses for person 2
    question_concept_id = rep(c(40192500, 40192493, 40192457, 40192476, 40192404, 40192400, 40192384), 2),
    answer_concept_id = c(40192514, 40192514, 40192514, 40192514, 40192514, 40192514, 40192514,  # 4
                          40192422, 40192422, 40192422, 40192422, 40192422, 40192422, 40192422)  # 1
  )

  # Expected output: correctly calculated mean scores (reverse-coded items adjusted)
  expected_output <- data.frame(
    person_id = c(1, 2),
    social_disorder = c(3.14, 1.86)  # Mean scores after adjusting reverse-coded items
  )

  # Call the function and compare with expected output
  result <- calc_social_disorder(survey_df)
  expect_equal(result$social_disorder, expected_output$social_disorder)
})



test_that("calc_social_disorder handles incomplete responses", {
  # Person 2 has fewer than 7 responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1,  # 7 responses for person 1
                  2, 2, 2, 2, 2),       # Only 5 responses for person 2
    question_concept_id = c(rep(c(40192500, 40192493, 40192457, 40192476, 40192404, 40192400, 40192384), 1),
                            c(40192500, 40192493, 40192457, 40192476, 40192404)), # Missing 2 for person 2
    answer_concept_id = c(rep(40192514, 7), rep(40192514, 5))  # All "Strongly agree" (4)
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    social_disorder = c(3.14, NA_real_)
  )

  result <- calc_social_disorder(survey_df)
  expect_equal(result$social_disorder, expected_output$social_disorder)
})



test_that("calc_social_disorder handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1,  # 7 responses for person 1
                  2, 2, 2, 2, 2, 2, 2), # 7 responses for person 2
    question_concept_id = rep(c(40192500, 40192493, 40192457, 40192476, 40192404, 40192400, 40192384), 2),
    answer_concept_id = c(40192514, 40192514, 40192514, 40192514, 40192514, 40192514, 40192514, # All "Strongly Agree - 4" for person 1
                          40192514, 903096, 40192514, 40192514, 903096, 40192514, 40192514)  # Skipped some for person 2
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    social_disorder = c(3.14, NA_real_)
  )

  result <- calc_social_disorder(survey_df)
  expect_equal(result$social_disorder, expected_output$social_disorder)
})



test_that("calc_social_disorder handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    question_concept_id = c(40192500, 40192493, 40192457, 40192476, 40192404, 40192400, 40192384),
    answer_concept_id = c(NA, NA, NA, NA, NA, NA, NA)  # All NA responses
  )

  # Expected output: all should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    social_disorder = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  result <- calc_social_disorder(survey_df)
  expect_equal(result$social_disorder, expected_output$social_disorder)
})



test_that("calc_social_disorder handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_social_disorder(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_social_disorder handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192500, 40192493, 40192457),
    wrong_answer_id = c(40192514, 40192455, 40192408)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_social_disorder(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_social_disorder returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    question_concept_id = c(40192500, 40192493, 40192457, 40192476, 40192404, 40192400, 40192384),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999)  # All invalid responses
  )

  result <- calc_social_disorder(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    social_disorder = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  expect_equal(result$social_disorder, expected_output$social_disorder)
})

