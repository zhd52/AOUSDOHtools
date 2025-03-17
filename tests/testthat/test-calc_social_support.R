test_that("calc_social_support works with valid input", {
  # Valid input with different levels of social support
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1, 1,  # 8 responses for person 1
                  2, 2, 2, 2, 2, 2, 2, 2), # 8 responses for person 2
    question_concept_id = rep(c(40192388, 40192399, 40192439, 40192442, 40192446,
                                40192480, 40192511, 40192528), 2),
    answer_concept_id = c(40192521, 40192521, 40192521, 40192521, 40192521, 40192521, 40192521, 40192521,  # All "All of the time" (5)
                          40192454, 40192454, 40192454, 40192454, 40192454, 40192454, 40192454, 40192454)  # All "None of the time" (1)
  )

  # Expected output: correctly calculated scores (scaled from 0 to 100)
  expected_output <- data.frame(
    person_id = c(1, 2),
    social_support = c(100, 0)  # Maximum and minimum scores
  )

  # Call the function and compare with expected output
  result <- calc_social_support(survey_df)
  expect_equal(result$social_support, expected_output$social_support)
})



test_that("calc_social_support handles incomplete responses", {
  # Person 2 has fewer than 8 responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1, 1,  # 8 responses for person 1
                  2, 2, 2, 2, 2, 2, 2),    # 7 responses for person 2
    question_concept_id = c(rep(c(40192388, 40192399, 40192439, 40192442, 40192446,
                                  40192480, 40192511, 40192528), 1),
                            c(40192388, 40192399, 40192439, 40192442, 40192446, 40192480, 40192511)), # Missing 1 response for person 2
    answer_concept_id = c(rep(40192521, 8), rep(40192521, 7))  # All "All of the time" (5)
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    social_support = c(100, NA_real_)
  )

  result <- calc_social_support(survey_df)
  expect_equal(result$social_support, expected_output$social_support)
})



test_that("calc_social_support handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1, 1,  # 8 responses for person 1
                  2, 2, 2, 2, 2, 2, 2, 2), # 8 responses for person 2
    question_concept_id = rep(c(40192388, 40192399, 40192439, 40192442, 40192446,
                                40192480, 40192511, 40192528), 2),
    answer_concept_id = c(40192486, 40192486, 40192486, 40192486, 40192486, 40192486, 40192486, 40192486, # All "Some of the time" (3) for person 1
                          40192486, 903096, 40192486, 40192486, 903096, 40192486, 40192486, 40192486)  # Skipped some for person 2
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    social_support = c(50, NA_real_)
  )

  result <- calc_social_support(survey_df)
  expect_equal(result$social_support, expected_output$social_support)
})



test_that("calc_social_support handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7, 8),
    question_concept_id = c(40192388, 40192399, 40192439, 40192442, 40192446,
                            40192480, 40192511, 40192528),
    answer_concept_id = c(NA, NA, NA, NA, NA, NA, NA, NA)  # All NA responses
  )

  # Expected output: all should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7, 8),
    social_support = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  result <- calc_social_support(survey_df)
  expect_equal(result$social_support, expected_output$social_support)
})



test_that("calc_social_support handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_social_support(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_social_support handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192388, 40192399, 40192439),
    wrong_answer_id = c(40192521, 40192455, 40192408)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_social_support(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_social_support returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7, 8),
    question_concept_id = c(40192388, 40192399, 40192439, 40192442, 40192446,
                            40192480, 40192511, 40192528),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999)  # All invalid responses
  )

  result <- calc_social_support(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7, 8),
    social_support = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  expect_equal(result$social_support, expected_output$social_support)
})

