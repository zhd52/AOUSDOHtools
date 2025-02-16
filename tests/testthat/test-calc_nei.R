test_that("calc_nei works with valid input", {
  # Valid input with different neighborhood environments
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1,  # 6 responses for person 1
                  2, 2, 2, 2, 2, 2), # 6 responses for person 2
    question_concept_id = rep(c(40192410, 40192431, 40192436, 40192437, 40192440, 40192458), 2),
    answer_concept_id = c(40192422, 40192422, 40192422, 40192422, 40192422, 40192407,  # 0
                          40192514, 40192514, 40192514, 40192514, 40192514, 40192418)  # 1
  )

  # Expected output: correctly calculated sum scores
  expected_output <- data.frame(
    person_id = c(1, 2),
    nei = c(0, 6)  # Sum of responses
  )

  # Call the function and compare with expected output
  result <- calc_nei(survey_df)
  expect_equal(result$nei, expected_output$nei)
})



test_that("calc_nei handles mixed responses", {
  # One participant has mixed responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1),
    question_concept_id = c(40192410, 40192431, 40192436, 40192437, 40192440, 40192458),
    answer_concept_id = c(40192422, 40192514, 40192422, 40192514, 40192422, 40192418)
  )

  # Expected output: correctly categorized sum score
  expected_output <- data.frame(
    person_id = c(1),
    nei = c(3)
  )

  result <- calc_nei(survey_df)
  expect_equal(result$nei, expected_output$nei)
})



test_that("calc_nei handles incomplete responses", {
  # Person 2 has fewer than 6 responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1,  # 6 responses for person 1
                  2, 2, 2, 2, 2),     # 5 responses for person 2
    question_concept_id = c(rep(c(40192410, 40192431, 40192436, 40192437, 40192440, 40192458), 1),
                            c(40192410, 40192431, 40192436, 40192437, 40192440)), # Missing 1 for person 2
    answer_concept_id = c(40192422, 40192422, 40192422, 40192422, 40192422, 40192407,  # 0
                          40192514, 40192514, 40192514, 40192514, 40192514)  # 1
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    nei = c(0, NA_real_)
  )

  result <- calc_nei(survey_df)
  expect_equal(result$nei, expected_output$nei)
})



test_that("calc_nei handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1,  # 6 responses for person 1
                  2, 2, 2, 2, 2, 2), # 6 responses for person 2
    question_concept_id = rep(c(40192410, 40192431, 40192436, 40192437, 40192440, 40192458), 2),
    answer_concept_id = c(40192422, 40192422, 40192422, 40192422, 40192422, 40192407,  # 0
                          40192514, 903096, 40192514, 903096, 40192514, 40192418)      # Skipped some for person 2
  )

  # Expected output: person 1 gets a valid score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    nei = c(0, NA_real_)
  )

  result <- calc_nei(survey_df)
  expect_equal(result$nei, expected_output$nei)
})



test_that("calc_nei handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6),
    question_concept_id = c(40192410, 40192431, 40192436, 40192437, 40192440, 40192458),
    answer_concept_id = c(NA, NA, NA, NA, NA, NA)  # All NA responses
  )

  # Expected output: all should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6),
    nei = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  result <- calc_nei(survey_df)
  expect_equal(result$nei, expected_output$nei)
})



test_that("calc_nei handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_nei(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_nei handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192410, 40192431, 40192436),
    wrong_answer_id = c(40192422, 40192422, 40192422)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_nei(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_nei returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6),
    question_concept_id = c(40192410, 40192431, 40192436, 40192437, 40192440, 40192458),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999, 99999999, 99999999)  # All invalid responses
  )

  result <- calc_nei(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6),
    nei = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )

  expect_equal(result$nei, expected_output$nei)
})

