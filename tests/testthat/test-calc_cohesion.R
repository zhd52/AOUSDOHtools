test_that("calc_cohesion works with valid input", {
  # Valid input: 4 responses for each person
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 2, 2, 2, 2),
    question_concept_id = c(40192463, 40192411, 40192499, 40192417,
                            40192463, 40192411, 40192499, 40192417),
    answer_concept_id = c(40192514, 40192455, 40192524, 40192408,
                          40192514, 40192455, 40192422, 40192408)
  )

  # Expected output for valid input (cohesion scores based on the answers)
  expected_output <- data.frame(
    person_id = c(1, 2),
    cohesion = c(3.5, 3.0)  # Calculated means for each person
  )

  # Call the function and compare with expected output
  result <- calc_cohesion(survey_df)
  expect_equal(result$cohesion, expected_output$cohesion)
})



test_that("calc_cohesion handles incomplete responses", {
  # Incomplete responses: person 2 has fewer than 4 answers
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 2, 2, 2),
    question_concept_id = c(40192463, 40192411, 40192499, 40192417,
                            40192463, 40192411, 40192499),
    answer_concept_id = c(40192514, 40192455, 40192524, 40192408,
                          40192514, 40192455, 40192422)
  )

  # Expected output: person 1 gets a cohesion score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    cohesion = c(3.5, NA_real_)
  )

  result <- calc_cohesion(survey_df)
  expect_equal(result$cohesion, expected_output$cohesion)
})



test_that("calc_cohesion handles missing responses", {
  # Some of the responses are not valid (invalid answer concept ids)
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 2, 2, 2, 2),
    question_concept_id = c(40192463, 40192411, 40192499, 40192417,
                            40192463, 40192411, 40192499, 40192417),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999, # invalid answers for person 1
                          40192514, 40192455, 40192422, 40192408) # valid for person 2
  )

  # Expected output: person 1 has no valid answers, so NA; person 2 gets a valid cohesion score
  expected_output <- data.frame(
    person_id = c(1, 2),
    cohesion = c(NA_real_, 3.0)
  )

  result <- calc_cohesion(survey_df)

  # Sort both result and expected output to avoid order issues
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$cohesion, expected_output$cohesion)
})



test_that("calc_cohesion handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_cohesion(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_cohesion handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 1, 1, 1),
    wrong_question_id = c(40192463, 40192411, 40192499, 40192417),
    wrong_answer_id = c(40192514, 40192455, 40192524, 40192408)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_cohesion(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_cohesion returns NA for participants without any valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 2, 2, 2, 2),
    question_concept_id = c(40192463, 40192411, 40192499, 40192417,
                            40192463, 40192411, 40192499, 40192417),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999,
                          99999999, 99999999, 99999999, 99999999) # invalid answers for everyone
  )

  result <- calc_cohesion(survey_df)

  # Both persons should have NA scores as none of the answers are valid
  expected_output <- data.frame(
    person_id = c(1, 2),
    cohesion = c(NA_real_, NA_real_)
  )

  # Sort both result and expected output to avoid order issues
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$cohesion, expected_output$cohesion)
})
