test_that("calc_ins_support works with valid input", {
  # Valid input with different levels of instrumental support
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 2, 2, 2, 2),
    question_concept_id = c(40192388, 40192442, 40192480, 40192511,
                            40192388, 40192442, 40192480, 40192511),
    answer_concept_id = c(40192521, 40192521, 40192521, 40192521,  # All of the time (5)
                          40192454, 40192454, 40192454, 40192454)  # None of the time (1)
  )

  # Expected output for valid input (scaled from 0 to 100)
  expected_output <- data.frame(
    person_id = c(1, 2),
    ins_support = c(100, 0)  # Full support vs no support
  )

  # Call the function and compare with expected output
  result <- calc_ins_support(survey_df)
  expect_equal(result$ins_support, expected_output$ins_support)
})



test_that("calc_ins_support handles incomplete responses", {
  # Incomplete responses: person 2 has fewer than 4 answers
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 2, 2, 2),
    question_concept_id = c(40192388, 40192442, 40192480, 40192511,
                            40192388, 40192442, 40192480),
    answer_concept_id = c(40192521, 40192521, 40192521, 40192521,
                          40192454, 40192454, 40192454)
  )

  # Expected output: person 1 gets a score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    ins_support = c(100, NA_real_)
  )

  result <- calc_ins_support(survey_df)
  expect_equal(result$ins_support, expected_output$ins_support)
})



test_that("calc_ins_support handles missing responses", {
  # Some of the responses are invalid (invalid answer concept ids)
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 2, 2, 2, 2),
    question_concept_id = c(40192388, 40192442, 40192480, 40192511,
                            40192388, 40192442, 40192480, 40192511),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999, # invalid answers for person 1
                          40192486, 40192486, 40192486, 40192486) # valid for person 2
  )

  # Expected output: person 1 has no valid answers, so NA; person 2 gets a valid support score
  expected_output <- data.frame(
    person_id = c(1, 2),
    ins_support = c(NA_real_, 50.0) # Person 2 has mixed responses
  )

  result <- calc_ins_support(survey_df)

  # Sort both result and expected output to avoid order issues
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$ins_support, expected_output$ins_support)
})



test_that("calc_ins_support handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_ins_support(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_ins_support handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 1, 1, 1),
    wrong_question_id = c(40192388, 40192442, 40192480, 40192511),
    wrong_answer_id = c(40192521, 40192454, 40192486, 40192518)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_ins_support(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_ins_support returns NA for participants without any valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 2, 2, 2, 2),
    question_concept_id = c(40192388, 40192442, 40192480, 40192511,
                            40192388, 40192442, 40192480, 40192511),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999,
                          99999999, 99999999, 99999999, 99999999) # invalid answers for everyone
  )

  result <- calc_ins_support(survey_df)

  # Both persons should have NA scores as none of the answers are valid
  expected_output <- data.frame(
    person_id = c(1, 2),
    ins_support = c(NA_real_, NA_real_)
  )

  # Sort both result and expected output to avoid order issues
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$ins_support, expected_output$ins_support)
})

