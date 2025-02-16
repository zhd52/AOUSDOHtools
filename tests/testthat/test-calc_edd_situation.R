test_that("calc_edd_situation works with valid input", {
  # Valid input: Participants with valid answers for all 9 questions
  survey_df <- data.frame(
    person_id = c(rep(1, 9), rep(2, 9)),
    question_concept_id = rep(c(40192380, 40192395, 40192416, 40192451, 40192466, 40192489, 40192490, 40192496, 40192519), 2),
    answer_concept_id = c(rep(40192465, 9), c(rep(40192465, 5), rep(40192461, 4))) # Person 1 answered "Never", person 2 has mix of answers
  )

  # Expected output: Person 1 has a situation score of 0, person 2 has a score of 4 (4 responses other than "Never")
  expected_output <- data.frame(
    person_id = c(1, 2),
    edd_situation = c(0, 4)
  )

  # Call the function and compare with expected output
  result <- calc_edd_situation(survey_df)

  # Sort both result and expected output for comparison
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$edd_situation, expected_output$edd_situation)
})



test_that("calc_edd_situation handles incomplete responses correctly", {
  # Input with some participants missing answers
  survey_df <- data.frame(
    person_id = c(rep(1, 7), rep(2, 9)),
    question_concept_id = c(rep(c(40192380, 40192395, 40192416, 40192451, 40192466, 40192489, 40192490), 1),
                            rep(c(40192380, 40192395, 40192416, 40192451, 40192466, 40192489, 40192490, 40192496, 40192519), 1)),
    answer_concept_id = c(rep(40192465, 7), c(rep(40192465, 5), rep(40192461, 4))) # Person 1 missing 2 questions
  )

  # Expected output: Person 1 gets NA for incomplete responses, person 2 has a score of 4
  expected_output <- data.frame(
    person_id = c(1, 2),
    edd_situation = c(NA_real_, 4)
  )

  result <- calc_edd_situation(survey_df)

  # Sort both result and expected output
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$edd_situation, expected_output$edd_situation)
})



test_that("calc_edd_situation handles missing responses", {
  # Input with missing answer concept IDs
  survey_df <- data.frame(
    person_id = c(rep(1, 9), rep(2, 9)),
    question_concept_id = rep(c(40192380, 40192395, 40192416, 40192451, 40192466, 40192489, 40192490, 40192496, 40192519), 2),
    answer_concept_id = c(rep(NA, 9), c(rep(40192465, 5), rep(40192461, 4))) # Missing answers for person 1
  )

  # Expected output: Person 1 gets NA, person 2 has a score of 4
  expected_output <- data.frame(
    person_id = c(1, 2),
    edd_situation = c(NA_real_, 4) # NA for person 1 with missing answers
  )

  result <- calc_edd_situation(survey_df)

  # Sort both result and expected output
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$edd_situation, expected_output$edd_situation)
})



test_that("calc_edd_situation handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_edd_situation(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_edd_situation handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 1),
    wrong_question_id = c(40192380, 40192395),
    wrong_answer_id = c(40192465, 40192416)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_edd_situation(bad_survey_df), "object 'question_concept_id' not found")
})


