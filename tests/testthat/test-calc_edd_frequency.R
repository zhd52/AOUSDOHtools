test_that("calc_edd_frequency works with valid input", {
  # Valid input: Participants with valid answers for all 9 questions
  survey_df <- data.frame(
    person_id = c(rep(1, 9), rep(2, 9)),
    question_concept_id = rep(c(40192380, 40192395, 40192416, 40192451, 40192466, 40192489, 40192490, 40192496, 40192519), 2),
    answer_concept_id = c(rep(40192421, 9), rep(40192453, 9)) # "Almost everyday" for person 1, "A few times a year" for person 2
  )

  # Expected output: Person 1 has the maximum frequency score (6 * 9 = 54), person 2 has a moderate score (3 * 9 = 27)
  expected_output <- data.frame(
    person_id = c(1, 2),
    edd_frequency = c(54, 27) # 6 * 9 = 54, 3 * 9 = 27
  )

  # Call the function and compare with expected output
  result <- calc_edd_frequency(survey_df)

  # Sort both result and expected output for comparison
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$edd_frequency, expected_output$edd_frequency)
})



test_that("calc_edd_frequency handles incomplete responses correctly", {
  # Input with some participants missing answers
  survey_df <- data.frame(
    person_id = c(rep(1, 6), rep(2, 9)),
    question_concept_id = c(rep(c(40192380, 40192395, 40192416, 40192451, 40192466, 40192489), 1),
                            rep(c(40192380, 40192395, 40192416, 40192451, 40192466, 40192489, 40192490, 40192496, 40192519), 1)),
    answer_concept_id = c(rep(40192453, 6), rep(40192421, 9)) # Person 1 missing 3 questions, Person 2 has "Almost everyday"
  )

  # Expected output: Person 1 gets NA for incomplete responses, person 2 has the maximum score (6 * 9 = 54)
  expected_output <- data.frame(
    person_id = c(1, 2),
    edd_frequency = c(NA_real_, 54) # Person 1 has incomplete answers
  )

  result <- calc_edd_frequency(survey_df)

  # Sort both result and expected output
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$edd_frequency, expected_output$edd_frequency)
})



test_that("calc_edd_frequency handles missing responses", {
  # Input with missing answer concept IDs
  survey_df <- data.frame(
    person_id = c(rep(1, 9), rep(2, 9)),
    question_concept_id = rep(c(40192380, 40192395, 40192416, 40192451, 40192466, 40192489, 40192490, 40192496, 40192519), 2),
    answer_concept_id = c(rep(NA, 9), rep(40192421, 9)) # Missing answers for person 1, "Almost everyday" for person 2
  )

  # Expected output: Person 1 gets NA, person 2 has the maximum score (6 * 9 = 54)
  expected_output <- data.frame(
    person_id = c(1, 2),
    edd_frequency = c(NA_real_, 54) # NA for person 1 with missing answers
  )

  result <- calc_edd_frequency(survey_df)

  # Sort both result and expected output
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$edd_frequency, expected_output$edd_frequency)
})



test_that("calc_edd_frequency handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_edd_frequency(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_edd_frequency handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 1),
    wrong_question_id = c(40192380, 40192395),
    wrong_answer_id = c(40192421, 40192421)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_edd_frequency(bad_survey_df), "object 'question_concept_id' not found")
})
