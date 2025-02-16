test_that("calc_disorder works with valid input", {
  # Valid input: Participants with valid answers for all 13 questions
  survey_df <- data.frame(
    person_id = c(rep(1, 13), rep(2, 13)),
    question_concept_id = rep(c(40192420, 40192522, 40192412, 40192469, 40192456, 40192386,
                                40192500, 40192493, 40192457, 40192476, 40192404, 40192400,
                                40192384), 2),
    answer_concept_id = c(rep(40192514, 13), rep(40192422, 13)) # Strongly agree for person 1, Strongly disagree for person 2
  )

  # After reverse-coding 4 items, the expected score should reflect the new values
  expected_output <- data.frame(
    person_id = c(1, 2),
    disorder = c(3.08, 1.92) # Rounded mean after reverse coding
  )

  # Call the function and compare with expected output
  result <- calc_disorder(survey_df)

  # Sort both result and expected output for comparison
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$disorder, expected_output$disorder)
})



test_that("calc_disorder handles incomplete responses correctly", {
  # Input with some participants missing answers
  survey_df <- data.frame(
    person_id = c(rep(1, 10), rep(2, 13)),
    question_concept_id = c(rep(c(40192420, 40192522, 40192412, 40192469, 40192456, 40192386,
                                  40192500, 40192493, 40192457, 40192476), 1), # Person 1 missing 3 questions
                            rep(c(40192420, 40192522, 40192412, 40192469, 40192456, 40192386,
                                  40192500, 40192493, 40192457, 40192476, 40192404, 40192400,
                                  40192384), 1)),
    answer_concept_id = c(rep(40192455, 10), rep(40192514, 13)) # Person 1 has "Agree", Person 2 has "Strongly agree"
  )

  # Expected output: Person 1 gets NA for incomplete responses, person 2 has a disorder score of 4
  expected_output <- data.frame(
    person_id = c(1, 2),
    disorder = c(NA_real_, 3.08) # Person 1 has incomplete answers
  )

  result <- calc_disorder(survey_df)

  # Sort both result and expected output
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$disorder, expected_output$disorder)
})



test_that("calc_disorder handles reverse-coded questions correctly", {
  # Input with reverse-coded and normal questions
  survey_df <- data.frame(
    person_id = c(rep(1, 13)),
    question_concept_id = c(40192420, 40192522, 40192412, 40192469, 40192456, 40192386,
                            40192500, 40192493, 40192457, 40192476, 40192404, 40192400,
                            40192384),
    answer_concept_id = c(40192455, 40192455, 40192455, 40192455, # Agree for all normal items
                          40192408, 40192408, 40192455, 40192455, # Disagree for reverse-coded items
                          40192455, 40192455, 40192455, 40192408, 40192408)
  )

  # Expected output: The reverse-coded items should adjust the score correctly
  expected_output <- data.frame(
    person_id = c(1),
    disorder = c(3.0) # Manually calculated based on reverse coding
  )

  result <- calc_disorder(survey_df)

  # Sort both result and expected output for comparison
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$disorder, expected_output$disorder)
})



test_that("calc_disorder handles missing responses", {
  # Input with missing answer concept IDs
  survey_df <- data.frame(
    person_id = c(rep(1, 13), rep(2, 13)),
    question_concept_id = rep(c(40192420, 40192522, 40192412, 40192469, 40192456, 40192386,
                                40192500, 40192493, 40192457, 40192476, 40192404, 40192400,
                                40192384), 2),
    answer_concept_id = c(rep(NA, 13), rep(40192422, 13)) # Missing answers for person 1, Strongly disagree for person 2
  )

  # Expected output: Person 1 gets NA, person 2 has a disorder score of 1
  expected_output <- data.frame(
    person_id = c(1, 2),
    disorder = c(NA_real_, 1.92) # NA for person 1 with missing answers
  )

  result <- calc_disorder(survey_df)

  # Sort both result and expected output
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$disorder, expected_output$disorder)
})



test_that("calc_disorder handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_disorder(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_disorder handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 1),
    wrong_question_id = c(40192420, 40192412),
    wrong_answer_id = c(40192514, 40192408)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_disorder(bad_survey_df), "object 'question_concept_id' not found")
})
