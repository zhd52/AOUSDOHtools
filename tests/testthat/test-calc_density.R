test_that("calc_density works with valid input", {
  # Valid input: Participants with valid answers for housing type
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192458, 40192458, 40192458, 40192458),
    answer_concept_id = c(40192407, 40192472, 40192409, 40192433) # Low and High responses
  )

  # Expected output: Participant 1 has "Low" density, others have "High"
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    density = c("Low", "High", "High", "High")
  )

  # Call the function and compare with expected output
  result <- calc_density(survey_df)

  # Sort both result and expected output for comparison
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$density, expected_output$density)
})



test_that("calc_density handles non-answers correctly", {
  # Input with non-answer codes or irrelevant answer_concept_ids
  survey_df <- data.frame(
    person_id = c(1, 2, 3),
    question_concept_id = c(40192458, 40192458, 40192458),
    answer_concept_id = c(99999999, 99999998, 99999997) # Invalid codes
  )

  # Expected output: Empty data frame since non-answers are removed
  expected_output <- data.frame(
    person_id = integer(0), # Empty integer vector for person_id
    density = character(0)  # Empty character vector for density
  )

  result <- calc_density(survey_df)

  # Sort both result and expected output
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$density, expected_output$density)
})



test_that("calc_density handles missing responses correctly", {
  # Some responses are missing or invalid
  survey_df <- data.frame(
    person_id = c(1, 2, 3),
    question_concept_id = c(40192458, 40192458, 40192458),
    answer_concept_id = c(903096, 40192407, 99999999) # Missing or invalid responses
  )

  # Expected output: NA for missing response, "Low" for valid, NA for invalid
  expected_output <- data.frame(
    person_id = c(2),
    density = c("Low")  # Only valid responses are expected
  )

  result <- calc_density(survey_df)

  # Sort both result and expected output
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$density, expected_output$density)
})



test_that("calc_density returns empty data frame for no relevant data", {
  # Input where none of the question_concept_id matches
  survey_df <- data.frame(
    person_id = c(1, 2, 3),
    question_concept_id = c(99999999, 99999999, 99999999), # Non-matching concept ID
    answer_concept_id = c(40192407, 40192472, 40192409)
  )

  result <- calc_density(survey_df)

  # Expect the result to be an empty data frame (no matching question_concept_id)
  expect_equal(nrow(result), 0)
})



test_that("calc_density handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_density(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_density handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 1),
    wrong_question_id = c(40192458, 40192458),
    wrong_answer_id = c(40192407, 40192407)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_density(bad_survey_df), "object 'question_concept_id' not found")
})
