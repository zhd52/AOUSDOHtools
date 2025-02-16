test_that("calc_crime_safety works with valid input", {
  # Valid input: 2 responses for each person
  survey_df <- data.frame(
    person_id = c(1, 1, 2, 2),
    question_concept_id = c(40192414, 40192492, 40192414, 40192492),
    answer_concept_id = c(40192514, 40192478, 40192527, 40192422) # reverse-coded answers
  )

  # Expected output for valid input (crime safety scores based on the answers)
  expected_output <- data.frame(
    person_id = c(1, 2),
    crime_safety = c(1.5, 3.5)  # Calculated means for each person
  )

  # Call the function and compare with expected output
  result <- calc_crime_safety(survey_df)

  # Sort the result to avoid issues with row order
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$crime_safety, expected_output$crime_safety)
})



test_that("calc_crime_safety handles incomplete responses", {
  # Incomplete responses: person 2 has only 1 answer
  survey_df <- data.frame(
    person_id = c(1, 1, 2),
    question_concept_id = c(40192414, 40192492, 40192414),
    answer_concept_id = c(40192514, 40192478, 40192527)
  )

  # Expected output: person 1 gets a crime safety score, person 2 gets NA
  expected_output <- data.frame(
    person_id = c(1, 2),
    crime_safety = c(1.5, NA_real_)  # Use NA_real_ for numeric NA
  )

  result <- calc_crime_safety(survey_df)

  # Sort both result and expected output
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$crime_safety, expected_output$crime_safety)
})



test_that("calc_crime_safety handles missing responses", {
  # Some of the responses are invalid (invalid answer concept ids)
  survey_df <- data.frame(
    person_id = c(1, 1, 2, 2),
    question_concept_id = c(40192414, 40192492, 40192414, 40192492),
    answer_concept_id = c(99999999, 99999999, 40192527, 40192422) # invalid answers for person 1
  )

  # Expected output: person 1 has no valid answers, so NA; person 2 gets a valid crime safety score
  expected_output <- data.frame(
    person_id = c(1, 2),
    crime_safety = c(NA_real_, 3.5)  # Use NA_real_ for numeric NA
  )

  result <- calc_crime_safety(survey_df)

  # Sort both result and expected output
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$crime_safety, expected_output$crime_safety)
})



test_that("calc_crime_safety handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_crime_safety(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_crime_safety handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 1),
    wrong_question_id = c(40192414, 40192492),
    wrong_answer_id = c(40192514, 40192478)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_crime_safety(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_crime_safety returns NA for participants without any valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 1, 2, 2),
    question_concept_id = c(40192414, 40192492, 40192414, 40192492),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999) # invalid answers for everyone
  )

  result <- calc_crime_safety(survey_df)

  # Both persons should have NA scores as none of the answers are valid
  expected_output <- data.frame(
    person_id = c(1, 2),
    crime_safety = c(NA_real_, NA_real_)  # Use NA_real_ for numeric NA
  )

  # Sort both result and expected output
  result <- result[order(result$person_id), ]
  expected_output <- expected_output[order(expected_output$person_id), ]

  expect_equal(result$crime_safety, expected_output$crime_safety)
})
