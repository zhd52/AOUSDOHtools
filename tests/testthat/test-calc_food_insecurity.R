test_that("calc_food_insecurity works with valid input", {
  # Valid input with different food insecurity responses
  survey_df <- data.frame(
    person_id = c(1, 1, 2, 2, 3, 3, 4, 4),
    question_concept_id = c(40192426, 40192517, 40192426, 40192517,
                            40192426, 40192517, 40192426, 40192517),
    answer_concept_id = c(40192508, 40192508, 40192488, 40192488, # Often true, Sometimes true (food insecurity)
                          40192474, 40192474, 40192474, 40192474) # Never true (not food insecure)
  )

  # Expected output: correctly classified as food insecure (TRUE) or not (FALSE)
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    food_insecurity = c(TRUE, TRUE, FALSE, FALSE)
  )

  # Call the function and compare with expected output
  result <- calc_food_insecurity(survey_df)
  expect_equal(result$food_insecurity, expected_output$food_insecurity)
})



test_that("calc_food_insecurity handles partial responses", {
  # Incomplete responses: person 2 has only one response
  survey_df <- data.frame(
    person_id = c(1, 1, 2, 3, 3, 4, 4),
    question_concept_id = c(40192426, 40192517, 40192426,
                            40192426, 40192517, 40192426, 40192517),
    answer_concept_id = c(40192508, 40192474, 40192488,
                          40192474, 40192474, 40192508, 40192508)
  )

  # Expected output: person 2 has NA (incomplete), others classified correctly
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    food_insecurity = c(TRUE, TRUE, FALSE, TRUE)
  )

  result <- calc_food_insecurity(survey_df)
  expect_equal(result$food_insecurity, expected_output$food_insecurity)
})



test_that("calc_food_insecurity handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 1, 2, 2, 3, 3),
    question_concept_id = c(40192426, 40192517, 40192426, 40192517, 40192426, 40192517),
    answer_concept_id = c(40192508, 903096, 40192488, 903096, 903096, 903096) # Skipped responses
  )

  # Expected output: skipped responses result in NA unless one answer is positive
  expected_output <- data.frame(
    person_id = c(1, 2, 3),
    food_insecurity = c(TRUE, TRUE, NA)
  )

  result <- calc_food_insecurity(survey_df)
  expect_equal(result$food_insecurity, expected_output$food_insecurity)
})



test_that("calc_food_insecurity handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192426, 40192517, 40192426, 40192517),
    answer_concept_id = c(40192508, 40192508, NA, NA) # NA responses
  )

  # Expected output: missing responses should remain NA, others classified correctly
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    food_insecurity = c(TRUE, TRUE, NA, NA)
  )

  result <- calc_food_insecurity(survey_df)
  expect_equal(result$food_insecurity, expected_output$food_insecurity)
})



test_that("calc_food_insecurity handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_food_insecurity(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_food_insecurity handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192426, 40192517, 40192426),
    wrong_answer_id = c(40192508, 40192488, 40192474)
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_food_insecurity(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_food_insecurity returns NA for participants without valid answers", {
  # No valid answers for any participants
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192426, 40192517, 40192426, 40192517),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999) # All invalid responses
  )

  result <- calc_food_insecurity(survey_df)

  # Expected output: all participants should have NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    food_insecurity = c(NA, NA, NA, NA)
  )

  expect_equal(result$food_insecurity, expected_output$food_insecurity)
})
