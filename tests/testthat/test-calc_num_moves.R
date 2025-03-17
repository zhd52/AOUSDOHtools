test_that("calc_num_moves works with valid input", {
  # Valid input with different number of moves
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5),
    question_concept_id = c(40192441, 40192441, 40192441, 40192441, 40192441),
    answer = c("0", "1", "2", "3", "4")  # Different move counts
  )

  # Expected output: correctly parsed numeric move counts
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5),
    num_moves = c(0, 1, 2, 3, 4)
  )

  # Call the function and compare with expected output
  result <- calc_num_moves(survey_df)
  expect_equal(result$num_moves, expected_output$num_moves)
})



test_that("calc_num_moves handles skipped responses", {
  # Some participants have skipped responses
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192441, 40192441, 40192441, 40192441),
    answer = c("0", "2", "Skip", "Skip")
  )

  # Expected output: skipped responses should result in NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    num_moves = c(0, 2, NA_real_, NA_real_)
  )

  result <- calc_num_moves(survey_df)
  expect_equal(result$num_moves, expected_output$num_moves)
})



test_that("calc_num_moves handles missing responses", {
  # Some participants did not respond (NA values)
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4),
    question_concept_id = c(40192441, 40192441, 40192441, 40192441),
    answer = c("0", "3", NA, NA)
  )

  # Expected output: missing responses should remain NA
  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4),
    num_moves = c(0, 3, NA_real_, NA_real_)
  )

  result <- calc_num_moves(survey_df)
  expect_equal(result$num_moves, expected_output$num_moves)
})



test_that("calc_num_moves handles empty input", {
  # Empty input case
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer = character(0)
  )

  result <- calc_num_moves(survey_df)

  # Expect the result to be an empty data frame
  expect_equal(nrow(result), 0)
})



test_that("calc_num_moves handles invalid column names", {
  # Input with incorrect column names
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192441, 40192441, 40192441),
    wrong_answer = c("0", "2", "3")
  )

  # Expect an error when the input does not have the correct column names
  expect_error(calc_num_moves(bad_survey_df), "object 'question_concept_id' not found")
})
