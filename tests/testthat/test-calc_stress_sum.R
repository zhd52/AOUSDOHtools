test_that("calc_stress_sum works with valid input", {
  # Valid input with different stress levels
  survey_df <- data.frame(
    person_id = rep(1:3, each = 10),  # 3 participants, each with 10 responses
    question_concept_id = rep(c(40192381, 40192396, 40192419, 40192445, 40192449,
                                40192452, 40192462, 40192491, 40192506, 40192525), 3),
    answer_concept_id = c(rep(40192465, 10),  # All "Never" (0)
                          rep(40192429, 10),  # All "Sometimes" (2)
                          rep(40192424, 10))  # All "Very Often" (4)
  )

  expected_output <- data.frame(
    person_id = c(1, 2, 3),
    stress_sum = c(16, 20, 24)  # Sum of responses per participant
  )

  result <- calc_stress_sum(survey_df)
  expect_equal(result$stress_sum, expected_output$stress_sum)
})



test_that("calc_stress_sum handles incomplete responses", {
  survey_df <- data.frame(
    person_id = c(rep(1, 10), rep(2, 7)),
    question_concept_id = c(rep(c(40192381, 40192396, 40192419, 40192445, 40192449,
                                  40192452, 40192462, 40192491, 40192506, 40192525), 1),
                            c(40192381, 40192396, 40192419, 40192445, 40192449, 40192452, 40192462)),
    answer_concept_id = c(rep(40192424, 10), rep(40192424, 7))
  )

  expected_output <- data.frame(
    person_id = c(1, 2),
    stress_sum = c(24, NA_real_)  # Second person has incomplete responses
  )

  result <- calc_stress_sum(survey_df)
  expect_equal(result$stress_sum, expected_output$stress_sum)
})



test_that("calc_stress_sum handles skipped responses", {
  survey_df <- data.frame(
    person_id = rep(1:2, each = 10),
    question_concept_id = rep(c(40192381, 40192396, 40192419, 40192445, 40192449,
                                40192452, 40192462, 40192491, 40192506, 40192525), 2),
    answer_concept_id = c(rep(40192429, 10),
                          c(40192429, 903096, 40192429, 40192429, 903096, 40192429, 40192429, 40192429, 40192429, 40192429))
  )

  expected_output <- data.frame(
    person_id = c(1, 2),
    stress_sum = c(20, NA_real_)  # Skipped responses lead to NA
  )

  result <- calc_stress_sum(survey_df)
  expect_equal(result$stress_sum, expected_output$stress_sum)
})



test_that("calc_stress_sum handles missing responses", {
  survey_df <- data.frame(
    person_id = 1:10,
    question_concept_id = rep(40192381, 10),
    answer_concept_id = rep(NA, 10)  # All missing responses
  )

  expected_output <- data.frame(
    person_id = 1:10,
    stress_sum = rep(NA_real_, 10)
  )

  result <- calc_stress_sum(survey_df)
  expect_equal(result$stress_sum, expected_output$stress_sum)
})



test_that("calc_stress_sum handles empty input", {
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_stress_sum(survey_df)

  expect_equal(nrow(result), 0)
})



test_that("calc_stress_sum handles invalid column names", {
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192381, 40192396, 40192419),
    wrong_answer_id = c(40192465, 40192430, 40192429)
  )

  expect_error(calc_stress_sum(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_stress_sum returns NA for participants without valid answers", {
  survey_df <- data.frame(
    person_id = 1:10,
    question_concept_id = rep(40192381, 10),
    answer_concept_id = rep(99999999, 10)  # Invalid responses
  )

  expected_output <- data.frame(
    person_id = 1:10,
    stress_sum = rep(NA_real_, 10)  # All invalid responses should result in NA
  )

  result <- calc_stress_sum(survey_df)
  expect_equal(result$stress_sum, expected_output$stress_sum)
})
