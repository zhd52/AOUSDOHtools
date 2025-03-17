test_that("calc_stress_category works with valid input", {
  # Valid input with different stress levels
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                  2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                  3, 3, 3, 3, 3, 3, 3, 3, 3, 3),
    question_concept_id = rep(c(40192381, 40192396, 40192419, 40192445, 40192449,
                                40192452, 40192462, 40192491, 40192506, 40192525), 3),
    answer_concept_id = c(rep(40192465, 10),  # All "Never" (0)
                          rep(40192429, 10),  # All "Sometimes" (2)
                          rep(40192424, 10))  # All "Very Often" (4)
  )

  # Expected output: correctly categorized stress levels
  expected_output <- data.frame(
    person_id = c(1, 2, 3),
    stress_category = c("Moderate", "Moderate", "Moderate")
  )

  result <- calc_stress_category(survey_df)
  expect_equal(result$stress_category, expected_output$stress_category)
})



test_that("calc_stress_category handles incomplete responses", {
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                  2, 2, 2, 2, 2, 2, 2),
    question_concept_id = c(rep(c(40192381, 40192396, 40192419, 40192445, 40192449,
                                  40192452, 40192462, 40192491, 40192506, 40192525), 1),
                            c(40192381, 40192396, 40192419, 40192445, 40192449, 40192452, 40192462)),
    answer_concept_id = c(rep(40192424, 10), rep(40192424, 7))
  )

  expected_output <- data.frame(
    person_id = c(1, 2),
    stress_category = c("Moderate", NA_character_)
  )

  result <- calc_stress_category(survey_df)
  expect_equal(result$stress_category, expected_output$stress_category)
})



test_that("calc_stress_category handles skipped responses", {
  survey_df <- data.frame(
    person_id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                  2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
    question_concept_id = rep(c(40192381, 40192396, 40192419, 40192445, 40192449,
                                40192452, 40192462, 40192491, 40192506, 40192525), 2),
    answer_concept_id = c(rep(40192429, 10),
                          40192429, 903096, 40192429, 40192429, 903096, 40192429, 40192429, 40192429, 40192429, 40192429)
  )

  expected_output <- data.frame(
    person_id = c(1, 2),
    stress_category = c("Moderate", NA_character_)
  )

  result <- calc_stress_category(survey_df)
  expect_equal(result$stress_category, expected_output$stress_category)
})



test_that("calc_stress_category handles missing responses", {
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    question_concept_id = c(40192381, 40192396, 40192419, 40192445, 40192449,
                            40192452, 40192462, 40192491, 40192506, 40192525),
    answer_concept_id = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  )

  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    stress_category = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_)
  )

  result <- calc_stress_category(survey_df)
  expect_equal(result$stress_category, expected_output$stress_category)
})



test_that("calc_stress_category handles empty input", {
  survey_df <- data.frame(
    person_id = integer(0),
    question_concept_id = integer(0),
    answer_concept_id = integer(0)
  )

  result <- calc_stress_category(survey_df)

  expect_equal(nrow(result), 0)
})



test_that("calc_stress_category handles invalid column names", {
  bad_survey_df <- data.frame(
    wrong_person_id = c(1, 2, 3),
    wrong_question_id = c(40192381, 40192396, 40192419),
    wrong_answer_id = c(40192465, 40192430, 40192429)
  )

  expect_error(calc_stress_category(bad_survey_df), "object 'question_concept_id' not found")
})



test_that("calc_stress_category returns NA for participants without valid answers", {
  survey_df <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    question_concept_id = c(40192381, 40192396, 40192419, 40192445, 40192449,
                            40192452, 40192462, 40192491, 40192506, 40192525),
    answer_concept_id = c(99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999)
  )

  expected_output <- data.frame(
    person_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    stress_category = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_)
  )

  result <- calc_stress_category(survey_df)
  expect_equal(result$stress_category, expected_output$stress_category)
})
