#' Calculate Perceived Stress Category
#'
#' This function creates an ordinal categorical variable indicating perceived stress levels as 'Low', 'Moderate', or 'High' based on participants' total perceived stress score.
#' The perceived stress score is calculated as the sum of individual item scores, with 'Low' for scores 0-13, 'Moderate' for scores 14-26, and 'High' for scores 27-40.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `stress_category`, where `stress_category` represents the stress level ('Low', 'Moderate', or 'High') for each participant.
#' Participants who did not answer all 10 questions will have NA values.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = rep(1:3, each = 10),
#'   question_concept_id = rep(c(40192381, 40192396, 40192419, 40192445,
#'                               40192449, 40192452, 40192462, 40192491,
#'                               40192506, 40192525), times = 3),
#'   answer_concept_id = sample(c(40192465, 40192430, 40192429, 40192477,
#'                                 40192424), 30, replace = TRUE)
#' )
#'
#' # Compute perceived stress categories
#' stress_category_scores <- calc_stress_category(survey_df)
#' head(stress_category_scores)
#'
#' @export
calc_stress_category <- function(survey_df) {
  if (!is.null(survey_df)){
    df_stress_category  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192381, 40192396, 40192419, 40192445, 40192449, 40192452, 40192462,
                                               40192491, 40192506, 40192525)) |> # 10 specific items
      # 40192381 = In the last month, how often have you felt that you were unable to control the important things
      #            in your life?
      # 40192396 = In the last month, how often have you been angered because of things that were outside of your control?
      # 40192419 = In the last month, how often have you felt confident about your ability to handle your personal problems?
      # 40192445 = In the last month, how often have you felt that you were on top of things?
      # 40192449 = In the last month, how often have you been able to control irritations in your life?
      # 40192452 = In the last month, how often have you been upset because of something that happened unexpectedly?
      # 40192462 = In the last month, how often have you felt difficulties were piling up so high that you could not
      #            overcome them?
      # 40192491 = In the last month, how often have you felt nervous and "stressed"?
      # 40192506 = In the last month, how often have you found that you could not cope with all the things that you had
      #            to do?
      # 40192525 = In the last month, how often have you felt that things were going your way?
      dplyr::select(person_id, question_concept_id, answer_concept_id) |> # map answer_concept_id to value
      dplyr::mutate(value = dplyr::case_when(
        answer_concept_id == 40192465 ~ 0, # Never
        answer_concept_id == 40192430 ~ 1, # Almost Never
        answer_concept_id == 40192429 ~ 2, # Sometimes
        answer_concept_id == 40192477 ~ 3, # Fairly Often
        answer_concept_id == 40192424 ~ 4, # Very Often
        TRUE ~ 999)) |>
      dplyr::filter(value != 999) |> # remove skips
      dplyr::mutate(value = dplyr::case_when(
        # reverse score for 4 questions
        question_concept_id %in% c(40192419, 40192445, 40192449, 40192525) ~ 4-value, # 4 -> 0, 3 -> 1 etc
        TRUE ~ value)) |>
      dplyr::group_by(person_id) |> # group by person_id and calculate sum score
      dplyr::mutate(stress_sum = sum(value,
                                     na.rm = TRUE),
                    nrows = length(value)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 10) |> # include only participants who answered all 10 questions
      dplyr::mutate(stress_category = dplyr::case_when(
        stress_sum < 14 ~ "Low", # 0-13 = low
        stress_sum < 27 ~ "Moderate", # 14-26 = moderate
        TRUE ~ "High")) |> # 27-40 = high
      dplyr::select(person_id, stress_category) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_stress_category
}
