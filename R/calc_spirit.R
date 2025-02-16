#' Calculate Daily Religious or Spiritual Experiences (Spirit) Score
#'
#' This function computes a numeric score representing the frequency of daily religious or spiritual experiences.
#' The score ranges from 6 to 36, with higher scores indicating more frequent daily religious or spiritual experiences.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `spirit`, where `spirit` is the sum score for daily religious or spiritual experiences.
#' The score is based on responses to six items, with higher values indicating more frequent experiences.
#' Participants who did not answer all six questions will have NA values.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = rep(1:3, each = 6),
#'   question_concept_id = rep(c(40192401, 40192415, 40192443, 40192471,
#'                               40192475, 40192498), times = 3),
#'   answer_concept_id = sample(c(40192487, 40192432, 40192509, 40192459,
#'                                 40192513, 40192484, 40192385, 40192403),
#'                              18, replace = TRUE)
#' )
#'
#' # Compute daily religious or spiritual experiences (Spirit) scores
#' spirit_scores <- calc_spirit(survey_df)
#' head(spirit_scores)
#'
#' @export
calc_spirit <- function(survey_df) {
  if (!is.null(survey_df)){
    df_spirit  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192401, 40192415, 40192443, 40192471,
                                               40192475, 40192498)) |> # 6 specific items
      # 40192401 = How often do you feel deep inner peace or harmony?
      # 40192415 = How often do you feel that you are spiritually touched by the beauty of creation?
      # 40192443 = How often do you desire to be closer to or in union with God (or a higher power)?
      # 40192471 = How often do you feel God's (or a higher power's) love for you, directly or through others?
      # 40192475 = How often do you find strength and comfort in your religion?
      # 40192498 = How often do you feel God's (or a higher power's) presence?
      dplyr::select(person_id, question_concept_id, answer_concept_id) |> # map answer_concept_id to value
      dplyr::mutate(value = dplyr::case_when(
        answer_concept_id == 40192487 ~ 1, # I do not believe in God (or a higher power)
        answer_concept_id == 40192432 ~ 1, # I am not religious
        answer_concept_id == 40192509 ~ 1, # Never or almost never
        answer_concept_id == 40192459 ~ 2, # Once in a while
        answer_concept_id == 40192513 ~ 3, # Some days
        answer_concept_id == 40192484 ~ 4, # Most days
        answer_concept_id == 40192385 ~ 5, # Every day
        answer_concept_id == 40192403 ~ 6, # Many times a day
        TRUE ~ 999)) |>
      dplyr::filter(value != 999) |> # remove skips
      dplyr::group_by(person_id) |> # group by person_id and calculate sum score
      dplyr::mutate(spirit = sum(value,
                                 na.rm = TRUE),
                    nrows = length(value)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 6) |> # include only participants who answered all 6 questions
      dplyr::select(person_id, spirit) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_spirit
}
