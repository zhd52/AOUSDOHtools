#' Calculate Loneliness Score
#'
#' This function computes a loneliness score based on responses to 8 specific items.
#' The score ranges from 8 to 32, with higher scores indicating a greater degree of loneliness.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `loneliness`, where `loneliness`
#' is the calculated loneliness score for each participant. The score is the sum of the individual item scores,
#' with higher values indicating a higher degree of loneliness. Participants who did not answer all 8 questions will have NA values.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = rep(1:3, each = 8),
#'   question_concept_id = rep(c(40192390, 40192397, 40192398, 40192494,
#'                               40192501, 40192504, 40192507, 40192516), times = 3),
#'   answer_concept_id = sample(c(40192465, 40192481, 40192429, 40192482),
#'                              24, replace = TRUE)
#' )
#'
#' # Compute loneliness scores
#' loneliness_scores <- calc_loneliness(survey_df)
#' head(loneliness_scores)
#'
#' @export
calc_loneliness <- function(survey_df) {
  if (!is.null(survey_df)){
    df_loneliness  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192390, 40192397, 40192398, 40192494, 40192501,
                                               40192504, 40192507, 40192516)) |> # 8 specific items
      # 40192390 = How often do you feel that you are unhappy being so withdrawn?
      # 40192397 = How often do you feel that there is no one you can turn to?
      # 40192398 = How often do you feel left out?
      # 40192494 = How often do you feel that people are around you but not with you?
      # 40192501 = How often do you feel isolated from others?
      # 40192504 = How often do you feel that you are an outgoing person?
      # 40192507 = How often do you feel lack companionship?
      # 40192516 = How often do you fell that you can find companionship when you want it?
      dplyr::select(person_id, question_concept_id, answer_concept_id) |> # map answer_concept_id to value
      dplyr::mutate(value = dplyr::case_when(
        answer_concept_id == 40192465 ~ 1, # Never
        answer_concept_id == 40192481 ~ 2, # Rarely
        answer_concept_id == 40192429 ~ 3, # Sometimes
        answer_concept_id == 40192482 ~ 4, # Often
        TRUE ~ 999)) |>
      dplyr::filter(value != 999) |> # remove skips
      dplyr::mutate(value = dplyr::case_when(
        # reverse code for 4 questions
        question_concept_id == 40192504 ~ 5 - value, # 4 -> 1, 3 -> 2 etc
        question_concept_id == 40192516 ~ 5 - value,
        TRUE ~ value)) |>
      dplyr::group_by(person_id) |> # group by person_id and calculate sum score
      dplyr::mutate(loneliness = sum(value,
                                     na.rm = TRUE),
                    nrows = length(value)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 8) |> # include only participants who answered all 8 questions
      dplyr::select(person_id, loneliness) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_loneliness
}
