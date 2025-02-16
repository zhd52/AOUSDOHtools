#' Calculate Instrumental Support Score
#'
#' This function computes a numeric instrumental support score ranging from 0 to 100.
#' The score is the mean of individual item scores transformed to a 0-100 scale, where higher scores indicate greater tangible support.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `ins_support`, where `ins_support`
#' is the calculated instrumental support score for each participant. The score is scaled from 0 to 100, with higher values indicating more tangible support.
#' Participants who did not answer all four relevant questions will have NA values.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = rep(1:3, each = 4),
#'   question_concept_id = rep(c(40192388, 40192442, 40192480, 40192511), times = 3),
#'   answer_concept_id = sample(c(40192454, 40192518, 40192486, 40192382, 40192521),
#'                              12, replace = TRUE)
#' )
#'
#' # Compute instrumental support scores
#' ins_support_scores <- calc_ins_support(survey_df)
#' head(ins_support_scores)
#'
#' @export
calc_ins_support <- function(survey_df) {
  if (!is.null(survey_df)){
    df_ins_support  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192388, 40192442, 40192480, 40192511)) |> # 4 specific items
      # 40192388 = How often do you have someone to prepare your meals if you were unable to do it yourself?
      # 40192442 = How often do you have someone to help you if you were confined to bed?
      # 40192480 = How often do you have someone to take you to the doctor if you need it?
      # 40192511 = How often do you have someone to help you with daily chores if you were sick?
      dplyr::select(person_id, question_concept_id, answer_concept_id) |> # map answer_concept_id to value
      dplyr::mutate(value = dplyr::case_when(
        answer_concept_id == 40192454 ~ 1, # None of the time
        answer_concept_id == 40192518 ~ 2, # A little of the time
        answer_concept_id == 40192486 ~ 3, # Some of the time
        answer_concept_id == 40192382 ~ 4, # Most of the time
        answer_concept_id == 40192521 ~ 5, # All of the time
        TRUE ~ 999)) |>
      dplyr::filter(value != 999) |> # remove skips
      dplyr::group_by(person_id) |> # group by person_id and calculate mean score
      dplyr::mutate(ins_support = round(100*(sum(value, na.rm = TRUE)-4)/(20-4),
                                        2), # rounded to 2 decimals
                    nrows = length(value)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 4) |> # include only participants who answered all 4 questions
      dplyr::select(person_id, ins_support) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_ins_support
}
