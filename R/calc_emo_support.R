#' Calculate Emotional Support Score
#'
#' This function computes an emotional support score on a 0-100 scale based on
#' survey responses. The score is the mean of four specific item scores, with higher
#' scores indicating more emotional support.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `emo_support`, where `emo_support`
#' is the calculated emotional support score for each participant. Participants who did not answer
#' all four questions will have an NA score.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = rep(1:3, each = 4),
#'   question_concept_id = rep(c(40192399, 40192439, 40192446, 40192528), times = 3),
#'   answer_concept_id = sample(c(40192454, 40192518, 40192486, 40192382, 40192521),
#'                              12, replace = TRUE)
#' )
#'
#' # Compute emotional support scores
#' emo_support_scores <- calc_emo_support(survey_df)
#' head(emo_support_scores)
#'
#' @export
calc_emo_support <- function(survey_df) {
  if (!is.null(survey_df)){
    df_emo_support  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192399, 40192439, 40192446, 40192528)) |> # 4 specific items
      # 40192399 = How often do you have someone who understands your problems?
      # 40192439 = How often do you have someone to have a good time with?
      # 40192446 = How often do you have someone to love and make you feel wanted?
      # 40192528 = How often do you have someone to turn to for suggestions about how to deal with a personal problem?
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
      dplyr::mutate(emo_support = round(100*(sum(value, na.rm = TRUE)-4)/(20-4),
                                        2), # rounded to 2 decimals
                    nrows = length(value)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 4) |> # include only participants who answered all 4 questions
      dplyr::select(person_id, emo_support) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_emo_support
}
