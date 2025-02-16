#' Calculate Neighborhood Cohesion Score
#'
#' This function computes a neighborhood cohesion score ranging from 1 to 5 based on
#' survey responses. The score is the mean of four specific item scores, where higher
#' scores indicate greater neighborhood cohesion.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `cohesion`, where `cohesion`
#' is the calculated cohesion score for each participant. Participants who did not answer
#' all four questions will have an NA score.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = c(1, 1, 2, 2, 3, 3, 4, 4),
#'   question_concept_id = c(40192463, 40192411, 40192463, 40192411,
#'                           40192499, 40192417, 40192499, 40192417),
#'   answer_concept_id = c(40192514, 40192455, 40192524, 40192408,
#'                         40192514, 40192524, 40192408, 40192422)
#' )
#'
#' # Compute neighborhood cohesion scores
#' cohesion_scores <- calc_cohesion(survey_df)
#' head(cohesion_scores)
#'
#' @export
calc_cohesion <- function(survey_df) {
  if (!is.null(survey_df)){
    df_cohesion  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192463, 40192411, 40192499, 40192417)) |> # 4 specific items
      # 40192463 = How much you agree or disagree that people around here are willing to help their neighbor?
      # 40192411 = How much you agree or disagree that people in your neighborhood generally get along with each other?
      # 40192499 = How much you agree or disagree that people in your neighborhood can be trusted?
      # 40192417 = How much you agree or disagree that people in your neighborhood share the same values?
      dplyr::select(person_id, question_concept_id, answer_concept_id) |> # map answer_concept_id to value
      dplyr::mutate(value = dplyr::case_when(
        answer_concept_id == 40192514 ~ 5, # Strongly agree
        answer_concept_id == 40192455 ~ 4, # Agree
        answer_concept_id == 40192524 ~ 3, # Neutral (neither agree nor disagree)
        answer_concept_id == 40192408 ~ 2, # Disagree
        answer_concept_id == 40192422 ~ 1, # Strongly disagree
        TRUE ~ 999)) |>
      dplyr::filter(value != 999) |> # remove skips
      dplyr::group_by(person_id) |> # group by person_id and calculate mean score
      dplyr::mutate(cohesion = round(mean(value, na.rm = TRUE), 2), # rounded to 2 decimals
                    nrows = length(value)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 4) |> # include only participants who answered all 4 questions
      dplyr::select(person_id, cohesion) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_cohesion
}

