#' Calculate English Proficiency Level
#'
#' This function creates an ordinal categorical variable that describes the level of proficiency
#' in English for participants who reported speaking a language other than English at home.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer`.
#'
#' @return A data frame with two columns: `person_id` and `english_level`, where `english_level`
#' represents the participant's self-reported proficiency in English. Participants who did not respond
#' or provided a "PMI: Skip" will have an NA value.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = c(1, 2, 3, 4, 5),
#'   question_concept_id = c(40192529, 40192529, 40192529, 40192529, 40192529),
#'   answer = c("Very well", "Well", "Not well", "Not at all", "Skip")
#' )
#'
#' # Compute English proficiency levels
#' english_level_scores <- calc_english_level(survey_df)
#' head(english_level_scores)
#'
#' @export
calc_english_level <- function(survey_df) {
  if (!is.null(survey_df)){
    df_english_level  <-  survey_df |>
      dplyr::filter(question_concept_id == 40192529) |> # 1 specific item
      # 40192529 = Since you speak a language other than English at home, we are interested in your own thoughts about
      #            how well you think you speak English. Would you say you speak English...
      dplyr::select(person_id, question_concept_id, answer) |> # map answer_concept_id to value
      dplyr::filter(answer != "Skip") |> # remove skips
      dplyr::mutate(english_level = answer) |>
      dplyr::select(person_id, english_level) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id')
  }
  df_english_level
}
