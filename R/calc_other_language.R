#' Calculate Whether Participant Speaks a Language Other Than English at Home
#'
#' This function creates a nominal categorical variable with values 'Yes', 'No', or 'PMI: Prefer Not To Answer',
#' indicating whether the participant speaks a language other than English at home.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer`.
#'
#' @return A data frame with two columns: `person_id` and `other_language`, where `other_language` contains the values
#' 'Yes', 'No', or 'PMI: Prefer Not To Answer'. Participants without data or who skipped the question will have NA values.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = c(1, 2, 3, 4, 5),
#'   question_concept_id = rep(40192526, 5),
#'   answer = c("Yes", "No", "Yes", "Prefer Not To Answer", "Skip")
#' )
#'
#' # Compute whether participants speak a language other than English at home
#' other_language_scores <- calc_other_language(survey_df)
#' head(other_language_scores)
#'
#' @export
calc_other_language <- function(survey_df) {
  if (!is.null(survey_df)){
    df_other_language  <-  survey_df |>
      dplyr::filter(question_concept_id == 40192526) |> # 1 specific item
      # 40192526 = Do you speak a language other than English at home?
      dplyr::select(person_id, question_concept_id, answer) |> # map answer_concept_id to value
      dplyr::filter(answer != "Skip") |> # remove skips
      dplyr::mutate(other_language = answer) |>
      dplyr::select(person_id, other_language) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id')
  }
  df_other_language
}
