#' Calculate English Proficiency Category
#'
#' This function creates a nominal categorical variable with values 'Proficient', 'Not proficient', or 'Unknown'
#' for participants who endorsed speaking a language other than English at home.
#' Proficient' refers to participants who reported speaking English 'Very well' or 'Well',
#' while 'Not proficient' refers to participants who reported speaking English 'Not well' or 'Not at all'.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `english_proficient`, where `english_proficient`
#' categorizes participants as 'Proficient', 'Not proficient', or 'Unknown'. Participants who did not respond
#' will have an NA value for `english_proficient`.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = c(1, 2, 3, 4, 5, 6, 7),
#'   question_concept_id = rep(40192529, 7),
#'   answer_concept_id = c(40192435, 40192510, 40192405, 40192387,
#'                         903087, 903079, NA)
#' )
#'
#' # Compute English proficiency categories
#' english_proficient_scores <- calc_english_proficient(survey_df)
#' head(english_proficient_scores)
#'
#' @export
calc_english_proficient <- function(survey_df) {
  if (!is.null(survey_df)){
    df_english_proficient  <-  survey_df |>
      dplyr::filter(question_concept_id == 40192529) |> # 1 specific item
      # 40192529 = Since you speak a language other than English at home, we are interested in your own thoughts about
      #            how well you think you speak English. Would you say you speak English...
      dplyr::select(person_id, question_concept_id, answer_concept_id) |> # map answer_concept_id to value
      dplyr::mutate(english_proficient = dplyr::case_when(
        answer_concept_id == 40192435 ~ "Proficient", # Very well
        answer_concept_id == 40192510 ~ "Proficient", # Well
        answer_concept_id == 40192405 ~ "Not proficient", # Not well
        answer_concept_id == 40192387 ~ "Not proficient", # Not at all
        answer_concept_id == 903087 ~ "Unknown", # PMI: Dont Know
        answer_concept_id == 903079 ~ "Unknown", # PMI: Prefer Not To Answer
        TRUE ~ "none")) |>
      dplyr::filter(english_proficient != "none") |> # remove skips
      dplyr::select(person_id, english_proficient) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_english_proficient
}
