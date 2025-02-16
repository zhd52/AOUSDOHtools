#' Calculate Housing Insecurity
#'
#' This function creates a binary categorical variable indicating whether a participant is at risk of, or is currently experiencing, housing insecurity.
#' Housing insecurity is defined as having moved two or more times in the past 12 months.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer`.
#'
#' @return A data frame with two columns: `person_id` and `housing_insecurity`, where `housing_insecurity`
#' is a TRUE or FALSE indicator for each participant. TRUE indicates housing insecurity (two or more moves in the past year), and FALSE otherwise.
#' Participants without data will have NA values.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = c(1, 2, 3, 4, 5),
#'   question_concept_id = rep(40192441, 5),
#'   answer = c("0", "1", "2", "3", "Skip")
#' )
#'
#' # Compute housing insecurity status
#' housing_insecurity_scores <- calc_housing_insecurity(survey_df)
#' head(housing_insecurity_scores)
#'
#' @export
calc_housing_insecurity <- function(survey_df) {
  if (!is.null(survey_df)){
    df_housing_insecurity  <-  survey_df |>
      dplyr::filter(question_concept_id == 40192441) |> # 1 specific item
      # 40192441 = In the last 12 months, how many times have you or your family moved from one home to another?
      #            Number of moves in past 12 months:
      dplyr::select(person_id, question_concept_id, answer) |> # map answer_concept_id to value
      dplyr::filter(answer != "Skip") |> # remove skips
      dplyr::mutate(housing_insecurity = dplyr::case_when(
        answer %in% c("0","1") ~ FALSE, # 0 or 1 moves = false for household insecurity
        TRUE ~ TRUE)) |> # any response greater than 1 = true for household insecurity
      dplyr::select(person_id, housing_insecurity) |> # only 2 columns in the final result
      dplyr::right_join(survey_df |> # include participants without data as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id')
  }
  df_housing_insecurity
}
