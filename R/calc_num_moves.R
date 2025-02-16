#' Calculate Number of Moves in the Past Year
#'
#' This function creates a numeric variable representing the number of times a participant has moved in the past 12 months.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer`.
#'
#' @return A data frame with two columns: `person_id` and `num_moves`, where `num_moves`
#' represents the number of moves in the past year for each participant. Participants without data or who skipped the question will have NA values.
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
#' # Compute number of moves in the past year
#' num_moves_scores <- calc_num_moves(survey_df)
#' head(num_moves_scores)
#'
#' @export
calc_num_moves <- function(survey_df) {
  if (!is.null(survey_df)){
    df_num_moves  <-  survey_df |>
      dplyr::filter(question_concept_id == 40192441) |> # 1 specific item
      # 40192441 = In the last 12 months, how many times have you or your family moved from one home to another?
      #            Number of moves in past 12 months:
      dplyr::select(person_id, question_concept_id, answer) |> # map answer_concept_id to value
      dplyr::filter(answer != "Skip") |> # remove skips
      dplyr::mutate(num_moves = as.numeric(answer)) |>
      dplyr::select(person_id, num_moves) |> # only 2 columns in the final result
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id')
  }
  df_num_moves
}
