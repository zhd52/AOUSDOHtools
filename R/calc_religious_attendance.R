#' Calculate Religious Attendance Frequency
#'
#' This function creates an ordinal categorical variable indicating the frequency of attending religious meetings or services, based on participant responses.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer`.
#'
#' @return A data frame with two columns: `person_id` and `religious_attendance`, where `religious_attendance`
#' indicates how often the participant attends religious meetings or services. Participants without data or who skipped the question will have NA values.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = c(1, 2, 3, 4, 5),
#'   question_concept_id = rep(40192470, 5),
#'   answer = c("Never", "Once a week", "More than once a week", "Never", "Skip")
#' )
#'
#' # Compute religious attendance frequency
#' religious_attendance_scores <- calc_religious_attendance(survey_df)
#' head(religious_attendance_scores)
#'
#' @export
calc_religious_attendance <- function(survey_df) {
  if (!is.null(survey_df)){
    df_religious_attendance  <-  survey_df |>
      dplyr::filter(question_concept_id == 40192470) |> # 1 specific item
      # 40192470 = How often do you go to religious meetings or services?
      dplyr::select(person_id, question_concept_id, answer) |> # map answer_concept_id to value
      dplyr::filter(answer != "Skip") |> # remove skips
      dplyr::mutate(religious_attendance = answer) |>
      dplyr::select(person_id, religious_attendance) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id')
  }
  df_religious_attendance
}
