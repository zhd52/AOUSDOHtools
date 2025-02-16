#' Calculate Housing Quality Needs
#'
#' This function creates a binary categorical variable indicating whether a participant endorses having any housing-related problems (housing need).
#' A value of TRUE indicates the participant selected at least one housing problem, while FALSE indicates no problems were reported.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `housing_quality`, where `housing_quality`
#' is a TRUE or FALSE indicator for each participant. TRUE indicates the participant selected at least one housing problem, and FALSE indicates no housing problems.
#' Participants without data will have NA values.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = c(1, 2, 3, 4, 5, 6, 7),
#'   question_concept_id = rep(40192402, 7),
#'   answer_concept_id = c(40192392, 40192479, 40192444, 40192460,
#'                         40192434, 40192468, 40192393)
#' )
#'
#' # Compute housing quality needs
#' housing_quality_scores <- calc_housing_quality(survey_df)
#' head(housing_quality_scores)
#'
#' @export
calc_housing_quality <- function(survey_df) {
  if (!is.null(survey_df)){
    df_housing_quality  <-  survey_df |>
      dplyr::filter(question_concept_id == 40192402) |> # 1 specific item
      # 40192402 = Think about the place you live. Do you have problems with any of the following? Select all that apply.
      dplyr::select(person_id, question_concept_id, answer_concept_id) |> # map answer_concept_id to value
      dplyr::mutate(value = dplyr::case_when(
        answer_concept_id == 40192392 ~ 0, # None of the above
        answer_concept_id %in% c(40192479, 40192444, 40192460, 40192434, 40192468, 40192393, 40192495) ~ 1, # Mold, water leaks, ...
        TRUE ~ 999)) |>
      dplyr::filter(value != 999) |> # remove skips
      dplyr::group_by(person_id) |> # group by person_id and calculate score
      dplyr::mutate(housing_quality = 1 %in% value) |> # did the participant note any problems
      dplyr::select(person_id, housing_quality) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_housing_quality
}
