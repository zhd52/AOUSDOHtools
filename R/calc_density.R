#' Calculate Residential Density
#'
#' This function creates a binary categorical variable representing residential density
#' based on survey responses. 'Low' denotes low residential density (detached single-family housing),
#' while 'High' denotes high residential density.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `density`, where `density`
#' is either "High" or "Low" based on the housing type in the neighborhood. Participants with
#' non-answers will have an NA value for `density`.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = c(1, 2, 3, 4, 5),
#'   question_concept_id = c(40192458, 40192458, 40192458, 40192458, 40192458),
#'   answer_concept_id = c(40192407, 40192472, 40192418, 40192433, 40192409)
#' )
#'
#' # Compute residential density categories
#' density_scores <- calc_density(survey_df)
#' head(density_scores)
#'
#' @export
calc_density <- function(survey_df) {
  if (!is.null(survey_df)) {
    df_density <- survey_df |>
      dplyr::filter(question_concept_id == 40192458) |> # 1 specific item
      # 40192458 = What is the main type of housing in your neighborhood?
      dplyr::mutate(density = dplyr::case_when(
        answer_concept_id == 40192407 ~ "Low", # Detached single-family housing
        answer_concept_id %in% c(40192472, 40192418, 40192433, 40192409) ~ "High", # Townhouses, ...
        TRUE ~ "none")) |>
      dplyr::filter(density != "none") |> # remove non-answers
      dplyr::select(person_id, density) |>
      dplyr::distinct() # ensure each person_id is represented once

    # Right join with the original survey_df to include participants without scores as NA
    df_density <- survey_df |>
      dplyr::select(person_id) |>
      dplyr::distinct() |> # ensure each person_id is represented once
      dplyr::right_join(df_density, by = "person_id")
  }
  df_density
}
