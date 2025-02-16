#' Calculate Neighborhood Physical Disorder Score
#'
#' This function computes a numeric score representing the level of physical disorder in a participant's neighborhood.
#' The score ranges from 1 to 4, with higher scores indicating higher physical disorder, and lower scores indicating better physical order in the neighborhood.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `physical_disorder`, where `physical_disorder` is the average score for neighborhood physical disorder
#' for each participant. The score is calculated as the mean of six items, with higher values indicating more physical disorder.
#' Participants who did not answer all six questions will have NA values.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = rep(1:3, each = 6),
#'   question_concept_id = rep(c(40192420, 40192522, 40192412,
#'                               40192469, 40192456, 40192386), times = 3),
#'   answer_concept_id = sample(c(40192514, 40192455, 40192408, 40192422),
#'                              18, replace = TRUE)
#' )
#'
#' # Compute neighborhood physical disorder scores
#' physical_disorder_scores <- calc_physical_disorder(survey_df)
#' head(physical_disorder_scores)
#'
#' @export
calc_physical_disorder <- function(survey_df) {
  if (!is.null(survey_df)){
    df_physical_disorder  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192420, 40192522, 40192412,
                                               40192469, 40192456, 40192386)) |> # 6 specific items
      # 40192420 = How much you agree or disagree that there is a lot of graffiti in your neighborhood?
      # 40192522 = How much you agree or disagree that your neighborhood is noisy?
      # 40192412 = How much you agree or disagree that vandalism is common in your neighborhood?
      # 40192469 = How much you agree or disagree that there are lot of abandoned buildings in your neighborhood?
      # 40192456 = How much you agree or disagree that your neighborhood is clean?
      # 40192386 = How much you agree or disagree that people in your neighborhood take good care of their houses and
      #            apartments?
      dplyr::select(person_id, question_concept_id, answer_concept_id) |>
      dplyr::mutate(value = dplyr::case_when(
        answer_concept_id == 40192514 ~ 4, # Strongly agree
        answer_concept_id == 40192455 ~ 3, # Agree
        answer_concept_id == 40192408 ~ 2, # Disagree
        answer_concept_id == 40192422 ~ 1, # Strongly disagree
        TRUE ~ 999)) |>
      dplyr::filter(value != 999) |> # remove skips
      dplyr::mutate(value = dplyr::case_when(
        # reverse code for 2 questions
        question_concept_id == 40192456 ~ 5 - value, # 4 -> 1, 3 -> 2, etc
        question_concept_id == 40192386 ~ 5 - value,
        TRUE ~ value)) |>
      dplyr::group_by(person_id) |> # group by person_id and calculate mean score
      dplyr::mutate(physical_disorder = round(mean(value,
                                                   na.rm = TRUE), 2), # rounded to 2 decimals
                    nrows = length(value)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 6) |> # include only participants who answered all 6 questions
      dplyr::select(person_id, physical_disorder) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_physical_disorder
}
