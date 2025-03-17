#' Calculate Neighborhood Social Disorder Score
#'
#' This function computes a numeric score representing the level of social disorder in a participant's neighborhood.
#' The score ranges from 1 to 4, with higher scores indicating higher social disorder and lower scores indicating social order.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `social_disorder`, where `social_disorder` is the average score for neighborhood social disorder
#' for each participant. The score is calculated as the mean of seven items, with higher values indicating more social disorder.
#' Participants who did not answer all seven questions will have NA values.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = rep(1:3, each = 7),
#'   question_concept_id = rep(c(40192500, 40192493, 40192457, 40192476,
#'                               40192404, 40192400, 40192384), times = 3),
#'   answer_concept_id = sample(c(40192514, 40192455, 40192408, 40192422),
#'                              21, replace = TRUE)
#' )
#'
#' # Compute neighborhood social disorder scores
#' social_disorder_scores <- calc_social_disorder(survey_df)
#' head(social_disorder_scores)
#'
#' @export
calc_social_disorder <- function(survey_df) {
  if (!is.null(survey_df)){
    df_social_disorder  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192500, 40192493, 40192457, 40192476,
                                               40192404, 40192400, 40192384)) |> # 7 specific items
      # 40192500 = How much you agree or disagree that there are too many people hanging around on the streets near your home?
      # 40192493 = How much you agree or disagree that there is a lot of crime in your neighborhood?
      # 40192457 = How much you agree or disagree that there is too much drug use in your neighborhood?
      # 40192476 = How much you agree or disagree that there is too much alcohol use in your neighborhood?
      # 40192404 = How much you agree or disagree that you are always having trouble with your neighbors?
      # 40192400 = How much you agree or disagree that in your neighborhood people watch out for each other?
      # 40192384 = How much you agree or disagree that your neighborhood is safe?
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
        question_concept_id == 40192400 ~ 5 - value, # 4 -> 1, 3 -> 2, etc
        question_concept_id == 40192384 ~ 5 - value,
        TRUE ~ value)) |>
      dplyr::group_by(person_id) |> # group by person_id and calculate mean score
      dplyr::mutate(social_disorder = round(mean(value,
                                                 na.rm = TRUE), 2), # rounded to 2 decimals
                    nrows = length(value)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 7) |> # include only participants who answered all 7 questions
      dplyr::select(person_id, social_disorder) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_social_disorder
}
