#' Calculate Environmental Support for Physical Activity (SPA) Score
#'
#' This function computes a numeric score representing the environmental support for physical activity (SPA).
#' The score ranges from 7 to 28, with higher scores indicating greater environmental support for physical activity.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `spa`, where `spa` is the sum score for environmental support for physical activity.
#' The score is based on responses to seven items, with higher values indicating more support for physical activity.
#' Participants who did not answer all seven questions will have NA values.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = rep(1:3, each = 7),
#'   question_concept_id = rep(c(40192436, 40192440, 40192437, 40192431,
#'                               40192410, 40192492, 40192414), times = 3),
#'   answer_concept_id = sample(c(40192514, 40192478, 40192527, 40192422),
#'                              21, replace = TRUE)
#' )
#'
#' # Compute environmental support for physical activity (SPA) scores
#' spa_scores <- calc_spa(survey_df)
#' head(spa_scores)
#'
#' @export
calc_spa <- function(survey_df) {
  if (!is.null(survey_df)){
    df_spa  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192436, 40192440, 40192437, 40192431, 40192410,
                                               40192492, 40192414)) |> # 7 specific items
      # 40192436 = Many shops, stores, markets or other places to buy things I need are within easy walking distance of
      #            my home. Would you say that you...
      # 40192440 = It is within a 10-15 minute walk to a transit stop (such as bus, train, trolley, or tram) from my home.
      #            Would you say that you...
      # 40192437 = There are sidewalks on most of the streets in my neighborhood. Would you say that you...
      # 40192431 = There are facilities to bicycle in or near my neighborhood, such as special lanes, separate paths or
      #            trails, or shared use paths for cycles and pedestrians. Would you say that you...
      # 40192410 = My neighborhood has several free or low-cost recreation facilities, such as parks, walking trails,
      #            bike paths, recreation centers, playgrounds, public swimming pools, etc. Would you say that you...
      # 40192492 = The crime rate in my neighborhood makes it unsafe to go on walks at night. Would you say that you...
      # 40192414 = The crime rate in my neighborhood makes it unsafe to go on walks during the day. Would you say that you...
      dplyr::select(person_id, question_concept_id, answer_concept_id) |> # map answer_concept_id to value
      dplyr::mutate(value = dplyr::case_when(
        answer_concept_id == 40192514 ~ 4, # Strongly agree
        answer_concept_id == 40192478 ~ 3, # Somewhat agree
        answer_concept_id == 40192527 ~ 2, # Somewhat disagree
        answer_concept_id == 40192422 ~ 1, # Strongly disagree
        TRUE ~ 999)) |>
      dplyr::filter(value != 999) |> # remove skips
      dplyr::mutate(value = dplyr::case_when(
        # reverse code for 2 questions
        question_concept_id == 40192492 ~ 5 - value, # 4 -> 1, 3 -> 2 etc
        question_concept_id == 40192414 ~ 5 - value,
        TRUE ~ value)) |>
      dplyr::group_by(person_id) |> # group by person_id and calculate sum score
      dplyr::mutate(spa = sum(value,
                              na.rm = TRUE),
                    nrows = length(value)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 7) |> # include only participants who answered all 7 questions
      dplyr::select(person_id, spa) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_spa
}
