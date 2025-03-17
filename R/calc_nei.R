#' Calculate Neighborhood Environment Index (NEI)
#'
#' This function computes a Neighborhood Environment Index (NEI) score based on 6 specific items related to the built environment for physical activity.
#' The score ranges from 0 to 6, with higher scores indicating a more favorable environment for physical activity.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `nei`, where `nei`
#' is the calculated Neighborhood Environment Index score for each participant. The score is the sum of individual item scores,
#' where higher values indicate a more favorable built environment for physical activity. Participants who did not answer all 6 questions will have NA values.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = rep(1:3, each = 6),
#'   question_concept_id = rep(c(40192410, 40192431, 40192436, 40192437,
#'                               40192440, 40192458), times = 3),
#'   answer_concept_id = sample(c(40192527, 40192422, 40192407, 903087,
#'                                 903096, 40192520, 40192514, 40192455),
#'                              18, replace = TRUE)
#' )
#'
#' # Compute Neighborhood Environment Index (NEI) scores
#' nei_scores <- calc_nei(survey_df)
#' head(nei_scores)
#'
#' @export
calc_nei <- function(survey_df) {
  if (!is.null(survey_df)){
    df_nei  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192410, 40192431, 40192436, 40192437,
                                               40192440, 40192458)) |> # 6 specific items
      # 40192410 = My neighborhood has several free or low-cost recreation facilities, such as parks, walking trails,
      #            bike paths, recreation centers, playgrounds, public swimming pools, etc. Would you say that you...
      # 40192431 = There are facilities to bicycle in or near my neighborhood, such as special lanes, separate paths or
      #            trails, or shared use paths for cycles and pedestrians. Would you say that you...
      # 40192436 = Many shops, stores, markets or other places to buy things I need are within easy walking distance of
      #            my home. Would you say that you...
      # 40192437 = There are sidewalks on most of the streets in my neighborhood. Would you say that you...
      # 40192440 = It is within a 10-15 minute walk to a transit stop (such as bus, train, trolley, or tram) from my home.
      #            Would you say that you...
      # 40192458 = What is the main type of housing in your neighborhood?
      dplyr::select(person_id, question_concept_id, answer_concept_id) |> # map answer_concept_id to value
      dplyr::mutate(value = dplyr::case_when(
        # any legitimate response option other than the first 3 gets coded to a 1
        answer_concept_id == 40192527 ~ 0, # Somewhat disagree
        answer_concept_id == 40192422 ~ 0, # Strongly disagree
        answer_concept_id == 40192407 ~ 0, # Detached single-family housing
        answer_concept_id == 903087 ~ 999, # PMI: Dont Know
        answer_concept_id == 903096 ~ 999, # PMI: Skip
        answer_concept_id == 40192520 ~ 999, # Does not apply to my neighborhood
        TRUE ~ 1)) |>
      dplyr::filter(value != 999) |> # remove skips
      dplyr::group_by(person_id) |> # group by person_id and calculate sum score
      dplyr::mutate(nei = sum(value,
                              na.rm = TRUE),
                    nrows = length(value)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 6) |> # include only participants who answered all 6 questions
      dplyr::select(person_id, nei) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_nei
}
