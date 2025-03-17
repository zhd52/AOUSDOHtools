#' Calculate Food Insecurity Risk
#'
#' This function creates a binary categorical variable (TRUE/FALSE) indicating whether a participant
#' is at risk or currently experiencing food insecurity based on their responses to two specific survey items.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `food_insecurity`, where `food_insecurity`
#' is TRUE if the participant reported experiencing food insecurity, and FALSE otherwise. Participants who
#' did not respond to both questions will have an NA value for `food_insecurity`.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
#'   question_concept_id = rep(c(40192426, 40192517), 5),
#'   answer_concept_id = c(40192508, 40192488, 40192508, 903096, 903096, 903096,
#'                         40192488, 40192488, 40192508, 40192508)
#' )
#'
#' # Compute food insecurity risk scores
#' food_insecurity_scores <- calc_food_insecurity(survey_df)
#' head(food_insecurity_scores)
#'
#' @export
calc_food_insecurity <- function(survey_df) {
  if (!is.null(survey_df)){
    df_food_insecurity  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192426, 40192517)) |> # 2 specific items
      # 40192426 = Within the past 12 months, were you worried whether the food you had bought just didn't last and
      #            you didn't have money to get more?
      # 40192517 = Within the past 12 months, were you worried whether your food would run out before you got money
      #            to buy more?
      dplyr::select(person_id, question_concept_id, answer_concept_id) |> # map answer_concept_id to value
      dplyr::filter(answer_concept_id != 903096) |> # remove skips
      dplyr::group_by(person_id) |> # group by person_id and calculate score
      dplyr::mutate(food_insecurity = 40192508 %in% answer_concept_id | 40192488 %in% answer_concept_id,
                    # label as positive for food insecurity if answered positively to either question
                    # 40192508 = Often true
                    # 40192488 = Sometimes true
                    nrows = length(answer_concept_id)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 2 | food_insecurity == TRUE) |>
      # include only participants who answered all 2 questions OR there is a positive
      dplyr::select(person_id, food_insecurity) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_food_insecurity
}
