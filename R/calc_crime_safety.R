#' Calculate Crime Safety Score
#'
#' This function computes a crime safety score ranging from 1 to 4 based on
#' survey responses. The score is the mean of two specific item scores, where higher
#' scores indicate a greater sense of crime safety in the neighborhood.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `crime_safety`, where `crime_safety`
#' is the calculated crime safety score for each participant. Participants who did not answer
#' both questions will have an NA score.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = c(1, 1, 2, 2, 3, 3, 4, 4),
#'   question_concept_id = c(40192414, 40192492, 40192414, 40192492,
#'                           40192414, 40192492, 40192414, 40192492),
#'   answer_concept_id = c(40192514, 40192478, 40192527, 40192422,
#'                         40192514, 40192527, 40192422, 40192478)
#' )
#'
#' # Compute crime safety scores
#' crime_safety_scores <- calc_crime_safety(survey_df)
#' head(crime_safety_scores)
#'
#' @export
calc_crime_safety <- function(survey_df) {
  if (!is.null(survey_df)){
    df_crime_safety  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192414, 40192492)) |> # 2 specific items
      # 40192414 = The crime rate in my neighborhood makes it unsafe to go on walks during the day. Would you say that you...
      # 40192492 = The crime rate in my neighborhood makes it unsafe to go on walks at night. Would you say that you...
      dplyr::select(person_id, question_concept_id, answer_concept_id) |> # map answer_concept_id to value
      dplyr::mutate(value = dplyr::case_when(
        # reverse code for 2 questions
        answer_concept_id == 40192514 ~ 1, # Strongly agree
        answer_concept_id == 40192478 ~ 2, # Somewhat agree
        answer_concept_id == 40192527 ~ 3, # Somewhat disagree
        answer_concept_id == 40192422 ~ 4, # Strongly disagree
        TRUE ~ 999)) |>
      dplyr::filter(value != 999) |> # remove skips
      dplyr::group_by(person_id) |> # group by person_id and calculate sum score
      dplyr::mutate(crime_safety = round(mean(value,
                                              na.rm = TRUE), 2), # rounded to 2 decimals
                    nrows = length(value)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 2) |> # include only participants who answered all 2 questions
      dplyr::select(person_id, crime_safety) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_crime_safety
}
