#' Calculate Mean Perceived Health Care Discrimination Score
#'
#' This function creates a numeric score (range 1-5) that reflects the mean level of perceived discrimination in
#' health care based on responses to seven specific survey items. Higher scores indicate greater perceived
#' discrimination in health care.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @return A data frame with two columns: `person_id` and `hcd_mean`, where `hcd_mean`
#' represents the mean level of perceived discrimination in health care. Participants who did not respond
#' to all 7 items will have an NA value for `hcd_mean`.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = rep(1:3, each = 7),
#'   question_concept_id = rep(c(40192383, 40192394, 40192423, 40192425,
#'                               40192497, 40192503, 40192505), times = 3),
#'   answer_concept_id = sample(c(40192465, 40192481, 40192429, 40192382, 40192515),
#'                              21, replace = TRUE)
#' )
#'
#' # Compute mean perceived health care discrimination scores
#' hcd_mean_scores <- calc_hcd_mean(survey_df)
#' head(hcd_mean_scores)
#'
#' @export
calc_hcd_mean <- function(survey_df) {
  if (!is.null(survey_df)){
    df_hcd_mean  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192383, 40192394, 40192423, 40192425, 40192497,
                                               40192503, 40192505)) |> # 7 specific items
      # 40192383 = How often does a doctor or nurse act as if he or she is better than you when you go to a doctor's office
      #            or other health care provider?
      # 40192394 = How often do you feel like a doctor or nurse is not listening to what you were saying,
      #            when you go to a doctor's office or other health care provider?
      # 40192423 = How often does a doctor or nurse act as if he or she is afraid of you when you go to a doctor's office
      #            or other health care provider?
      # 40192425 = How often are you treated with less respect than other people when you go to a doctor's office
      #            or other health care provider?
      # 40192497 = How often are you treated with less courtesy than other people when you go to a doctor's office
      #            or other health care provider?
      # 40192503 = How often do you receive poorer service than others when you go to a doctor's office
      #            or other health care provider?
      # 40192505 = How often does a doctor or nurse act as if he or she thinks you are not smart when you go to a doctor's
      #            office or other health care provider?
      dplyr::select(person_id, question_concept_id, answer_concept_id) |> # map answer_concept_id to value
      dplyr::mutate(value = dplyr::case_when(
        answer_concept_id == 40192465 ~ 1, # Never
        answer_concept_id == 40192481 ~ 2, # Rarely
        answer_concept_id == 40192429 ~ 3, # Sometimes
        answer_concept_id == 40192382 ~ 4, # Most of the time
        answer_concept_id == 40192515 ~ 5, # Always
        TRUE ~ 999)) |>
      dplyr::filter(value != 999) |> # remove skips
      dplyr::group_by(person_id) |> # group by person_id and calculate mean score
      dplyr::mutate(hcd_mean = round(mean(value,
                                          na.rm = TRUE), 2), # rounded to 2 decimals
                    nrows = length(value)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 7) |> # include only participants who answered all 7 questions
      dplyr::select(person_id, hcd_mean) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id') |>
      dplyr::ungroup() # ungroup
  }
  df_hcd_mean
}
