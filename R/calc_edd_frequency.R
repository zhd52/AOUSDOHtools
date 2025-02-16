#' Calculate Everyday Discrimination Frequency Score
#'
#' This function computes a frequency score indicating how often participants experience perceived discrimination
#' based on 9 specific survey items. The score ranges from 9 to 54, where higher scores indicate more frequent
#' perceived unfair treatment. The function also allows for filtering by a specific reason for discrimination.
#'
#' @param survey_df A data frame containing survey data with at least three columns:
#' `person_id`, `question_concept_id`, and `answer_concept_id`.
#'
#' @param reason An optional argument specifying the reason for perceived discrimination, e.g., race or age.
#'               If provided, the function will limit the analysis to participants who reported this reason.
#'
#' @return A data frame with two columns: `person_id` and `edd_frequency`, where `edd_frequency`
#' is the calculated frequency score for each participant. Participants who did not answer all 9 questions
#' or did not specify the given reason (if provided) will have an NA score.
#'
#' @importFrom dplyr filter select mutate case_when group_by distinct right_join summarize ungroup
#'
#' @examples
#' # Create a sample survey data frame
#' survey_df <- data.frame(
#'   person_id = rep(1:3, each = 9),
#'   question_concept_id = rep(c(40192380, 40192395, 40192416, 40192451, 40192466,
#'                               40192489, 40192490, 40192496, 40192519), times = 3),
#'   answer_concept_id = sample(c(40192465, 40192464, 40192453, 40192461, 40192391,
#'                                40192421), 27, replace = TRUE)
#' )
#'
#' # Compute everyday discrimination frequency scores
#' edd_frequency_scores <- calc_edd_frequency(survey_df)
#' head(edd_frequency_scores)
#'
#' @export
calc_edd_frequency <- function(survey_df, reason) {
  if (!is.null(survey_df)){
    df_edd_frequency  <-  survey_df |>
      dplyr::filter(question_concept_id %in% c(40192380, 40192395, 40192416, 40192451, 40192466, 40192489, 40192490,
                                               40192496, 40192519)) |> # 9 specific items
      # 40192380 = In your day-to-day life, how often do people act as if they are afraid of you?
      # 40192395 = In your day-to-day life, how often do people act as if they think you are dishonest?
      # 40192416 = In your day-to-day life, how often do you receive poorer service than other people at restaurants or
      #            stores?
      # 40192451 = In your day-to-day life, how often are you threatened or harassed?
      # 40192466 = In your day-to-day life, how often are you treated with less courtesy than other people?
      # 40192489 = In your day-to-day life, how often are you treated with less respect than other people?
      # 40192490 = In your day-to-day life, how often do people act as if they think you are not smart?
      # 40192496 = In your day-to-day life, how often do people act as if they're better than you are?
      # 40192519 = In your day-to-day life, how often are you called names or insulted?
      dplyr::select(person_id, question_concept_id, answer_concept_id) |> # map answer_concept_id to value
      dplyr::mutate(value = dplyr::case_when(
        answer_concept_id == 40192465 ~ 1, # Never
        answer_concept_id == 40192464 ~ 2, # Less than once a year
        answer_concept_id == 40192453 ~ 3, # A few times a year
        answer_concept_id == 40192461 ~ 4, # A few times a month
        answer_concept_id == 40192391 ~ 5, # At least once a week
        answer_concept_id == 40192421 ~ 6, # Almost everyday
        TRUE ~ 999)) |>
      dplyr::filter(value != 999) |> # remove skips
      dplyr::group_by(person_id) |> # group by person_id and calculate sum score
      dplyr::mutate(edd_frequency = sum(value,
                                        na.rm = TRUE),
                    nrows = length(value)) |> # how many questions did the participant answer?
      dplyr::filter(nrows == 9) |> # include only participants who answered all 9 questions
      dplyr::select(person_id, edd_frequency) |> # only 2 columns in the final result
      dplyr::distinct(person_id, .keep_all = TRUE) |> # remove duplicate rows
      dplyr::ungroup() # ungroup
    if (!missing(reason)) { # if reason for perceived discrimination is provided
      x <- survey_df |>
        dplyr::filter(question_concept_id == 40192428 & answer == reason)
      # 40192428 = Discrimination: What do you think is the main reason for these experiences?
      y <- x$person_id # which participants indicated the given reason for discrimination
      df_edd_frequency <- df_edd_frequency |>
        dplyr::filter(person_id %in% y) # filter to these participants
    }
    df_edd_frequency <- df_edd_frequency |>
      dplyr::right_join(survey_df |> # include participants without scores as NA
                          dplyr::group_by(person_id) |>
                          dplyr::summarize(),
                        by = 'person_id')
  }
  df_edd_frequency
}
