% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_crime_safety.R
\name{calc_crime_safety}
\alias{calc_crime_safety}
\title{Calculate Crime Safety Score}
\usage{
calc_crime_safety(survey_df)
}
\arguments{
\item{survey_df}{A data frame containing survey data with at least three columns:
`person_id`, `question_concept_id`, and `answer_concept_id`.}
}
\value{
A data frame with two columns: `person_id` and `crime_safety`, where `crime_safety`
is the calculated crime safety score for each participant. Participants who did not answer
both questions will have an NA score.
}
\description{
This function computes a crime safety score ranging from 1 to 4 based on
survey responses. The score is the mean of two specific item scores, where higher
scores indicate a greater sense of crime safety in the neighborhood.
}
\examples{
# Create a sample survey data frame
survey_df <- data.frame(
  person_id = c(1, 1, 2, 2, 3, 3, 4, 4),
  question_concept_id = c(40192414, 40192492, 40192414, 40192492,
                          40192414, 40192492, 40192414, 40192492),
  answer_concept_id = c(40192514, 40192478, 40192527, 40192422,
                        40192514, 40192527, 40192422, 40192478)
)

# Compute crime safety scores
crime_safety_scores <- calc_crime_safety(survey_df)
head(crime_safety_scores)

}
