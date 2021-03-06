% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{pairs_const}
\alias{pairs_const}
\title{Time-constant characteristics of the pairs.}
\format{
A data frame (tibble) with seven variables:
\describe{
  \item{from}{integer pupil ID for the speaker of the statement, key to other data frames}
  \item{to}{integer pupil ID for the addressee of the statement, key to other data frames}
  \item{lastplayed}{number of days the two pupils had not played together at the start of Break 1, as integer}
  \item{friend}{the two pupils are friends during the period of observation: yes (1) or no (0)}
  \item{simil_sex}{the two pupils have the same sex: yes (1) or no (0)}
  \item{simil_ethn}{the two pupils have the same ethnicity: yes (1) or no (0)}
  \item{simil_adhd}{the similarity of the two pupils'ADHD levels, calculated as: sqrt(0.5 / (0.05 + abs(adhd.x - adhd.y))) with adhd.x the ADHD score of the first pupil (from) and adhd.y the ADHD score of the second pupil (to)}
}
}
\usage{
pairs_const
}
\description{
The data describe friendships and similarities among the 26 fictional pupils
that do not change.
This is one of the four basic data frames from which all analysis data frames
have been derived.
The data have been simulated for the purpose of illustrating different
statistical models for events happening within networks.
}
\keyword{datasets}
