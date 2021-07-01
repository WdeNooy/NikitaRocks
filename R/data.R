#' Time-constant characteristics of the pupils.
#'
#' The data describe characteristics of 26 fictional pupils that do not change.
#' This is one of the four basic data frames from which all analysis data frames
#' have been derived.
#' The data have been simulated for the purpose of illustrating different
#' statistical models for events happening within networks.
#'
#' @format A data frame (tibble) with eight variables:
#' \describe{
#'   \item{ID}{integer pupil ID, key to other data frames}
#'   \item{label}{pupil's first name, as a caracter string}
#'   \item{present}{presence of the pupil at the start of data collection, as boolean; required for visualizations with the ndtv package}
#'   \item{onset}{time point at which the pupil appears within the data set, as integer; required for visualizations with the ndtv package}
#'   \item{terminus}{time point at which the pupil appears within the data set for the last time, as integer; required for visualizations with the ndtv package}
#'   \item{sex}{pupil's sex, boy (0) or girl(1)}
#'   \item{ethnicity}{pupil's ethnicity, coded as a single letter (A, B, C, D, E)}
#'   \item{adhd}{pupil's ADHD level, coded as a number between 0 and 10}
#' }
"pupils_const"

#' Time-varying characteristics of the pupils.
#'
#' The data describe characteristics of 26 fictional pupils that may change,
#' namely the actions of using their voices and playing a new game app.
#' This is one of the four basic data frames from which all analysis data frames
#' have been derived.
#' The data have been simulated for the purpose of illustrating different
#' statistical models for events happening within networks.
#'
#' @format A data frame (tibble) with eight variables:
#' \describe{
#'   \item{ID}{integer pupil ID, key to other data frames}
#'   \item{breakID}{sequential number of the break, as integer (1 - 10)}
#'   \item{onset}{time point within the break at which a pupil starts an action, as real number indicating the (fraction of) the minute in the break}
#'   \item{onset.censored}{indicates whether the start time (onset) was unobserved, as boolean}
#'   \item{terminus}{time point within the break at which a pupil ends an action, as real number indicating the (fraction of) the minute in the break}
#'   \item{terminus.censored}{indicates whether the end time (terminus) was unobserved or terminated by circumstances, as boolean}
#'   \item{loudness}{(average) loudness of pupil's voice, as a real number between 0.05 (whisper) to 1 (loudest cry)}
#'   \item{gameapp}{pupil is playing a new game app on their own mobile device, as integer: 0 (No) or 1 (Yes)}
#' }
"pupils_dyn"

