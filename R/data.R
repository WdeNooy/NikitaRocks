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
#'   \item{adhd}{pupil's ADHD level, coded as a real number between 0 and 10}
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
#'   \item{onset}{time point within the break at which a pupil starts an action, as real number indicating the (fraction of) the minute within the break}
#'   \item{onset.censored}{indicates whether the start time (onset) was unobserved, as boolean}
#'   \item{terminus}{time point within the break at which a pupil ends an action, as real number indicating the (fraction of) the minute within the break}
#'   \item{terminus.censored}{indicates whether the end time (terminus) was unobserved or terminated by circumstances, as boolean}
#'   \item{loudness}{(average) loudness of pupil's voice, as a real number between 0.05 (whisper) to 1 (loudest cry)}
#'   \item{gameapp}{pupil is playing a new game app on their own mobile device, as integer: 0 (No) or 1 (Yes)}
#' }
"pupils_dyn"

#' prepared data for the analysis of statement negativity.
#'
#' For each statement made by a pupil in Break 1 directed at another pupil, the
#' dependent and independent variables were created from the four basic data frames
#' (pupils_const, pupils_dyn, Pairs_const, Pairs_dyn).
#'
#' @format A data frame (tibble) with twenty variables:
#' \describe{
#'   \item{from}{integer pupil ID for the speaker of the statement, key to other data frames}
#'   \item{to}{integer pupil ID for the addressee of the statement, key to other data frames}
#'   \item{breakID}{integer sequential number of the break in which the statement was made (constant: only Break 1)}
#'   \item{onset}{time point within the break at which a pupil starts the statement, as real number indicating the (fraction of) the minute within the break}
#'   \item{terminus}{time point within the break at which a pupil ends the statement, as real number indicating the (fraction of) the minute within the break}
#'   \item{negative}{indicator of the valence of the statement: 1 = negative, 0 = positive (not negative)}
#'   \item{playmates}{indicator whether speaker and addressee are currently playing together: 1 = yes, 0 = no}
#'   \item{received_neg}{negativity reciprocity indicator: last utterance received by speaker (from) from addressee (to) that ended no longer than 3 minutes before the current utterance (onset) was negative (1) or positive/not negative (0)}
#'   \item{received_pos}{positivity reciprocity indicator: last utterance received by speaker (from) from addressee (to) that ended no longer than 3 minutes before the current utterance (onset) was positive/not negative (1) or negative (0)}
#'   \item{balance_neg}{balance indicator predicting a negative statement: number of two-step ties (semipaths of length 2) between speaker and addressee with a negative sign (resulting from multiplying the signs of the two steps) using only the last statement for each pair of pupils that started no longer than 5 minutes before the current utterance (onset).}
#'   \item{balance_pos}{balance indicator predicting a positive statement: number of two-step ties (semipaths of length 2) between speaker and addressee with a positive sign (resulting from multiplying the signs of the two steps) using only the last statement for each pair of pupils that started no longer than 5 minutes before the current utterance (onset).}
#'   \item{sex_from}{sex indicator of the speaker of the statement (from): 1 = girl, 0 = boy}
#'   \item{adhd_from}{ADHD level of the speaker of the statement (from), coded as a real number between 0 and 10}
#'   \item{sex_to}{sex indicator of the addressee of the statement (to): 1 = girl, 0 = boy}
#'   \item{adhd_to}{ADHD level of the addressee of the statement (to), coded as a real number between 0 and 10}
#'   \item{lastplayed}{number of days since the speaker and addressee (from & to) last played together}
#'   \item{friend}{indicator of friendship between speaker and addressee (from & to): 1 = friends, 0 = not friends}
#'   \item{simil_sex}{indicator whether speaker and addressee (from & to) have the same sex: 1 = yes, 0 = no}
#'   \item{simil_ethn}{indicator whether speaker and addressee (from & to) have the same ethnicity: 1 = yes, 0 = no}
#'   \item{simil_adhd}{similarity of the ADHD levels of speaker and addressee (from & to): sqrt(0.5 / (0.05 + abs(adhd.x - adhd.y))) with adhd.x the ADHD score of the speaker and adhd.y the ADHD score of the addressee}
#'   }
"utterances"

