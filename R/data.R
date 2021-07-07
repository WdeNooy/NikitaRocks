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
#'   \item{label}{pupil's first name, as a character string}
#'   \item{present}{presence of the pupil at the start of data collection, as boolean; required for analysis with the goldfish package}
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

#' Time-constant characteristics of the pairs.
#'
#' The data describe friendships and similarities among the 26 fictional pupils
#' that do not change.
#' This is one of the four basic data frames from which all analysis data frames
#' have been derived.
#' The data have been simulated for the purpose of illustrating different
#' statistical models for events happening within networks.
#'
#' @format A data frame (tibble) with seven variables:
#' \describe{
#'   \item{from}{integer pupil ID for the speaker of the statement, key to other data frames}
#'   \item{to}{integer pupil ID for the addressee of the statement, key to other data frames}
#'   \item{lastplayed}{number of days the two pupils had not played together at the start of Break 1, as integer}
#'   \item{friend}{the two pupils are friends during the period of observation: yes (1) or no (0)}
#'   \item{simil_sex}{the two pupils have the same sex: yes (1) or no (0)}
#'   \item{simil_ethn}{the two pupils have the same ethnicity: yes (1) or no (0)}
#'   \item{simil_adhd}{the similarity of the two pupils'ADHD levels, calculated as: sqrt(0.5 / (0.05 + abs(adhd.x - adhd.y))) with adhd.x the ADHD score of the first pupil (from) and adhd.y the ADHD score of the second pupil (to)}
#' }
"pairs_const"

#' Time-varying characteristics of the pupils.
#'
#' The data describe ties and interactions among the 26 fictional pupils that
#' change over time. It contains utterances (directed, one pupil speaking to
#' another) for Break 1 and playmate ties (undirected, both pupils appear as
#' sender and as receiver) for Breaks 1 to 10.
#' This is one of the four basic data frames from which all analysis data frames
#' have been derived.
#' The data have been simulated for the purpose of illustrating different
#' statistical models for events happening within networks.
#'
#' @format A data frame (tibble) with nine variables:
#' \describe{
#'   \item{from}{integer pupil ID for the sender, key to other data frames}
#'   \item{to}{integer pupil ID for the receiver, key to other data frames}
#'   \item{breakID}{integer sequential number of the break in which the statement was made (constant: only Break 1)}
#'   \item{onset}{time point within the break at which a pupil starts the statement, as real number indicating the (fraction of) the minute within the break}
#'   \item{onset-censored}{is the start of the tie or interaction observed and initiated by the pupils themselves?; boolean}
#'   \item{terminus}{time point within the break at which a pupil ends the statement, as real number indicating the (fraction of) the minute within the break}
#'   \item{terminus-censored}{is the end of the tie or interaction observed and initiated by the pupils themselves?; boolean}
#'   \item{dyntie}{type of dynamic tie or interaction, coded as text: "Playmate" or "Utterance}
#'   \item{negative}{the valence of utterances as integer: negative (1) or positive/not negative (0), NA for playmate ties}
#' }
"pairs_dyn"

#' prepared data for the analysis of statement loudness at the pupil level.
#'
#' For each pupil, average loudness (during Break 1) has been calculated (from
#' basic data set pupils_dyn) and personal characteristics have been added (from
#' basic data set pupils_const).
#'
#' @format A data frame (tibble) with six variables:
#' \describe{
#'   \item{ID}{integer pupil ID, key to other data frames}
#'   \item{avg_loudness}{average loudness of all statements made by a pupil during Break 1, both those directed at another pupil and those that are not; as real number}
#'   \item{label}{pupil's first name, as a character string}
#'   \item{sex}{pupil's sex, boy (0) or girl(1)}
#'   \item{ethnicity}{pupil's ethnicity, coded as a single letter (A, B, C, D, E)}
#'   \item{adhd}{pupil's ADHD level, coded as a real number between 0 and 10}
#'   }
"loudness_average"

#' prepared data for the analysis of statement loudness at the statement level.
#'
#' For each statement during Break 1, loudness is recorded with pupil, start and
#' end time (from basic data set pupils_dyn). Pupil characteristics have been
#' added from basic data set pupils_const. Several indicators of exposure to the
#' loudness of other pupils have been calculated.
#'
#' @format A data frame (tibble) with fourteen variables:
#' \describe{
#'   \item{ID}{integer pupil ID, key to other data frames}
#'   \item{onset}{time point within the break at which a pupil starts an action, as real number indicating the (fraction of) the minute within the break}
#'   \item{terminus}{time point within the break at which a pupil ends an action, as real number indicating the (fraction of) the minute within the break}
#'   \item{loudness}{(average) loudness of pupil's voice, as a real number between 0.05 (whisper) to 1 (loudest cry)}
#'   \item{expo_last_max}{loudness of the loudest voice at the moment a pupil starts an utterance, as real number}
#'   \item{expo_last_mean}{he average sound level at the moment a pupil starts an utterance, as real number}
#'   \item{expo_last_min}{loudness of the softest voice at the moment a pupil starts an utterance, as real number}
#'   \item{expo_minute_max_playmate}{loudness of the loudest voice of playmates' last utterances in the preceding minute, as real number}
#'   \item{expo_minute_max_conversation}{loudness of the loudest voice of conversation partners' last utterances in the preceding minute, as real number}
#'   \item{expo_minute_max_other}{loudness of the loudest voice of other pupils' last utterances in the preceding minute, as real number}
#'   \item{label}{pupil's first name, as a character string}
#'   \item{sex}{pupil's sex, boy (0) or girl(1)}
#'   \item{ethnicity}{pupil's ethnicity, coded as a single letter (A, B, C, D, E)}
#'   \item{adhd}{pupil's ADHD level, coded as a real number between 0 and 10}
#'   }
"loudness_events"

#' prepared data for the analysis of the diffusion of the new game app among pupils.
#'
#' For each pupil-break combination (Breaks 1-10) an observation (row) has been
#' created up to and including the first break in which the pupil has the new
#' game app on their device.
#'
#' @format A data frame (tibble) with eleven variables:
#' \describe{
#'   \item{ID}{integer pupil ID, key to other data frames}
#'   \item{label}{pupil's first name, as a character string}
#'   \item{sex}{pupil's sex, boy (0) or girl(1)}
#'   \item{ethnicity}{pupil's ethnicity, coded as a single letter (A, B, C, D, E)}
#'   \item{adhd}{pupil's ADHD level, coded as a real number between 0 and 10}
#'   \item{breakID}{integer sequential number of the break in which the statement was made (constant: only Break 1)}
#'   \item{breakAdopt}{the sequential number of the break in which the pupil has the game app on their device for the first time, as integer}
#'   \item{adoption}{does the pupil have the game app on their device, as integer: yes (1) or no (0)}
#'   \item{exposure}{number of minutes that a pupil played with a peer who was playing the new game app in the preceding break, summed over all peers with whom the pupil was playing (at the same time), as real number}
#'   \item{friends_adopted}{number of a pupil's friends who adopted the new game app in the previous break}
#'   \item{cum_friends_adopted}{total number of a pupil's friends who adopted the new game app before the current break}
#'   }
"diffusion_data"

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
