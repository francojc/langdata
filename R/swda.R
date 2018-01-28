#' Switchboard Dialog Act Corpus
#'
#' A dataset containing the 1,1150 conversations of 440 speakers of American
#' English.
#'
#' @format A data frame with 223,506 rows and 11 variables:
#' \describe{
#'   \item{doc_id}{ID for each conversation document}
#'   \item{damsl_tag}{DAMSL dialog act annotation labels}
#'   \item{speaker}{Label for each speaker in the conversation}
#'   \item{turn_num}{Number of contiguous utterance turns for a given speaker}
#'   \item{utterance_num}{The cumulative number of utterances in the conversation}
#'   \item{utterance_text}{The actual dialog utterance}
#'   \item{speaker_id}{Unique speaker identification code}
#'   \item{sex}{Sex of the speaker}
#'   \item{birth_year}{Year that the speaker was born}
#'   \item{dialect_area}{Region from the US where the speaker spent first 10 years}
#'   \item{education}{Highest educational level attained}
#' }
#' @source \url{https://catalog.ldc.upenn.edu/docs/LDC97S62/}
"swda"
