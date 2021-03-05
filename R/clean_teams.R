#' Cleaning a transcript from Microsoft Teams
#'
#' This function takes a caption file from Microsoft Teams/Streams and
#' makes it a more readable/editable transcript.
#'
#' This can then be proofread for transcription errors, and once ready, further
#' processed.
#'
#' The output can be set-up to retain the time-stamps related to the recording.
#' If keepTime is TRUE a single timestamp (rather than a time range) starts each line.
#' By default, there is also a flag indicating lines where the transcription
#' confidence is low — ie they need more attention when manually cleaning.
#' confFlag sets the level at which lines are flagged.
#'
#' @param input For `clean_teams` a path to a file that contains
#' the source transcription.
#' @param output If not `NULL`,
#' this is the name (and path) of the file in which to save the processed source (if it
#' *is* `NULL`, a new file will be created with `_clean` appended to the file name).
#' @param keepTime If true, retains the timestamp for each section at the start of the line
#' @param flagConfidence If true puts *** at the start of each line where the
#' automated transcribing is uncertain. Threshold for this flag is set by `confFlag`
#' @param confFlag sets the threshold below which a line is marked as Teams having
#' poor confidence in the transcription


clean_teams <- function(file,
                        output = NULL,
                        keepTime = FALSE,
                        flagConfidence = TRUE,
                        confFlag = c(0.70),
                        preventOverwriting = rock::opts$get(preventOverwriting),
                        encoding = rock::opts$get(encoding),
                        silent = rock::opts$get(silent)
) {

  # read in text from file
  if (file.exists(input)) {
    res <- readLines(input,
                     encoding=encoding);
  } else {
    stop("Please specify a file") #This else is mostly here for directories
  }

  # get duration of audio
  duration <- regmatches(res, regexpr("(?<=NOTE duration:\")[^\"]*", res, perl = TRUE))

  # get language
  language <- regmatches(res, regexpr("(?<=language:).*", res, perl = TRUE))

  res.long <- c(paste(res), collapse = " ")
  chunks <- strsplit(res.long, "NOTE Confidence: ")

  conf <- regmatches(res, regexpr("(?<=NOTE Confidence: )0.[0-9]*", res, perl = TRUE))
  conf <- as.numeric(conf)
  time <- regmatches(res, regexpr(".*(?= -->)", res, perl = TRUE))

  small <- res[1:50]
  small <- res
  small <- paste(small, collapse = " ")
  small <- gsub("\"", "", small)
  chunks <- strsplit(small, "NOTE ")[[1]]

  chunkText <- regmatches(chunks,
                          regexpr("-> .*", chunks, perl = TRUE))
  chunkText <- gsub("-> \\d*:\\d*:\\d*.\\d{1,3}", "", chunkText)

  confFlagMark <- rep("", length(conf))
  confFlagMark[which(conf < confFlag)] <- "***"

  if(keepTime == FALSE) {
    time <- NULL
  }

  if(confFlag == FALSE) {
    confFlagMark <- NULL
  }

  lines <- paste0(time, confFlagMark, chunkText)

  if(is.null(output)){
    ext <- tools::file_ext(input)
    input <- gsub(paste0("\\.", ext), "", input)
    output <- paste0(dirname(input), "/", basename(input), "_clean.", ext)
  }
  #consider preventing overwriting behaviour

  writeLines(lines, output)

}
