#' Cleaning caption files (sources) from Microsoft Teams auto-captioning
#'
#' These functions can be used to 'clean' one or more sets of auto-captions
#' produced by Microsoft Teams. Cleaning consists of several operations: removing
#' various Teams headers, removing or tidying time stamps, removing or flagging
#' lines with low confidence in the transcriping, and removing excess spaces.
#' This produces a more readable/editable transcript, but will still need
#' re-punctuating and manual splitting of utterances
#'
#' This can then be proofread for transcription errors, manually fixing
#' punctuation (Teams punctuation seems based on pause length rather then
#' senetence structure), separating utterances (Teams captioning does not
#' attempt to distinguish speakers) and once ready, further processed.
#'
#' The output can be set-up to retain the time-stamps related to the recording.
#' If keepTime is TRUE a single timestamp (rather than a time range) starts each line.
#' By default, there is also a flag indicating lines where the transcription
#' confidence is low — ie they need more attention when manually cleaning.
#' confFlag sets the level at which lines are flagged.
#'
#' @param input For `clean_source_from_teams` a path to a file that contains
#' the source transcription. If not a path to a file, then a text object (e.g.,
#' character vector)
#' @param output If not `NULL`,
#' this is the name (and path) of the file in which to save the processed source (if it
#' *is* `NULL`, a new file will be created with `_clean` appended to the file name).
#' @param keepTime If true, retains the timestamp for each section at the start of the line
#' @param flagConfidence If true puts *** at the start of each line where the
#' automated transcribing is uncertain. Threshold for this flag is set by `confFlag`
#' @param confFlag sets the threshold below which a line is marked as Teams having
#' poor confidence in the transcription
#' @param removeMultiSpace Teams has several spaces at teh end of each line. If
#' TRUE, these are removed
#'
#' @examples exampleSource <-
#' "WEBVTT
#'
#'  NOTE duration:\"01:14:13.5060000\"
#'
#'  NOTE language:en-us
#'
#'  NOTE Confidence: 0.864107251167297
#'
#'  1fbc54e1-43d1-4f2a-b639-ea8466e6f279
#'  00:00:02.260 --> 00:00:06.412
#'  Go back on WhatsApp. Yeah. So
#'  my first question is just a
#'
#'  NOTE Confidence: 0.864107251167297
#'
#'  2c90cf04-4e4b-4857-8b40-273d37b1e878
#'  00:00:06.412 --> 00:00:09.872
#'  little bit. How did you come
#'  to teach research methods?"
#'
#'  clean_source_from_teams("exampleSource")
#'
#' @rdname cleaning_sources_from_teams
#'
#' @export
clean_source_from_teams <- function(input,
                        output = NULL,
                        keepTime = FALSE,
                        flagConfidence = TRUE,
                        confFlag = c(0.70),
                        removeNewLines = FALSE,
                        removeMultiSpace = TRUE,
                        preventOverwriting = rock::opts$get(preventOverwriting),
                        encoding = rock::opts$get(encoding),
                        silent = rock::opts$get(silent)
) {

  # read in text from file
  if (file.exists(input)) {
    res <- readLines(input,
                     encoding=encoding);
  } else {
    res <- input;
  }

  # get duration of audio
  duration <- regmatches(res, regexpr("(?<=NOTE duration:\")[^\"]*", res, perl = TRUE));

  # get language
  language <- regmatches(res, regexpr("(?<=language:).*", res, perl = TRUE));

  # split into lines
  # res.long <- c(paste(res), collapse = " ");
  # chunks <- strsplit(res.long, "NOTE Confidence: ");  ### from older version of teams


  conf <- regmatches(res, regexpr("(?<=NOTE Confidence: )0.[0-9]*", res, perl = TRUE));
  conf <- as.numeric(conf);
  time <- regmatches(res, regexpr(".*(?= -->)", res, perl = TRUE));
  time2 <- regmatches(res, regexpr("(?<= -->).*", res, perl = TRUE));

  # small <- res[1:50];
  # small <- res;
  # small <- paste(small, collapse = " ");
  # small <- gsub("\"", "", small);
  # chunks <- strsplit(small, "NOTE ")[[1]];

  chunks <- paste(c(res), collapse='\t')
  chunks <- strsplit(chunks, "\t\\d*:", perl = TRUE)
  chunks <- chunks[[1]]

  chunkText <- regmatches(chunks,
                          regexpr("-> .*", chunks, perl = TRUE));
  chunkText <- gsub("-> \\d*:\\d*:\\d*.\\d{1,3}\t", "", chunkText);




  confFlagMark <- rep("", length(conf));
  confFlagMark[which(conf < confFlag)] <- "***";

  if(keepTime == FALSE) {
    time <- NULL;
    time2 <- NULL;
  }

  if(flagConfidence == FALSE) {
    confFlagMark <- NULL;
  }

  lines <- paste0(time, confFlagMark, chunkText);

  #removespace
  lines <- gsub("\\s+", " ", lines)

  if(is.null(output)){
    ext <- tools::file_ext(input);
    input <- gsub(paste0("\\.", ext), "", input);
    output <- paste0(dirname(input), "/", basename(input), "_cleaned.", ext);
  }
  #consider preventing overwriting behaviour

  writeLines(lines, output);

}
