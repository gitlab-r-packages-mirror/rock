cleaned_source_to_utterance_vector <- function(x,
                                               utteranceMarker = rock::opts$get("utteranceMarker"),
                                               fixed = FALSE,
                                               perl = TRUE) {
  x <- paste(x,
             collapse="\n");
  res <-
    strsplit(x,
             split = utteranceMarker,
             fixed = fixed,
             perl = perl)[[1]];
  return(res);
}
