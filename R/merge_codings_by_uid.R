#' @export
merge_codings_by_uid <- function(input,
                                 coderId = "\\[\\[coderId=([a-zA-Z0-9._-]+)\\]\\]",
                                 idForOmittedCoderIds = "noCoderId",
                                 codeRegexes = c(codes = "\\[\\[([a-zA-Z0-9._>-]+)\\]\\]"),
                                 idRegexes = c(caseId = "\\[\\[cid=([a-zA-Z0-9._-]+)\\]\\]",
                                               stanzaId = "\\[\\[sid=([a-zA-Z0-9._-]+)\\]\\]",
                                               coderId = "\\[\\[coderId=([a-zA-Z0-9._-]+)\\]\\]"),
                                 sectionRegexes = c(paragraphs = "---paragraph-break---",
                                                    secondary = "---<[a-zA-Z0-9]?>---"),
                                 uidRegex = "\\[\\[uid=([a-zA-Z0-9._-]+)\\]\\]",
                                 autoGenerateIds = c('stanzaId'),
                                 persistentIds = c('caseId', 'coderId'),
                                 noCodes = "^uid:|^uid=|^dct:|^ci:",
                                 recursive = TRUE,
                                 filenameRegex = ".*",
                                 delimiterRegEx = "^---$",
                                 ignoreRegex = "^#",
                                 ignoreOddDelimiters=FALSE,
                                 postponeDeductiveTreeBuilding = TRUE,
                                 encoding="UTF-8",
                                 silent=TRUE) {

  ### First parse all sources by coderId
  parsedSources <-
    do.call(parse_sources_by_coderId,
            as.list(environment()));

  sectionMatchCols <- paste0(names(sectionRegexes), "_match");
  idNames <- names(idRegexes);

  res <- list(parsedSources = parsedSources,
              codingsByCoder = list(),
              utterances = list());

  ### Construct objects with codes for each utterance
  for (filename in names(parsedSources)) {
    for (coderId in names(parsedSources[[filename]]$parsedSubsources)) {
      if ('uids' %in% names(parsedSources[[filename]]$parsedSubsources[[coderId]]$sourceDf)) {
        ### For convenience, store some stuff
        sourceDf <-
          parsedSources[[filename]]$parsedSubsources[[coderId]]$sourceDf;
        codings <-
          parsedSources[[filename]]$parsedSubsources[[coderId]]$codings;
        if (length(codings) == 0) {
          codingsList <- NA;
        } else {
          codingsList <-
            apply(sourceDf[, c('uids', codings)],
                  1,
                  function(x) {
                    codingNames <-
                      tail(names(x), -1);
                    logicalCodings <-
                      as.logical(as.numeric(tail(x, -1)));
                    return(codingNames[logicalCodings]);
                  });

          names(codingsList) <-
            sourceDf$uids;
        }
        if (!(filename %in% names(res$codingsByCoder))) {
          res$codingsByCoder[[filename]] <- list();
        }
        res$codingsByCoder[[filename]][[coderId]] <-
          codingsList;

        for (i in 1:nrow(parsedSources[[filename]]$parsedSubsources[[coderId]]$sourceDf)) {

          ### First store the utterance id of this row, of the last row, and of the next row
          uid_prev <-
            parsedSources[[filename]]$parsedSubsources[[coderId]]$sourceDf[
              max(which((nchar(parsedSources[[filename]]$parsedSubsources[[coderId]]$sourceDf[1:i, 'uid']) > 0))),
              'uid'];
          uid_current <-
            parsedSources[[filename]]$parsedSubsources[[coderId]]$sourceDf[i, 'uid'];
          uid_next <-
            parsedSources[[filename]]$parsedSubsources[[coderId]]$sourceDf[
              min(which((nchar(parsedSources[[filename]]$parsedSubsources[[coderId]]$sourceDf[i:nrow(parsedSources[[filename]]$parsedSubsources[[coderId]]$sourceDf), 'uid']) > 0))),
              'uid'];

          ### Check for matches with section breaks
          for (j in sectionMatchCols) {
            if (parsedSources[[filename]]$parsedSubsources[[coderId]]$sourceDf[i, j]) {
              ### We have a match with this section break
              if (i == nrow(parsedSources[[filename]]$parsedSubsources[[coderId]]$sourceDf)) {
                ### This is the final row; use last row with utterance id
                uid_use <- uid_prev;
                store_type <- "sectionBreaksAfter";
              } else {
                ### This is not the final row, so use the uid of the next row that has one
                uid_use <- uid_next;
                store_type <- "sectionBreaksBefore";
              }
              if (uid_use %in% names(utterances)) {
                ### Already exists
                res$utterances[[uid_use]][[store_type]] <-
                  c(res$utterances[[uid_use]][[store_type]],
                    j);
              } else {
                ### Didn't exist yet
                res$utterances[[uid_use]] <-
                  stats::setNames(list(j),
                                  store_type);
              }
            }
          } ### End checking for matching sections
        } ### End of for loop processing each dataframe row

      } ### End if section that's only run if 'uids' exists in the sourceDf
    }
  }






  return(res);

}
