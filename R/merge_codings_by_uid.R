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
              sectionBreaksByCoder = list());

  ### Construct objects with codes for each utterance
  for (filename in names(parsedSources)) {
    for (coderId in names(parsedSources[[filename]]$parsedSubsources)) {
      if ('uids' %in% names(parsedSources[[filename]]$parsedSubsources[[coderId]]$rawSourceDf)) {
        ### For convenience, store some stuff
        ### Note that we use 'rawSourceDf' since that still has lines with section breaks
        sourceDf <-
          parsedSources[[filename]]$parsedSubsources[[coderId]]$rawSourceDf;
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

        sectionMatchIndices <- list();

        for (j in sectionMatchCols) {
          sectionMatchIndices[[j]] <-
            which(sourceDf[, j]);
          sectionMatch_uids_at[[j]] <-
            sourceDf[sectionMatchIndices[[j]], "uids"];
          sectionMatch_uids_pre[[j]] <-
            unlist(lapply(sectionMatchIndices[[j]]-1,
                          function(i) {
                            if (i < 1) {
                              return(NA);
                            } else {
                              return(sourceDf[
                                max(which(nchar(sourceDf[1:i, 'uids']) > 0)),
                                'uids']);
                            }
                          }));
          sectionMatch_uids_post[[j]] <-
            unlist(lapply(sectionMatchIndices[[j]]+1,
                          function(i) {
                            if (i > nrow(sourceDf)) {
                              return(NA);
                            } else {
                              return(sourceDf[
                                min(which(nchar(sourceDf[i:nrow(sourceDf), 'uids']) > 0)),
                                'uids']);
                            }
                          }));
        }

        if (!(filename %in% names(res$sectionBreaksByCoder))) {
          res$sectionBreaksByCoder[[filename]] <- list();
        }

        res$sectionBreaksByCoder[[filename]][[coderId]] <-
          list(sectionMatchIndices = sectionMatchIndices,
               sectionMatch_uids_pre = sectionMatch_uids_pre,
               sectionMatch_uids_at = sectionMatch_uids_at,
               sectionMatch_uids_post = sectionMatch_uids_post);
#
#         for (i in 1:nrow(sourceDf)) {
#
#           ### First store the utterance id of this row, of the last row, and of the next row
          # uid_prev <-
          #   sourceDf[
          #     max(which((nchar(sourceDf[1:i, 'uids']) > 0))),
          #     'uids'];
          # uid_current <-
          #   sourceDf[i, 'uids'];
          # uid_next <-
          #   sourceDf[min(which((nchar(sourceDf[i:nrow(sourceDf), 'uids']) > 0))),
          #            'uids'];
#
#           print(sectionMatchCols);
#
#           # ### Check for matches with section breaks
#           # for (j in sectionMatchCols) {
#           #   if (sourceDf[i, j]) {
#           #
#           #     print(paste0("Match!"));
#           #
#           #     ### We have a match with this section break
#           #     if (i == nrow(sourceDf)) {
#           #       ### This is the final row; use last row with utterance id
#           #       uid_use <- uid_prev;
#           #       store_type <- "sectionBreaksAfter";
#           #     } else {
#           #       ### This is not the final row, so use the uid of the next row that has one
#           #       uid_use <- uid_next;
#           #       store_type <- "sectionBreaksBefore";
#           #     }
#           #     if (uid_use %in% names(utterances)) {
#           #       ### Already exists
#           #       res$utterances[[uid_use]][[store_type]] <-
#           #         c(res$utterances[[uid_use]][[store_type]],
#           #           j);
#           #     } else {
#           #       ### Didn't exist yet
#           #       res$utterances[[uid_use]] <-
#           #         stats::setNames(list(j),
#           #                         store_type);
#           #     }
#           #   }
#           # } ### End checking for matching sections
#
#
#         } ### End of for loop processing each dataframe row
      } ### End if section that's only run if 'uids' exists in the sourceDf
    }
  }






  return(res);

}
