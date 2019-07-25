#' @export
extract_codings_by_coderId <- function(input,
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
                      utils::tail(names(x), -1);
                    logicalCodings <-
                      as.logical(as.numeric(utils::tail(x, -1)));
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

        sectionMatches <- list();

        for (j in sectionMatchCols) {
          sectionMatches[[j]] <-
            list(index = which(sourceDf[, j]));
          sectionMatches[[j]]$uid_pre <-
            unlist(lapply(sectionMatches[[j]]$index-1,
                          function(i) {
                            if (i < 1) {
                              return(NA);
                            } else {
                              return(sourceDf[
                                max(which(nchar(sourceDf[1:i, 'uids']) > 0)),
                                'uids']);
                            }
                          }));
          sectionMatches[[j]]$uid_at <-
            sourceDf[sectionMatches[[j]]$index, "uids"];
          sectionMatches[[j]]$uid_post <-
            unlist(lapply(sectionMatches[[j]]$index+1,
                          function(i) {
                            if (i > nrow(sourceDf)) {
                              return(NA);
                            } else {
                              return(sourceDf[
                                ### Correct for index starting at i (i starts at next row)
                                i-1 +
                                  min(which(nchar(sourceDf[i:nrow(sourceDf), 'uids']) > 0)),
                                'uids']);
                            }
                          }));
        }

        if (!(filename %in% names(res$sectionBreaksByCoder))) {
          res$sectionBreaksByCoder[[filename]] <- list();
        }

        res$sectionBreaksByCoder[[filename]][[coderId]] <-
          sectionMatches;

      } ### End if section that's only run if 'uids' exists in the sourceDf
    }
  }

  res$utterances <- list();

  for (i in names(res$codingsByCoder)) {
    for (j in names(res$codingsByCoder[[i]])) {
      utterancesInSource <-
        names(res$codingsByCoder[[i]][[j]]);
      codedUtterances <-
        which(unlist(lapply(res$codingsByCoder[[i]][[j]],
                            length)) > 0);
      for (k in utterancesInSource[codedUtterances]) {
        ### Loop through all coded utterances by this coder in this source

        ### Get original codings from original source
        originalSourceLine <-
          parsedSources[[i]]$parsedSubsources[[j]]$rawSourceDf[
            parsedSources[[i]]$parsedSubsources[[j]]$rawSourceDf$uids == k,
            'utterances_raw'];
        rawCodings <-
          regmatches(originalSourceLine,
                     gregexpr(codeRegexes[codeRegex], originalSourceLine));
        codingInfo <-
          stats::setNames(rawCodings, #list(res$codingsByCoder[[i]][[j]][[k]]),
                          i);


        if (k %in% res$utterances) {
          ### If this uid already contains information, append the new info
          if (j %in% names(res$utterances[[k]])) {
            res$utterances[[k]][[j]] <-
              c(res$utterances[[k]][[j]],
                codingInfo);
          } else {
            res$utterances[[k]][[j]] <-
              codingInfo;
          }
        } else {
          ### No coding information about this utterance has been added yet
          res$utterances[[k]] <-
            stats::setNames(list(codingInfo),
                            j);
        }
      }
    }
  }




  return(res);

}
