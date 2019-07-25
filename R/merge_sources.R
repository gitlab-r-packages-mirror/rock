#' @export
merge_sources <- function(input,
                          output,
                          outputPrefix = "",
                          outputSuffix = "_merged",
                          primarySourcesRegex=".*",
                          primarySourcesPath = input,
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
                          primarySourcesRecursive = recursive,
                          filenameRegex = ".*",
                          delimiterRegEx = "^---$",
                          ignoreRegex = "^#",
                          overwrite = FALSE,
                          ignoreOddDelimiters=FALSE,
                          postponeDeductiveTreeBuilding = TRUE,
                          encoding="UTF-8",
                          silent=TRUE) {

  if (!dir.exists(primarySourcesPath)) {
    stop("Directory specified to read primary sources from (",
         primarySourcesPath, ")does not exist!");
  }

  ### Store all arguments and delete the ones specific to this function
  args <- as.list(environment());
  args <-
    args[setdiff(names(args),
                 c('primarySourcesRegex',
                   'primarySourcesPath',
                   'primarySourcesRecursive',
                   'output',
                   'outputPrefix',
                   'outputSuffix',
                   'overwrite'))];

  ### Then pass arguments along to extract_codings_by_coderId and store result
  parsedSources <-
    do.call(extract_codings_by_coderId,
            args);

  allCodedUtterances <-
    names(parsedSources$utterances);

  ### Read primary sources
  primarySources <-
    load_sources(input = primarySourcesPath,
                 encoding=encoding,
                 filenameRegex=primarySourcesRegex,
                 recursive=primarySourcesRecursive,
                 full.names=TRUE,
                 silent=silent);

  if (!(tolower(output) == "same")) {
    if (!dir.exists(output)) {
      warning("Directory provided to write to ('",
              output,
              "') does not exist - creating it!");
      dir.create(output,
                 recursive = TRUE);
    }
  }

  mergedSources <- list();
  primarySourceUids <- list();
  for (i in names(primarySources)) {

    primarySourceUids[[i]] <-
      ifelse(grepl(uidRegex,
                   primarySources[[i]],
                   perl=TRUE),
             gsub(paste0(".*", uidRegex, ".*"),
                  "\\1",
                  primarySources[[i]]),
             "");

    mergedSources[[i]] <- primarySources[[i]];

    ### This way, 'j' is both the index for the UID vector and
    ### for the corresponding line in the sources
    for (j in seq_along(primarySourceUids[[i]])) {

      currentUID <- primarySourceUids[[i]][j];

      if (currentUID %in% allCodedUtterances) {
        ### Check whether one of them is already applied
        codings <-
          unname(unlist(parsedSources$utterances[[currentUID]]));
        alreadyAppliedCodings <-
          unlist(lapply(codings,
                        grepl,
                        x = mergedSources[[i]][j]));
        ### Add new codes
        mergedSources[[i]][j] <-
          paste0(mergedSources[[i]][j], " ",
                 paste0(codings[!alreadyAppliedCodings],
                        collapse = " "));

      }

      newFilename <-
        paste0(outputPrefix,
               sub("^(.*)\\.[a-zA-Z0-9]+$",
                   "\\1",
                   basename(i)),
               outputSuffix,
               ".rock");
      if (tolower(output) == "same") {
        newFileDir <-
          dirname(i);
      } else {
        newFileDir <-
          output;
      }
      newFullname <- file.path(newFileDir,
                               newFilename);

      if (file.exists(newFullname) && (!overwrite)) {
        if (!silent) {
          message("Output file '", newFilename, "' already exists and ",
                  "`overwrite` is set to FALSE - not writing output file!");
        }
      } else {
        con <- file(description=,
                    open="w",
                    encoding=encoding);
        writeLines(text=mergedSources[[i]],
                   con=con);
        close(con);
      }

    }
  }

  res <- list(parsedSources = parsedSources,
              primarySources = primarySources,
              mergedSources = mergedSources);

  return(invisible(res));

}
