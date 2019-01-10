#' @rdname parsing_sources
#' @export
parse_sources <- function(path,
                          extension = "rock|dct",
                          regex,
                          codeRegexes = c(code = "\\[\\[([a-zA-Z0-9._>-]+)\\]\\]"),
                          idRegexes = c(caseId = "\\[\\[cid=([a-zA-Z0-9._-]+)\\]\\]",
                                        stanzaId = "\\[\\[sid=([a-zA-Z0-9._-]+)\\]\\]"),
                          autoGenerateIds = c('stanzaId'),
                          sectionRegexes = c(paragraphs = "---paragraph-break---",
                                             secondary = "---<[a-zA-Z0-9]?>---"),
                          inductiveCodingHierarchyMarker = ">",
                          delimiterRegEx = "^---$",
                          metadataContainers = c("metadata"),
                          codesContainers = c("codes", "dct"),
                          ignoreRegex = "^#",
                          ignoreOddDelimiters = FALSE,
                          encoding="UTF-8",
                          silent=TRUE) {

  if (missing(regex)) {
    regex <- paste0("^(.*)\\.", extension, "$");
  }

  filelist <- list.files(path,
                         pattern=regex,
                         full.names=FALSE);

  sourceFileList <- lapply(file.path(path, filelist),
                           readLines,
                           encoding=encoding);

  res <- list(input=as.list(environment()));

  res$parsedSources <-
    lapply(sourceFileList,
           parse_source,
           codeRegexes=codeRegexes,
           idRegexes=idRegexes,
           autoGenerateIds=autoGenerateIds,
           sectionRegexes=sectionRegexes,
           delimiterRegEx = delimiterRegEx,
           ignoreRegex = ignoreRegex,
           ignoreOddDelimiters = ignoreOddDelimiters,
           silent=silent);

  names(res$parsedSources) <-
    filelist;

  res$sourcesDf <-
    dplyr::bind_rows(purrr::map(res$parsedSources,
                                'sourceDf'));

  ###--------------------------------------------------------------------------
  ### Now look in the returned objects for generic information and structure
  ### the result better
  ###--------------------------------------------------------------------------

  yamlLineSets <-
    purrr::map(res$parsedSources,
               'yamlFragments');

  yamlLineSets <-
    unlist(yamlLineSets,
           recursive = FALSE);

  yamlLineSets <-
    lapply(yamlLineSets,
           paste,
           collapse="\n");

  if (!silent) {
    ufs::cat0("Extracted the following YAML fragments:\n\n",
              paste0(unlist(yamlLineSets),
                     collapse="\n\n"));
  }

  rawSpecs <-
    res$rawSpecs <-
    lapply(yamlLineSets,
           yaml::yaml.load);

  if (!silent) {
    print(glue::glue("\n\nLoaded {length(rawSpecs)} raw metadata specifications.\n"));
  }

  ### Get the metadata
  metadataList <- list();
  for (currentMetadataContainer in metadataContainers) {
    metadataList <-
      c(metadataList,
        unlist(purrr::map(rawSpecs, 'metadata'),
               recursive=FALSE));
  }

  ### Add type and convert to data frame
  metadataDfs <-
    lapply(metadataList,
           function(x) {
             x$type <-
               names(idRegexes)[names(idRegexes) %in% names(x)];
             return(as.data.frame(x,
                                  stringsAsFactors=FALSE));
           });

  ### Bind together into one dataframe
  res$metadata <-
    metadataDf <-
    dplyr::bind_rows(metadataDfs);

  ### Add metadata to the utterances
  for (i in seq_along(idRegexes)) {
    ### Check whether metadata was provided for this identifier
    if (names(idRegexes)[i] %in% names(metadataDf)) {
      if (!silent) {
        print(glue::glue("\n\nFor identifier class {names(idRegexes)[i]}, metadata was provided: proceeding to join to sources dataframe.\n"));
      }
      ### Convert to character to avoid errors
      metadataDf[, names(idRegexes)[i]] <-
        as.character(metadataDf[, names(idRegexes)[i]]);
      ### Join metadata based on identifier
      res$sourcesDf <-
        dplyr::left_join(res$sourcesDf,
                         metadataDf[, setdiff(names(metadataDf), 'type')],
                         by=names(idRegexes)[i]);
    } else {
      if (!silent) {
        print(glue::glue("\n\nFor identifier class {names(idRegexes)[i]}, no metadata was provided.\n"));
      }
    }
  }

  ### Get the codes
  deductiveCodeLists <- list();
  for (currentCodesContainer in codesContainers) {
    deductiveCodeLists[[currentCodesContainer]] <-
      purrr::map(rawSpecs, currentCodesContainer);
    if (length(deductiveCodeLists[[currentCodesContainer]]) > 0) {
      deductiveCodeLists[[currentCodesContainer]] <-
        deductiveCodeLists[[currentCodesContainer]][
          !unlist(lapply(deductiveCodeLists[[currentCodesContainer]],
                         is.null))
        ];
    }
  }
  res$deductiveCodeList <-
    deductiveCodeList <-
    do.call(c,
            deductiveCodeLists);

  if (length(res$deductiveCodeList) > 0) {
    res$deductiveCodeTree <-
      codes_to_nodes(res$deductiveCodeList,
                     silent=silent);
    res$deductiveCodeTree$root$Set(name = 'codes',
                                   filterFun=function(x) x$isRoot);
    res$deductiveCodeTreeGraph <-
      data.tree::ToDiagrammeRGraph(res$deductiveCodeTree);
    res$deductiveCodeTreeGraph <-
      apply_graph_theme(res$deductiveCodeTreeGraph,
                        c("layout", "dot", "graph"),
                        c("rankdir", "LR", "graph"),
                        c("outputorder", "nodesfirst", "graph"),
                        c("fixedsize", "false", "node"),
                        c("shape", "box", "node"),
                        c("style", "rounded,filled", "node"),
                        c("color", "#000000", "node"),
                        c("color", "#888888", "edge"),
                        c("dir", "none", "edge"),
                        c("fillcolor", "#FFFFFF", "node"));

  } else {
    res$deductiveCodeTree <- NULL;
    res$deductiveCodeTreeGraph <- NULL;
  }

  return(structure(res,
                   class="rockParsedSources"));

}

#' @rdname parsing_sources
#' @method print rockParsedSources
#' @export
print.rockParsedSources <- function(x, prefix="### ",  ...) {
  sourceFileNames <- names(x$parsedSources);
  print(glue::glue("Parsed {length(sourceFileNames)} sources, with filenames ",
                   "{ufs::vecTxtQ(sourceFileNames)}."));
  print(plot(x));
}

#' @rdname parsing_sources
#' @method plot rockParsedSources
#' @export
plot.rockParsedSources <- function(x, ...) {
  if (!is.null(x$deductiveCodeTreeGraph)) {
    DiagrammeR::render_graph(x$deductiveCodeTreeGraph);
  } else {
    cat("\nThese parsed sources do not contain a deductive code tree.\n\n");
  }
}
