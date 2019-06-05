#' @rdname parsing_sources
#' @export
parse_sources <- function(path,
                          extension = "rock|dct",
                          regex,
                          recursive=TRUE,
                          codeRegexes = c(code = "\\[\\[([a-zA-Z0-9._>-]+)\\]\\]"),
                          idRegexes = c(caseId = "\\[\\[cid=([a-zA-Z0-9._-]+)\\]\\]",
                                        stanzaId = "\\[\\[sid=([a-zA-Z0-9._-]+)\\]\\]"),
                          sectionRegexes = c(paragraphs = "---paragraph-break---",
                                             secondary = "---<[a-zA-Z0-9]?>---"),
                          autoGenerateIds = c('stanzaId'),
                          persistentIds = c('caseId'),
                          noCodes = "^uid:|^dct:|^ci:",
                          inductiveCodingHierarchyMarker = ">",
                          metadataContainers = c("metadata"),
                          codesContainers = c("codes", "dct"),
                          delimiterRegEx = "^---$",
                          ignoreRegex = "^#",
                          ignoreOddDelimiters = FALSE,
                          encoding="UTF-8",
                          silent=TRUE) {

  if (!dir.exists(path)) {
    stop("Directory '",
         path,
         "' does not exist!");
  }

  if (missing(regex)) {
    if (grep("|", extension, fixed=TRUE)) {
      regex <- paste0("^(.*)\\.",
                      strsplit(extension,
                               "|",
                               fixed=TRUE)[[1]],
                      "$");
      regex <- paste0("^(.*)\\.", extension, "$",
                      collapse="|");
    } else {
      regex <- paste0("^(.*)\\.", extension, "$");
    }
  }

  fileList <-
    list.files(path=path,
               pattern=regex,
               recursive=recursive,
               full.names=TRUE);

  res <- list(input=as.list(environment()));

  res$parsedSources <-
    lapply(fileList,
           parse_source,
           codeRegexes=codeRegexes,
           idRegexes=idRegexes,
           sectionRegexes=sectionRegexes,
           autoGenerateIds=autoGenerateIds,
           persistentIds=persistentIds,
           noCodes=noCodes,
           inductiveCodingHierarchyMarker=inductiveCodingHierarchyMarker,
           metadataContainers=metadataContainers,
           codesContainers=codesContainers,
           delimiterRegEx = delimiterRegEx,
           ignoreRegex = ignoreRegex,
           ignoreOddDelimiters = ignoreOddDelimiters,
           encoding=encoding,
           postponeDeductiveTreeBuilding = TRUE,
           silent=silent);

  names(res$parsedSources) <-
    basename(fileList);

  res$sourcesDf <-
    dplyr::bind_rows(purrr::map(res$parsedSources,
                                'sourceDf'));

  ###--------------------------------------------------------------------------
  ### Now look in the returned objects for generic information and structure
  ### the result better
  ###--------------------------------------------------------------------------

  ### Pre-yum bit; keeping it for now just in case

  # yamlLineSets <-
  #   purrr::map(res$parsedSources,
  #              'yamlFragments');
  #
  # yamlLineSets <-
  #   unlist(yamlLineSets,
  #          recursive = FALSE);
  #
  # yamlLineSets <-
  #   lapply(yamlLineSets,
  #          paste,
  #          collapse="\n");
  #
  # if (!silent) {
  #   ufs::cat0("Extracted the following YAML fragments:\n\n",
  #             paste0(unlist(yamlLineSets),
  #                    collapse="\n\n"));
  # }
  #
  # rawSpecs <-
  #   res$rawSpecs <-
  #   yum::load_yaml_list(yamlLineSets);
  #
  # if (!silent) {
  #   print(glue::glue("\n\nLoaded {length(rawSpecs)} raw metadata specifications.\n"));
  # }
  #
  # ### Get the metadata
  # metadataList <- list();
  # for (currentMetadataContainer in metadataContainers) {
  #   metadataList <-
  #     c(metadataList,
  #       unlist(purrr::map(rawSpecs,
  #                         currentMetadataContainer),
  #              recursive=FALSE));
  # }
  #
  # ### Add type and convert to data frame
  # metadataDfs <-
  #   lapply(metadataList,
  #          function(x) {
  #            x$type <-
  #              names(idRegexes)[names(idRegexes) %in% names(x)];
  #            return(as.data.frame(x,
  #                                 stringsAsFactors=FALSE));
  #          });
  #
  # ### Bind together into one dataframe
  # res$metadata <-
  #   metadataDf <-
  #   dplyr::bind_rows(metadataDfs);

  metadataDf <-
    res$metadataDf <-
    dplyr::bind_rows(lapply(purrr::map(res$parsedSources,
                                       'metadata'),
                            as.data.frame,
                            stringsAsFactors=FALSE));

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

  deductiveCodeLists <-
    do.call(c,
            purrr::map(res$parsedSources,
                       'deductiveCodes'));
    # yum::load_yaml_list(yamlLineSets,
    #                     select=paste0(codesContainers, sep="|"));

  if (is.null(deductiveCodeLists)) {
    res$deductiveCodeTrees <- NULL;
  } else {

    class(deductiveCodeLists) <-
      "simplifiedYum";

    res$deductiveCodeTrees <-
      yum::build_tree(deductiveCodeLists);

    res$deductiveCodeTrees$root$Set(name = 'codes',
                                   filterFun=function(x) x$isRoot);
    res$deductiveCodeTreeGraph <-
      data.tree::ToDiagrammeRGraph(res$deductiveCodeTrees);

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
  }

  # ### Get the codes
  # deductiveCodeLists <- list();
  # for (currentCodesContainer in codesContainers) {
  #   deductiveCodeLists[[currentCodesContainer]] <-
  #     purrr::map(rawSpecs,
  #                1,
  #                currentCodesContainer);
  #   if (length(deductiveCodeLists[[currentCodesContainer]]) > 0) {
  #     deductiveCodeLists[[currentCodesContainer]] <-
  #       deductiveCodeLists[[currentCodesContainer]][
  #         !unlist(lapply(deductiveCodeLists[[currentCodesContainer]],
  #                        is.null))
  #       ];
  #   }
  #   print(length(deductiveCodeLists[[currentCodesContainer]]))
  # }
  # res$deductiveCodeList <-
  #   deductiveCodeList <-
  #   do.call(c,
  #           deductiveCodeLists);
  #
  # if (length(res$deductiveCodeList) > 0) {
  #   res$deductiveCodeTree <-
  #     codes_to_nodes(res$deductiveCodeList,
  #                    silent=silent);
  #   res$deductiveCodeTree$root$Set(name = 'codes',
  #                                  filterFun=function(x) x$isRoot);
  #   res$deductiveCodeTreeGraph <-
  #     data.tree::ToDiagrammeRGraph(res$deductiveCodeTree);
  #   res$deductiveCodeTreeGraph <-
  #     apply_graph_theme(res$deductiveCodeTreeGraph,
  #                       c("layout", "dot", "graph"),
  #                       c("rankdir", "LR", "graph"),
  #                       c("outputorder", "nodesfirst", "graph"),
  #                       c("fixedsize", "false", "node"),
  #                       c("shape", "box", "node"),
  #                       c("style", "rounded,filled", "node"),
  #                       c("color", "#000000", "node"),
  #                       c("color", "#888888", "edge"),
  #                       c("dir", "none", "edge"),
  #                       c("fillcolor", "#FFFFFF", "node"));
  #
  # } else {
  #   res$deductiveCodeTree <- NULL;
  #   res$deductiveCodeTreeGraph <- NULL;
  # }

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
  print(graphics::plot(x));
  invisible(x);
}

#' @rdname parsing_sources
#' @method plot rockParsedSources
#' @export
plot.rockParsedSources <- function(x, ...) {
  if (!is.null(x$deductiveCodeTreeGraph)) {
    return(DiagrammeR::render_graph(x$deductiveCodeTreeGraph));
  } else {
    return(glue::glue("\nThese parsed sources do not contain a deductive code tree.\n"));
  }
}
