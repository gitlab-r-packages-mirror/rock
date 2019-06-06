inductiveCodes_to_tree <- function(inductiveCodes,
                                   codeRegex,
                                   inductiveCodingHierarchyMarker,
                                   silent=TRUE) {

  inductiveCodeProcessing <- list();
  inductiveCodeTrees <- list();
  codingLeaves <- list();
  codings <- list(inductiveCodes);
  names(codings) <- codeRegex;
  inductiveDiagrammeR <- list();

  ### Split these unique codes into levels in case inductive coding
  ### was applied
  if ((nchar(inductiveCodingHierarchyMarker) > 0) &&
      (!is.null(codings[[codeRegex]])) &&
      (length(codings[[codeRegex]]) > 0)) {

    inductiveCodeProcessing[[codeRegex]] <- list();

    inductiveCodeProcessing[[codeRegex]]$splitCodings <-
      strsplit(codings[[codeRegex]],
               inductiveCodingHierarchyMarker);

    inductiveCodeProcessing[[codeRegex]]$inductiveLeaves <-
      unlist(lapply(inductiveCodeProcessing[[codeRegex]]$splitCodings,
                    utils::tail,
                    1));
  } else {
    inductiveCodeProcessing[[codeRegex]]$inductiveLeaves <-
      codings[[codeRegex]];
  }

  ### Remove duplicate elements from the list with leaves
  ### and store for later
  codingLeaves[[codeRegex]] <-
    unique(inductiveCodeProcessing[[codeRegex]]$inductiveLeaves);

  ###---------------------------------------------------------------------------
  ### Build the inductive coding tree
  ###---------------------------------------------------------------------------

  ### If inductive coding was applied using a hierarchical structure,
  ### build the inductive code tree
  if ((nchar(inductiveCodingHierarchyMarker) > 0) &&
      (!is.null(inductiveCodeProcessing[[codeRegex]]$inductiveLeaves)) &&
      (length(inductiveCodeProcessing[[codeRegex]]$inductiveLeaves) > 0)) {

    ### Build tree for this code regex. First some preparation.
    inductiveCodeProcessing[[codeRegex]]$localRootsFull <-
      unlist(lapply(inductiveCodeProcessing[[codeRegex]]$splitCodings,
                    utils::head, 1));
    inductiveCodeProcessing[[codeRegex]]$localBranchesFull <-
      unlist(lapply(inductiveCodeProcessing[[codeRegex]]$splitCodings,
                    utils::tail, -1));

    ### Remove duplicates (no longer maps on 'splitCodings')
    inductiveCodeProcessing[[codeRegex]]$localRoots <-
      unique(inductiveCodeProcessing[[codeRegex]]$localRootsFull);
    inductiveCodeProcessing[[codeRegex]]$localBranches <-
      unique(inductiveCodeProcessing[[codeRegex]]$localBranchesFull);

    ### Check whether any local roots are actually branches; store both
    ### a logical vector that maps on localRootsFull (and therefore, on
    ### splitcodings) and a vector with just the names
    inductiveCodeProcessing[[codeRegex]]$localRootsThatAreBranchesFullLogical <-
      unlist(lapply(inductiveCodeProcessing[[codeRegex]]$localRootsFull,
                    `%in%`,
                    inductiveCodeProcessing[[codeRegex]]$localBranches));

    inductiveCodeProcessing[[codeRegex]]$localRootsThatAreBranchesFull <-
      unique(inductiveCodeProcessing[[codeRegex]]$localRootsFull[
        inductiveCodeProcessing[[codeRegex]]$localRootsThatAreBranchesFullLogical
        ]);

    ### Also store the local 'true roots' for convenience, where the logical
    ### vectors again maps on splitCodings
    inductiveCodeProcessing[[codeRegex]]$localRootsThatAreTrueRootsFullLogical <-
      !inductiveCodeProcessing[[codeRegex]]$localRootsThatAreBranchesFullLogical
    inductiveCodeProcessing[[codeRegex]]$localRootsThatAreTrueRootsFull <-
      unique(inductiveCodeProcessing[[codeRegex]]$localRootsFull[
        inductiveCodeProcessing[[codeRegex]]$localRootsThatAreTrueRootsFullLogical
        ]);

    ### Convert split codings into node-ready lists
    inductiveCodeProcessing[[codeRegex]]$subTrees <-
      lapply(inductiveCodeProcessing[[codeRegex]]$splitCodings,
             function(subTree) {
               return(lapply(subTree,
                             function(x) {
                               stats::setNames(list(x,x,x),
                                               c('idName',
                                                 'labelName',
                                                 'codeName'));
                             }));
             });

    ### Local roots that are not branches should be attached to the root of
    ### the inductive code tree for this code set, along with their children.
    inductiveCodeTrees[[codeRegex]] <-
      data.tree::Node$new('codes');

    if (!silent) {
      ufs::cat0("\nBuilding tree containing all 'true local roots'.");
    }

    ### First add only the local roots that have no parents
    for (currentLocalRoot in
         inductiveCodeProcessing[[codeRegex]]$localRootsThatAreTrueRootsFull) {

      if (!silent) {
        ufs::cat0("\n- Processing 'true local root' '",
                  currentLocalRoot,
                  "'.");
      }

      ### Add first node to the root
      inductiveCodeTrees[[codeRegex]]$AddChild(currentLocalRoot);
      inductiveCodeTrees[[codeRegex]][[currentLocalRoot]]$label <-
        currentLocalRoot;
      inductiveCodeTrees[[codeRegex]][[currentLocalRoot]]$code <-
        currentLocalRoot;
    }

    if (!silent) {
      ufs::cat0("\n\nProcessing subtrees of those 'true local roots'.");
    }

    ### Then process their branches/children
    for (currentSubtree in
         inductiveCodeProcessing[[codeRegex]]$splitCodings[
           inductiveCodeProcessing[[codeRegex]]$localRootsThatAreTrueRootsFullLogical
           ]) {
      if (!silent) {
        ufs::cat0("\n\n- Processing subtree consisting of the node sequence ",
                  ufs::vecTxtQ(currentSubtree),
                  ".");
      }
      if (length(currentSubtree) > 1) {
        ### Add children; first save reference to this node
        currentNode <-
          inductiveCodeTrees[[codeRegex]][[currentSubtree[1]]];
        if (!silent) {
          ufs::cat0("\n  - This subtree doesn't only consist of the parent/root code, but contains ",
                    length(currentSubtree)-1,
                    " child(ren). Processing child(ren).");
        }
        ### Then loop through children and progressively add them
        for (currentBranch in currentSubtree[2:length(currentSubtree)]) {
          if (!silent) {
            ufs::cat0("\n    - Processing node '",
                      currentBranch,
                      "'.");
          }

          if (is.null(currentNode[[currentBranch]])) {
            if (!silent) {
              ufs::cat0("\n      - This parent node does not yet have a child with the name '",
                        currentBranch,
                        "', so adding it to that parent node.");
            }
            currentNode <-
              currentNode$AddChild(currentBranch);
            currentNode$label <-
              currentBranch;
            currentNode$code <-
              currentBranch;
          } else {
            if (!silent) {
              ufs::cat0("\n      - This parent node already has a child with the name '",
                        currentBranch,
                        "', so not adding anything at this point.");
            }
            currentNode <-
              currentNode[[currentBranch]];
          }
        }
      } else {
        if (!silent) {
          ufs::cat0("\n  - This 'subtree' only consists of the parent/root code, ",
                    "so no further processing required.");
        }
      }
    }

    ### Then start working on the subtrees that should be attached to
    ### a parent

    if (!silent) {
      ufs::cat0("\n\nProcessing subtrees of 'local roots that are branches', i.e. single codes ",
                "that are descendants of other codes (without the full path to the root being ",
                "specified in the code), or subtrees where the local root is in fact a descendant.");
    }

    for (i in seq_along(inductiveCodeProcessing[[codeRegex]]$splitCodings[
      inductiveCodeProcessing[[codeRegex]]$localRootsThatAreBranchesFullLogical
      ])) {
      currentSubtree <-
        inductiveCodeProcessing[[codeRegex]]$splitCodings[
          inductiveCodeProcessing[[codeRegex]]$localRootsThatAreBranchesFullLogical
          ][[i]];

      if (!silent) {
        ufs::cat0("\n\n- Processing subtree consisting of the node sequence ",
                  ufs::vecTxtQ(currentSubtree),
                  ".");
      }

      if (length(currentSubtree) == 1) {

        if (!silent) {
          ufs::cat0("\n  - This 'subtree' only consists of the parent/root code, ",
                    "so no further processing required.");
        }

      } else {

        currentNode <-
          data.tree::FindNode(inductiveCodeTrees[[codeRegex]],
                              currentSubtree[1]);

        if (is.null(currentNode)) {
          ### Code not found - should normally not be possible

          if (!silent) {
            ufs::cat0("\n  - I cannot find the local root of this subtree ('",
                      currentSubtree[1], "') in the inductive code tree - this ",
                      "should normally not occur (actually, it should not be ",
                      "possible). Not processing this subtree further.");
          }

          warning(paste0("Code '", codings[[codeRegex]][i], "' does not ",
                         "have a parent I can find!"));

        } else {

          if (!silent) {
            ufs::cat0("\n  - This subtree doesn't only consist of the parent/root code, but contains ",
                      length(currentSubtree)-1,
                      " child(ren). Processing child(ren).");
          }

          ### If it's found, loop through the children and progressively add them
          for (currentBranch in currentSubtree[2:length(currentSubtree)]) {
            if (currentBranch %in% data.tree::Get(currentNode$children, 'name')) {
              if (!silent) {
                ufs::cat0("\n      - This parent node already has a child with the name '",
                          currentBranch,
                          "', so not adding anything at this point.");
              }

            } else {

              if (!silent) {
                ufs::cat0("\n      - This parent node does not yet have a child with the name '",
                          currentBranch,
                          "', so adding it to that parent node.");
              }

              currentNode <-
                currentNode$AddChild(currentBranch);
              currentNode$label <-
                currentBranch;
              currentNode$code <-
                currentBranch;
            }
          }
        }
      }
    }

    data.tree::SetGraphStyle(inductiveCodeTrees[[codeRegex]],
                             directed="false");
    data.tree::SetGraphStyle(inductiveCodeTrees[[codeRegex]],
                             rankdir = "LR");

    tryCatch({
      inductiveDiagrammeR[[codeRegex]] <-
        data.tree::ToDiagrammeRGraph(inductiveCodeTrees[[codeRegex]]);
    }, error = function(e) {
      warning("Error issued by 'data.tree::ToDiagrammeRGraph' when converting '",
              codeRegex, "' code tree: ", e$message, "\n\nClass and content:\n\n",
              paste0(utils::capture.output(print(class(inductiveCodeTrees[[codeRegex]]))),
                     collapse="\n"),
              "\n",
              paste0(utils::capture.output(print(inductiveCodeTrees[[codeRegex]])),
                     collapse="\n"));
    });

  } else {
    inductiveCodeTrees[[codeRegex]] <- NULL;
  }

  res <- list(inductiveCodeProcessing=inductiveCodeProcessing,
              inductiveCodeTrees=inductiveCodeTrees,
              inductiveDiagrammeR=inductiveDiagrammeR,
              codingLeaves=codingLeaves,
              codings=codings);

  return(res);

}
