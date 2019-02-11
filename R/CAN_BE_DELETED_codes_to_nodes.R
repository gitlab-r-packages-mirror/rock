#' Convert specifications of (deductive) codes into a hierarchical coding structure
#'
#' This function is called by [parse_sources()] to process the specification(s)
#' of codes as used for deductive coding into one hierarchical coding structure
#' (or coding tree).
#'
#' @param codes The list of YAML fragments that contain information about codes,
#' as extracted from the sources by [extract_yaml_fragments()].
#' @param silent Whether to provide (`FALSE`) or suppress (`TRUE`) more detailed progress updates.
#'
#' @return a [data.tree::Node()] object.
#' @export
#'
#' @examples Add example here!
# codes_to_nodes <- function(codes,
#                            silent=TRUE) {
#
#   if (is.null(codes)) {
#     return(codes);
#   }
#
#   ### Check whether this is a list of one list; this sometimes
#   ### occurs as a side effect, given that processing is mostly
#   ### geared towards lists.
#   if (identical(unlist(codes, recursive=FALSE), codes[[1]])) {
#     codes <- codes[[1]];
#   }
#
#   ### To allow for flexibility that may be nice in the future
#   idName <- 'id';
#   labelName <- 'label';
#   codeName <- 'code';
#   parentIdName <- 'parentId';
#   childrenName <- 'children';
#
#   if (!is.null(codes[[idName]])) {
#     ### We have an identifier, so this is a node in itself. First check
#     ### and if necessary, clean up this node.
#     if (!silent) {
#       print(glue::glue("Passed object has identifier '{codes[[idName]]}'."));
#     }
#     if (is.null(codes[[labelName]])) {
#       codes[[labelName]] <- codes[[idName]];
#     }
#     if (is.null(codes[[codeName]])) {
#       codes[[codeName]] <- codes[[idName]];
#     }
#     ### Then, check whether it has children.
#     if (is.null(codes[[childrenName]])) {
#       ### If not, convert this node into a Node and return it.
#       res <- data.tree::Node$new(codes[[idName]]); #as.Node(codes);
#       res[[labelName]] <- codes[[labelName]];
#       res[[codeName]] <- codes[[codeName]];
#       res[[parentIdName]] <- codes[[parentIdName]];
#       return(res);
#     } else {
#       ### Check whether the children are 'shorthand children', and if
#       ### they are, construct the proper nodes first.
#       if (is.atomic(codes$children)) {
#         codes$children <-
#           lapply(codes$children,
#                  function(x) {
#                    setNames(list(x,x,x),
#                             c(idName,
#                               labelName,
#                               codeName));
#                  });
#       }
#       if (!silent) {
#         print(glue::glue("Converting it to a data.tree Node and returning it."));
#       }
#       ### Then convert this node into a Node
#       res <-
#         data.tree::FromListExplicit(codes,
#                                     nameName=idName,
#                                     childrenName=childrenName);
#
#       if (!silent) {
#         print(glue::glue("Tree root object has name '{res$name}'."));
#       }
#
#       ### Check for missing labels and/or codes and fill them with the identifiers
#       res$Do(function(node) {
#         if (is.null(node[[labelName]])) {
#           node[[labelName]] <- node$name;
#         }
#         if (is.null(node[[codeName]])) {
#           node[[codeName]] <- node$name;
#         }
#       });
#       ### Return the result
#       return(res);
#     }
#   } else {
#     ### This is a list of nodes, so pass each on to this function and
#     ### collect the results; then start building the tree.
#     if (!silent) {
#       print(glue::glue("Passed object does not have an identifier; processing it as a list of nodes."));
#     }
#
#     nodeList <-
#       lapply(codes,
#              codes_to_nodes, silent=silent);
#
#     nodeIds <-
#       data.tree::Get(nodeList,
#                      'name');
#
#     if (!silent) {
#       print(glue::glue("Processed {length(nodeList)} nodes ({ufs::vecTxtQ(nodeIds)})."));
#     }
#
#     ### If it's a single node, just return it immediately.
#     if (length(nodeList) == 1) {
#       if (!silent) {
#         print(glue::glue("Single node, so returning it."));
#       }
#       return(nodeList[[1]]);
#     }
#
#     ### Create the data tree object
#     codeTree <- data.tree::Node$new();
#
#     ### Add all children of nodes without an id as children of the codeTree root
#     if (any(nchar(nodeIds)==0)) {
#       for (nodeWithoutId in nodeList[nchar(nodeIds)==0]) {
#         if (!silent) {
#           print(glue::glue("Found a set of nodes without identifiers; adding the children to the root of the code tree."));
#         }
#         for (subNodeWithoutId in nodeWithoutId$children) {
#           codeTree$AddChildNode(subNodeWithoutId);
#           if (!silent) {
#             print(glue::glue("Added '{subNodeWithoutId$name}' to the root of the code tree."));
#           }
#         }
#       }
#       ### Then remove them from the nodeList
#       nodeList <-
#         nodeList[nchar(nodeIds)>0];
#     }
#
#     ### Check which nodes have a parent
#     parentIds <-
#       data.tree::Get(nodeList,
#                      parentIdName);
#
#     nodesWithoutParents <-
#       nodeList[unlist(is.na(parentIds))];
#     nodesWithParents <-
#       nodeList[unlist(!is.na(parentIds))];
#
#     ### Attach those that don't to the root.
#     for (i in nodesWithoutParents) {
#       codeTree$AddChildNode(i);
#       if (!silent) {
#         print(glue::glue("Attached parentless node '{i$name}' to the root of the code tree."));
#       }
#     }
#     ### For those that do, insert them at the appropriate place.
#     for (i in seq_along(nodesWithParents)) {
#       if (!silent) {
#         print(glue::glue("Starting to process node '{nodesWithParents[[i]]$name}' to find its parent '{nodesWithParents[[i]][[parentIdName]]}'."));
#       }
#       parentNode <-
#         data.tree::FindNode(codeTree,
#                             nodesWithParents[[i]][[parentIdName]]);
#       if (!is.null(parentNode)) {
#         ### Parent is already in the coding tree; attach this child node.
#         parentNode$AddChildNode(nodesWithParents[[i]]);
#         if (!silent) {
#           print(glue::glue("Attached node '{nodesWithParents[[i]]$name}' to its parent node '{parentNode$name}' in the code tree."));
#         }
#       } else {
#         ### Parent is not in the coding tree; look in the other nodes with
#         ### parents that we still have to process
#         if (i == length(nodesWithParents)) {
#           stop(paste0("Node with identifier '", nodesWithParents[[i]]$name,
#                       "' has specified parent '", nodesWithParents[[i]][[parentIdName]],
#                       "' but no node with that identifier exists."));
#         } else {
#           foundParent <- FALSE;
#           for (j in (i+1):length(nodesWithParents)) {
#             parentNode <-
#               data.tree::FindNode(nodesWithParents[[j]],
#                                   nodesWithParents[[i]][[parentIdName]]);
#             if (!is.null(parentNode)) {
#               ### Parent is not yet in the coding tree; attach this child node.
#               parentNode$AddChildNode(nodesWithParents[[i]]);
#               foundParent <- TRUE;
#               if (!silent) {
#                 print(glue::glue("Attached node '{nodesWithParents[[i]]$name}' to its parent node '{parentNode$name}', for now outside the code tree."));
#               }
#             }
#           }
#           if (!foundParent) {
#             print(parentNode);
#             stop(paste0("Node with identifier '", nodesWithParents[[i]]$name,
#                         "' has specified parent '", nodesWithParents[[i]][[parentIdName]],
#                         "' but no node with that identifier exists (all node identifiers are ",
#                         ufs::vecTxtQ(nodeIds), ")."));
#           }
#         }
#       }
#     }
#     return(codeTree);
#   }
# }
