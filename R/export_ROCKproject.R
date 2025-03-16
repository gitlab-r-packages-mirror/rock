#' Export a ROCK project to a single ROCKproject file
#'
#' @param output The file to write to; should have the extension `.ROCKproject`
#' @param path The path with the ROCK project
#' @param includeRegex A regular expression used to select files to include in
#' the project file
#' @param excludeRegex A regular expression used to omit files from the
#' project file; selection takes place after the selection by `includeRegex`
#' @param createDirs Whether to, if any directories in the `output` path does
#' not exist, create these
#' @param preventOverwriting If the output file already exists, whether to
#' prevent it from being overwritten (`TRUE`) or not (`FALSE`).
#' @param forceBaseZip Whether to force using the `zip()` function included in
#' R even if the `zip` package is installed.
#' @param silent Whether to be chatty or silent
#'
#' @returns Invisibly, `output`.
#' @export
#'
#' @examples ### Get path to example project
#' examplePath <-
#'   system.file(
#'     "ROCKprojects",
#'     "exportable-ROCKproject-1",
#'     package="rock"
#'   );
#'
#' ### Get a temporary filename to write to
#' projectFilename <-
#'   tempfile(
#'     fileext = ".ROCKproject"
#'   );
#'
#' ### Export it
#' rock::export_ROCKproject(
#'   path = examplePath,
#'   output = projectFilename,
#'   silent = FALSE
#' );
export_ROCKproject <- function(output,
                               path = ".",
                               includeRegex = NULL,
                               excludeRegex = NULL,
                               createDirs = FALSE,
                               preventOverwriting = TRUE,
                               forceBaseZip = FALSE,
                               silent = rock::opts$get(silent)) {

  outputDir <- dirname(output);

  if (!dir.exists(dirname(output))) {
    if (createDirs) {
      dir.create(outputDir, recursive = TRUE);
    } else {
      stop("The directory where you wanted to create the ROCKproject file, `",
           outputDir,
           "`, does not exist!");
    }
  }

  if (!dir.exists(path)) {

    stop("The directory you specified to export the ROCKproject from, `", path,
         "`, does not exist!");

  }

  oldWorkingDirectory <- getwd();
  on.exit(setwd(oldWorkingDirectory));
  setwd(path);

  if (!file.exists(file.path(path, "_ROCKproject.yml"))) {
    ROCKprojectYAML <- '
_ROCKproject:

  project:

    title: "Project Title"                       # Any character string
    authors: "Author names as string"            # Any character string
    authorIds:
      -
        display_name: "Author name 1"            # Any character string
        orcid: "0000-0000-0000-0001"             # Any character string matching ^([0-9]{4}-){3}[0-9]{4}$
        shorcid: "ixxxxxx"                       # Any character string matching ^i([0-9a-zA-Z]+$
      -
        display_name: "Author name 2"            # Any character string
        orcid: "0000-0000-0000-0002"             # Any character string matching ^([0-9]{4}-){3}[0-9]{4}$
        shorcid: "ixxxxxx"                       # Any character string matching ^i([0-9a-zA-Z]+$

    version: "1.1"                               # Anything matching regex [0-9]+(\\.[0-9]+)*
    ROCK_version: 1                              # Anything matching regex [0-9]+(\\.[0-9]+)*
    ROCK_project_version: 1                      # Anything matching regex [0-9]+(\\.[0-9]+)*
    date_created: "2023-03-01 20:03:51 UTC"      # Anything matching that date format, preferably converted to UTC timezone
    date_modified: "2023-03-08 20:03:51 UTC"     # Anything matching that date format, preferably converted to UTC timezone

  sources:

    extension: ".rock"                           # Any valid extension
    recursive: true                              # true or false
    dirsToIncludeRegex: data/                    # Any regex or ~
    dirsToExcludeRegex: ~                        # Any regex or ~
    filesToIncludeRegex: ~                       # Any regex or ~
    filesToExcludeRegex: ~                       # Any regex or ~

  workflow:

    pipeline:
      -
        stage: raw                               # Anything matching regex [a-A-Z][a-zA-Z0-9_]*
        dirName: "data/010---raw-sources"        # Any valid directory name, using a forward slash as separator
        nextStages:
          -
            nextStageid: clean                   # A different stage identifier or ~
            actionId: cleanSource
          -
            nextStageid: uids                    # A different stage identifier or ~
            actionId: addUIDs
      -
        stage: clean                             # Anything matching regex [a-A-Z][a-zA-Z0-9_]*
        dirName: "data/020---cleaned-sources"    # Any valid directory name, using a forward slash as separator
        nextStages:
          -
            nextStageid: uids                    # A different stage identifier or ~
            actionId: addUIDs
      -
        stage: uids                              # Anything matching regex [a-A-Z][a-zA-Z0-9_]*
        dirName: "data/030---sources-with-uids"  # Any valid directory name, using a forward slash as separator
        nextStage: coded                         # A different stage identifier or ~
      -
        stage: coded                             # Anything matching regex [a-A-Z][a-zA-Z0-9_]*
        dirName: "data/040---coded-sources"      # Any valid directory name, using a forward slash as separator
        nextStage: masked                        # A different stage identifier or ~
      -
        stage: masked                            # Anything matching regex [a-A-Z][a-zA-Z0-9_]*
        dirName: "data/090---masked-sources"     # Any valid directory name, using a forward slash as separator
        nextStage: ~                             # A different stage identifier or ~

    actions:
      -
        actionId: addUIDs                        # String, referenced from the stages
        language: R                              # Language, has to be matched to interpreter
        dependencies: rock                       # Dependencies to be loaded before running the script
        script: |                                # Literal block style string
          rock::prepend_ids_to_sources(
            input = {currentStage::dirName},
            output = {nextStage::dirName}
          );

';
    writeLines(
      ROCKprojectYAML,
      file.path(path, "_ROCKproject.yml")
    );
  }

  fullFileList <-
    list.files(
      path,
      recursive = TRUE,
      full.names = FALSE
    );

  msg(
    "The full list of files is\n\n",
    paste0(paste0("  - ", fullFileList, "\n")),
    "\n",
    silent = silent
  );

  if (!is.null(includeRegex)) {

    includedFiles <-
      grep(
        includeRegex,
        fullFileList,
        value = TRUE
      );

  } else {

    includedFiles <- fullFileList;

  }

  if (!is.null(excludeRegex)) {

    excludedFiles <-
      grep(
        excludeRegex,
        includedFiles,
        value = TRUE
      );

    selectedFiles <-
      setdiff(
        includedFiles,
        excludedFiles
      );

  } else {

    excludedFiles <- NULL;
    selectedFiles <- includedFiles;

  }

  if (file.exists(output)) {
    if (preventOverwriting) {
      stop("The file you specified to write to, '", output,
           "', already exists, and ",
           "`preventOverwriting` is set to TRUE, so I'm aborting.");
    } else {

      unlink(output);

      msg("The file you specified to write to, '", output,
          "', already existed, and `preventOverwriting` is set to FALSE, ",
          "so I deleted it.\n\n",
          silent = silent);

    }
  }

  if (requireNamespace("zip", quietly = TRUE) && (!forceBaseZip)) {
    zip::zip(
      zipfile = output,
      files = selectedFiles
    );
  } else {
    tryCatch(
      {
        zipResults <-
          utils::capture.output(
            zip(
              zipfile = output,
              files = selectedFiles
            )
          )
      },
      error = function(e) {
        stop("ROCK project files are ZIP archives. I tried to ZIP the files ",
             "you specified using R's native `zip()` function, but it ",
             "returned this error:\n\n  ", e$message, "\n\nYou can install ",
             "the R package {zip}, which may resolve the error. To do that, ",
             "run:\n\n  install.packages('zip');\n\n");
      }
    );
  }

  return(invisible(output));

}
