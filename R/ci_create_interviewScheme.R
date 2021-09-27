ci_create_interviewScheme <- function(nrm_spec,
                                      language = "en") {

  if (!inherits(nrm_spec, "rock_nrm_spec")) {
    stop("As `nrm_spec`, pass a Narrative Response Model specification ",
         "as produced by a call to rock::ci_import_nrm_spec().");
  }

  res <- character();

  itemIds <-
    nrm_spec$instrument$item_id[
      order(as.numeric(nrm_spec$instrument$sequence))
    ];

  for (currentItem in itemIds) {

    res <-
      c(
        res,
        "\n\n",
        "--<<item_break>>--\n",
        "\n### Item text:\n\n",
        paste0(
          nrm_spec$stimuli$stimulus[
            nrm_spec$stimuli$item_id == currentItem &
              nrm_spec$stimuli$language == language
          ],
          collapse = " "
        ),
        paste0("   [[uiid:", currentItem, "]]\n"),
        "\n### Think-aloud notes:\n\n\n",
        "\n### Probes:\n\n",
        paste0(
          nrm_spec$probes$probe_label[
            nrm_spec$probes$item_id == currentItem
          ],
          "   [[",
          nrm_spec$probes$probe_id[
            nrm_spec$probes$item_id == currentItem
          ],
          "]]\n",
          collapse = "\n\n\n"
        ),
        "\n\n\n\n"
      );

  }

  res <- paste0(res, collapse="");

  return(res);

}
