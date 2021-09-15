ci_create_interviewScheme <- function(qrm_spec,
                                      language = "en") {

  if (!inherits(qrm_spec, "rock_qrm_spec")) {
    stop("As `qrm_spec`, pass a Qualitative Response Model specification ",
         "as produced by a call to rock::ci_import_qrm_spec().");
  }

  res <- character()

  itemIds <-
    qrm_spec$instrument$item_id[
      order(as.numeric(qrm_spec$instrument$sequence))
    ];

  for (currentItem in itemIds) {

    res <-
      c(
        res,
        "\n\n",
        "--<<item_break>>--\n",
        "\n### Item text:\n\n",
        paste0(
          qrm_spec$stimuli$stimulus[
            qrm_spec$stimuli$item_id == currentItem &
              qrm_spec$stimuli$language == language
          ],
          collapse = " "
        ),
        paste0("   [[uiid:", currentItem, "]]\n"),
        "\n### Think-aloud notes:\n\n\n",
        "\n### Probes:\n\n",
        paste0(
          qrm_spec$probes$probes_label[
            qrm_spec$probes$item_id == currentItem
          ],
          "   [[",
          qrm_spec$probes$probes_id[
            qrm_spec$probes$item_id == currentItem
          ],
          "]]\n",
          collapse = "\n\n\n"
        ),
        "\n\n\n\n"
      );

  }

  return(res);

}
