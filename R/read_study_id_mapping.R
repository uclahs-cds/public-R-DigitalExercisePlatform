read.study.id.mapping <- function(original.names = TRUE) {
  mapping <- read.table(
      file.path(data.folder, 'Phase1', 'raw_data', 'PRESTO_study_id_mapping.tsv'),
      header = TRUE
      );

  if (original.names) {
    res <- mapping$chronological.study.ID;
    names(res) <- mapping$original.study.ID;
    } else {
    res <- mapping$original.study.ID;
    names(res) <- mapping$chronological.study.ID;
    }
  return(res);
  }
