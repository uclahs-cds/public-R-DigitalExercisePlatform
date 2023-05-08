library(readxl)

if (FALSE) {
  folder <- file.path(data.folder, 'raw_data', 'Phase1')
  files <- setdiff(list.files(folder, pattern = '^PRESTO.*xlsx'), 'PRESTO_Adherence_Phase0-1.xlsx')
  fpath <- file.path(folder, files)
  outpath <- gsub('[.]xlsx$', '.tsv', fpath)

  for (i in seq_along(files)) {
    f <- fpath[i]
    out <- outpath[i]
    n.max <- if (!is.na(stringr::str_match(f, 'Time')[1])) 61 else Inf
    x <- readxl::read_excel(f, n.max = n.max)
    if ('patient identifier' %in% colnames(x)) {
      x[['study id']] <- trimws(x[['patient identifier']], whitespace = '[\\h\\v]')
      x[['patient identifier']] <- NULL
    }
    write.table(
      x,
      file = out,
      sep = '\t',
      row.names = FALSE
      )
    }
  }
