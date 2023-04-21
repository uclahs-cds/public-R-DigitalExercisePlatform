library(EXONC.DEXP);
library(BoutrosLab.plotting.general);

script.name <- 'waterfall';
data.folder <- Sys.getenv('EXONC_HOME');
if (data.folder == '') data.folder <- 'DEXP_results';

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    extension <- 'png';
    plot.path <- file.path(data.folder, 'plots', script.name);

    psa.dose <- read.table(
      file.path(data.folder, 'Phase1', 'raw_data', 'PRESTO_PSA_by_dose.tsv'),
      header = TRUE
      )
    colnames(psa.dose) <- gsub('[.]$', '', tolower(colnames(psa.dose)))
    colnames(psa.dose) <- gsub('[.]+', '.', colnames(psa.dose))

    psa.control <- read.table(
      file.path(data.folder, 'Phase1', 'raw_data', 'PRESTO_PSA_control_data.tsv'),
      header = TRUE
      )
    colnames(psa.control) <- c('bl.psa.ng.ml', 'fu.psa.ng.ml', 'days.between.psas');
    psa.control$dose <- 'control';
    # Artificial study id
    psa.control$study.id <- seq(max(psa.dose$study.id), max(psa.dose$study.id) + nrow(psa.control) - 1, by = 1) + 100

    colnames(psa.dose) <- gsub('[.]$', '', tolower(colnames(psa.dose)))
    colnames(psa.dose) <- gsub('[.]+', '.', colnames(psa.dose))

    common.cols <- c('study.id' , 'bl.psa.ng.ml', 'fu.psa.ng.ml', 'dose')
    psa.data <- rbind(
      psa.dose[, common.cols],
      psa.control[, common.cols]
      )

    psa.data$dose.fct <- factor(psa.data$dose, levels = c('control', unique(psa.dose$dose)))
    psa.data$psa.delta <- psa.data$fu.psa.ng.ml - psa.data$bl.psa.ng.ml
    psa.data$psa.percent <- psa.data$psa.delta / psa.data$bl.psa.ng.ml

    psa.data <- psa.data[order(- as.integer(psa.data$dose.fct), psa.data$psa.delta), ]
    rownames(psa.data) <- NULL

    dose.colors <- c('white', colour.gradient('royalblue', 6))
    names(dose.colors) <- levels(psa.data$dose.fct)

    # Split and combine
    # Stack on top of each other

    psa.data$y <- 1:nrow(psa.data) + (7 - as.numeric(psa.data$dose.fct))

    psa.data$col <- dose.colors[psa.data$dose]

    dummy.data <- data.frame(
      y = setdiff(seq(1, max(psa.data$y, na.rm = T)), psa.data$y),
      psa.delta = NA,
      col = 'transparent'
    )

    psa.data <- plyr::rbind.fill(
      psa.data,
      dummy.data
    )

    create.barplot(
        y ~ psa.delta,
        data = psa.data,
        col = psa.data$col,
        yat = seq(1, max(psa.data$y)),
        plot.horizontal = TRUE,
        ylab.label = 'Patient',
        xlab.label = 'PSA Delta',
        yaxis.lab = rep('', nrow(psa.data)),
        disable.factor.sorting = TRUE,
        yaxis.tck = 0,
        xaxis.tck = c(1, 0),
        width = 8,
        height = 10,
        resolution = 250,
        xlimits = range(psa.data$psa.delta) + c(-0.05, 0.05),
        filename = file.path(
          plot.path,
          generate.filename('digIT-EX', file.core = 'PSA_dose_waterfall', extension = 'png')
          )
        )
    }
  )
