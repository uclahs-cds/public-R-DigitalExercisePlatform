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

    psa.data <- psa.data[order(as.integer(psa.data$dose.fct), -psa.data$psa.delta), ]
    rownames(psa.data) <- NULL

    dose.colors <- c('white', colour.gradient('royalblue', 6))
    names(dose.colors) <- levels(psa.data$dose.fct)

    # Split and combine
    # Stack on top of each other

    # psa.data$y <- 1:nrow(psa.data) + (7 - as.numeric(psa.data$dose.fct))
    psa.data$y <- 1:nrow(psa.data) + (as.numeric(psa.data$dose.fct) - 1)

    psa.data$col <- dose.colors[psa.data$dose]

    dummy.data <- data.frame(
      y = setdiff(seq(1, max(psa.data$y, na.rm = T)), psa.data$y),
      psa.delta = NA,
      col = 'transparent'
    )

    psa.data.dummy <- plyr::rbind.fill(
      psa.data,
      dummy.data
    )

    xlimits <- range(psa.data$psa.delta) + c(-0.05, 0.05);
    xat <- seq(-6, 9, by = 1);
    waterfall.grouped.plot <- create.barplot(
        psa.delta ~ y,
        data = psa.data.dummy,
        col = psa.data.dummy$col,
        xat = seq(1, max(psa.data.dummy$y)),
        plot.horizontal = FALSE,
        xlab.label = 'Patient',
        ylab.label = 'PSA Delta',
        xaxis.lab = rep('', nrow(psa.data.dummy)),
        disable.factor.sorting = TRUE,
        xaxis.tck = 0,
        yaxis.tck = c(1, 0),
        ylimits = xlimits,
        yat = xat
        );

    waterfall.grouped.plot <- remove.axis(waterfall.grouped.plot, side = c('bottom', 'right', 'top'))

    write.plot(
      waterfall.grouped.plot,
      width = 12,
      height = 10,
      resolution = 500,
      filename = file.path(
        plot.path,
        generate.filename(
          'digIT-EX',
          file.core = 'PSA_dose_waterfall_grouped',
          extension = 'png'
          )
        )
      )

    # Reset y
    # Sort by delta
    psa.data <- psa.data[order(-psa.data$psa.delta), ]
    psa.data$y <- 1:nrow(psa.data)

    waterfall.plot <- create.barplot(
        psa.delta ~ y,
        data = psa.data,
        col = psa.data$col,
        plot.horizontal = FALSE,
        xlab.label = 'Patient',
        ylab.label = 'PSA Delta',
        xaxis.lab = rep('', nrow(psa.data)),
        disable.factor.sorting = TRUE,
        xaxis.tck = 0,
        yaxis.tck = c(1, 0),
        ylimits = xlimits,
        yat = xat
        )

    waterfall.plot <- remove.axis(waterfall.plot, side = c('bottom', 'right', 'top'))

    write.plot(
      waterfall.plot,
      width = 12,
      height = 10,
      resolution = 500,
      filename = file.path(
        plot.path,
        generate.filename('digIT-EX', file.core = 'PSA_dose_waterfall_delta_ordered', extension = 'png')
        )
      )
    }
  )
