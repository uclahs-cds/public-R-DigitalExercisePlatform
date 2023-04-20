library(EXONC.DEXP);
library(BoutrosLab.plotting.general);

script.name <- 'adherence';
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

    common.cols <- c("study.id" , "bl.psa.ng.ml", "fu.psa.ng.ml", "dose")
    psa.data <- rbind(
      psa.dose[, common.cols],
      psa.control[, common.cols]
      )

    psa.data$dose.fct <- factor(psa.data$dose, levels = c('control', unique(psa.dose$dose)))
    psa.data$psa.delta <- psa.data$fu.psa.ng.ml - psa.data$bl.psa.ng.ml
    psa.data$psa.percent <- psa.data$psa.delta / psa.data$bl.psa.ng.ml

    dose.colors <- c('white', colour.gradient('royalblue', 6))
    names(dose.colors) <- levels(psa.data$dose.fct)
    create.barplot(
      study.id ~ psa.delta,
      data = psa.data,
      col = dose.colors[as.character(psa.data$dose)],
      plot.horizontal = TRUE,
      ylab.label = 'Patient',
      ylab.cex = 1.5,
      xlab.label = 'PSA Delta',
      yaxis.lab = rep('', nrow(psa.data)),
      yaxis.tck = 0,
      key = list(
        corner = c(0.055,.95),
        cex = 0.8,
        cex.title = 1,
        title = 'Dose',
        rect = list(
          col = dose.colors
          ),
        text = list(
          names(dose.colors)
          )
        )
    )
    }
  )
