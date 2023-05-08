library(EXONC.DEXP);
library(BoutrosLab.plotting.general);

script.name <- 'psa_ki67';
data.folder <- Sys.getenv('EXONC_HOME');
if (data.folder == '') data.folder <- 'DEXP_results';
phase1.only <- FALSE;

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    extension <- 'png';
    plot.path <- file.path(data.folder, 'digIT-EX', 'plots', script.name);

    dosage <- read.table(
      file.path(data.folder, 'raw_data', 'Phase1', 'PRESTO_Dose_levels.tsv'),
      header = TRUE
      );

    psa.ki67.dose <- read.table(
      file.path(data.folder, 'raw_data', 'Phase1', 'PRESTO_ki67_PSA_data_prostate_FINAL.tsv'),
      header = TRUE
    )
    colnames(psa.ki67.dose) <- gsub('[.]$', '', tolower(colnames(psa.ki67.dose)));
    colnames(psa.ki67.dose) <- gsub('[.]+', '.', colnames(psa.ki67.dose));

    if (phase1.only) {
      psa.ki67.dose <- merge(psa.ki67.dose, dosage, all.x = TRUE, by = c('study.id', 'dose'));
      psa.ki67.dose <- psa.ki67.dose[psa.ki67.dose$phase == '1a', ];
      phase1.suffix <- '_phase1a';
      } else {
      phase1.suffix <- '_phase0b-prostate_1a';
      }

    psa.dose <- psa.ki67.dose[
      !is.na(psa.ki67.dose$psa.delta),
      c('study.id', 'dose', 'bl.psa.ng.ml', 'fu.psa.ng.ml', 'psa.delta')
      ]

    psa.control <- read.table(
      file.path(data.folder, 'raw_data', 'Phase1', 'PRESTO_PSA_control_data.tsv'),
      header = TRUE
      )
    colnames(psa.control) <- c('bl.psa.ng.ml', 'fu.psa.ng.ml', 'days.between.psas');
    psa.control$dose <- 'control';
    # Artificial study id
    psa.control$study.id <- seq(max(psa.dose$study.id), max(psa.dose$study.id) + nrow(psa.control) - 1, by = 1) + 100

    # colnames(psa.dose) <- gsub('[.]$', '', tolower(colnames(psa.dose)))
    # colnames(psa.dose) <- gsub('[.]+', '.', colnames(psa.dose))

    common.cols <- c('study.id' , 'bl.psa.ng.ml', 'fu.psa.ng.ml', 'dose')
    psa.data <- rbind(
      psa.dose[, common.cols],
      psa.control[, common.cols]
      )

    psa.data$delta <- psa.data$fu.psa.ng.ml - psa.data$bl.psa.ng.ml

    dose.colors <- c('white', colour.gradient('royalblue', 6));
    names(dose.colors) <- c('control', sort(unique(dosage$dose)));
    psa.data$col <- dose.colors[psa.data$dose];

    plot.delta.waterfall(
      psa.data,
      variable = 'PSA',
      dose.levels = c('control', sort(unique(psa.dose$dose))),
      filename = file.path(
        plot.path,
        generate.filename(
          'digIT-EX',
          file.core = paste0(
            'PSA_dose_waterfall_grouped',
            phase1.suffix
            ),
          extension = 'png'
          )
        )
    )

    ki67.data <- psa.ki67.dose[
      !is.na(psa.ki67.dose$ki67.delta),
      c('study.id', 'dose', 'bl.ki67.positivity', 'fu.ki67.positivity', 'ki67.delta')
      ];

    ki67.data$delta <- ki67.data$ki67.delta;
    ki67.data$ki67.delta <- NULL;
    ki67.data$col <- dose.colors[as.character(ki67.data$dose)];

    agg.res <- do.call(
      'data.frame',
      aggregate(psa.data$delta, by = list(psa.data$dose), function(x) c(mean = mean(x), sd = sd(x)))
      );
    rownames(agg.res) <- as.character(agg.res$Group.1);

    agg.res.ki67 <- do.call(
      'data.frame',
      aggregate(ki67.data$delta, by = list(ki67.data$dose), function(x) c(mean = mean(x), sd = sd(x)))
      );
    rownames(agg.res.ki67) <- as.character(agg.res.ki67$Group.1);

    kableExtra::kable_styling(kableExtra::kable(
      agg.res[names(dose.colors), ],
      col.names = c('dose', 'mean(delta-PSA)', 'sd(delta-PSA)'),
      row.names = FALSE
      ))

    kableExtra::kable_styling(kableExtra::kable(
      agg.res.ki67[setdiff(names(dose.colors), 'control'), ],
      col.names = c('dose', 'mean(delta-ki67)', 'sd(delta-ki67)'),
      row.names = FALSE
      ))

    plot.delta.waterfall(
      ki67.data,
      variable = 'ki67',
      dose.levels = c('control', sort(unique(psa.dose$dose))),
      filename = file.path(
        plot.path,
        generate.filename(
          'digIT-EX',
          file.core = paste0(
            'ki67_dose_waterfall_grouped',
            phase1.suffix
            ),
          extension = 'png'
          )
        )
    );

    # Reset y
    # Sort by delta
    # psa.data <- psa.data[order(-psa.data$psa.delta), ]
    # psa.data$y <- 1:nrow(psa.data)
    #
    # waterfall.plot <- create.barplot(
    #     psa.delta ~ y,
    #     data = psa.data,
    #     col = psa.data$col,
    #     plot.horizontal = FALSE,
    #     xlab.label = 'Patient',
    #     ylab.label = expression(bold('PSA'~Delta)),
    #     xaxis.lab = rep('', nrow(psa.data)),
    #     disable.factor.sorting = TRUE,
    #     xaxis.tck = 0,
    #     yaxis.tck = c(1, 0),
    #     ylimits = xlimits,
    #     yat = xat
    #     )
    #
    # waterfall.plot <- remove.axis(waterfall.plot, side = c('bottom', 'right', 'top'))
    #
    # write.plot(
    #   waterfall.plot,
    #   width = 12,
    #   height = 10,
    #   resolution = 500,
    #   filename = file.path(
    #     plot.path,
    #     generate.filename('digIT-EX', file.core = 'PSA_dose_waterfall_delta_ordered', extension = 'png')
    #     )
    #   )
    }
  )
