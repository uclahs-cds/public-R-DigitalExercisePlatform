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
    plot.path <- file.path(data.folder, 'digIT-EX', 'plots', script.name);

    # Exercise adherence and adherence to health devices for the 3 patients in phase 0a.
    # Boxplot with the points shown (stripplot with a barplot overlaid), with columns for each data-type,
    # and then outlier points labeled with their patient numbers
    # Make a copy of the adherence data for Phase 0a patients
    # perc.phase0a.long <- read.table(system.file('extdata', 'adherence_perc_phase0a.tsv', package = 'EXONC.DEXP'), header = TRUE, sep = '\t');
    # perc.phase0b.long <- read.table(system.file('extdata', 'adherence_perc_phase0b.tsv', package = 'EXONC.DEXP'), header = TRUE, sep = '\t');
    #
    # perc.phase0a.long <- perc.phase0a.long[perc.phase0a.long$Variable != 'Sleep', ];
    # perc.phase0b.long <- perc.phase0b.long[perc.phase0b.long$Variable != 'Sleep', ];

    variable.names <- c('Exercise\nTherapy', 'Watch', 'Blood\nPressure', 'Scale');

    adherence.path <- file.path(data.folder, 'raw_data', 'Phase1', 'PRESTO_Adherence_Phase0-1.xlsx');
    adherence <- parse.adherence.xlsx(adherence.path);

    dosage <- read.table(
      file.path(data.folder, 'raw_data', 'Phase1', 'PRESTO_Dose_levels.tsv'),
      header = TRUE
      )

    id.mapping <- read.study.id.mapping(original.names = FALSE);

    # Switch back to original study id
    adherence$study.id[startsWith(adherence$phase, '0')] <- id.mapping[as.character(adherence$study.id[startsWith(adherence$phase, '0')])]

    write.table(
      adherence,
      file = file.path(data.folder, 'Phase1', 'processed_xlsx', 'PRESTO_Adherence_Phase0-1.tsv'),
      sep = '\t',
      row.names = FALSE
      );

    adherence.long <- reshape(
      data = adherence[, c('study.id', 'phase', 'attendance.percent', 'watch.percent', 'bp.percent', 'scale.percent')],
      idvar = c('study.id', 'phase'),
      varying = c('attendance.percent', 'watch.percent', 'bp.percent', 'scale.percent'),
      v.names = 'Percent',
      timevar = 'variable',
      times = c('attendance', 'watch', 'bp', 'scale'),
      direction = 'long'
      );

    adherence.long$Variable.factor <- factor(adherence.long$variable, levels = c('attendance', 'watch', 'bp', 'scale'));
    adherence.long$Percent <- adherence.long$Percent * 100;

    adherence.long.0b <- adherence.long[startsWith(adherence.long$phase, '0B'), ];
    adherence.long.phase1 <- adherence.long[adherence.long$phase == '1a', ];

    # Add dosage
    adherence.long.phase1 <- merge(adherence.long.phase1, dosage, by = 'study.id', all.x = TRUE)

    adherence.boxplot(
      x = adherence.long.0b,
      plot.path = plot.path,
      extension = extension,
      phase = 'phase0b',
      use.gotham.font = FALSE,
      variable.names = variable.names
      );

    adherence.boxplot(
      x = adherence.long.phase1,
      plot.path = plot.path,
      extension = extension,
      phase = 'phase1',
      use.gotham.font = FALSE,
      variable.names = variable.names
      );

    doses <- sort(unique(dosage$dose), na.last = NA)
    dose.colors <- c(colour.gradient('royalblue', 6));
    names(dose.colors) <- doses

    adherence.long.phase1$Variable.factor.long <- adherence.long.phase1$Variable.factor
    levels(adherence.long.phase1$Variable.factor.long) <- c('Exercise Therapy', 'Watch', 'Blood Pressure', 'Scale');

    phase1.doses <- sort(unique(adherence.long.phase1$dose));
    # missing.doses <- setdiff(doses, unique(adherence.long.phase1$dose));
    # # This to to fill a dummy NA for the dose so that an empty space is drawn for the dose
    # adherence.long.phase1 <- merge(
    #   x = adherence.long.phase1,
    #   y = data.frame(
    #     dose = missing.doses,
    #     Variable.factor = 'attendance'
    #     ),
    #   all = TRUE,
    #   by = c('dose', 'Variable.factor')
    #   );

    adherence.long.phase1$dose.fct <- factor(adherence.long.phase1$dose, levels = phase1.doses);

    dose.adherence.boxplot <- adherence.boxplot(
      x = adherence.long.phase1,
      formula = Percent ~ dose.fct | Variable.factor.long,
      main = 'Phase 1',
      main.cex = 2,
      extension = extension,
      phase = 'phase1',
      variable.names = phase1.doses,
      use.gotham.font = FALSE,
      points.col = dose.colors[as.character(phase1.doses)],
      xlab.label = 'Dose'
      );

    dose.adherence.path <- file.path(
        plot.path,
        generate.filename(
          'digIT-EX',
          file.core = paste0('1a', '_adherence_dosage'),
          extension = 'png'
          )
        );
    cat('Writing dose-adherence plot to: ', dose.adherence.path, '\n');
    write.plot(
      dose.adherence.boxplot,
      width = 12,
      height = 10,
      resolution = 100,
      filename = dose.adherence.path
      )

    # Plot of only exercise therapy
    adherence.long.phase1.ET <- adherence.long.phase1[
      adherence.long.phase1$Variable.factor == 'attendance',
      ]

    dose.adherence.boxplot.ET <- adherence.boxplot(
      x = adherence.long.phase1.ET,
      formula = Percent ~ dose.fct,
      main = 'Phase 1',
      main.cex = 2,
      extension = extension,
      phase = 'phase1',
      variable.names = phase1.doses,
      use.gotham.font = FALSE,
      points.col = dose.colors[as.character(adherence.long.phase1.ET$dose)],
      xlab.label = 'Dose'
      );

    dose.adherence.path.ET <- file.path(
      plot.path,
      generate.filename(
        'digIT-EX',
        file.core = paste0('1a', '_ET_adherence_dosage'),
        extension = 'png'
        )
      );
    cat('Writing dose-adherence ET plot to: ', dose.adherence.path.ET, '\n');
    write.plot(
      dose.adherence.boxplot.ET,
      width = 12,
      height = 10,
      resolution = 250,
      filename = dose.adherence.path.ET
      )

    # Plot the completed sessions in waterfall plot
    adherence.phase1.waterfall <- adherence[
      adherence$phase == '1a',
      c('study.id', 'phase', 'completed.exercise.sessions')
      ];

    # Add dose data
    adherence.phase1.waterfall <- merge(
      x = adherence.phase1.waterfall,
      y = dosage,
      by = 'study.id',
      all.x = TRUE
      );

    adherence.phase1.waterfall$delta <- adherence.phase1.waterfall$completed.exercise.sessions;

    plot.delta.waterfall(
      adherence.phase1.waterfall,
      variable = 'adherence',
      filename = file.path(
        plot.path,
        generate.filename(
          'digIT-EX',
          file.core = 'exercise_sessions_waterfall_grouped',
          extension = 'png'
          )
        )
      );

    agg.res <- do.call(
      'data.frame',
      aggregate(
        adherence.phase1.waterfall$delta,
        by = adherence.phase1.waterfall[, c('phase.x', 'dose')],
        function(x) c(median = median(x), min = min(x), max = max(x), n = length(x)))
      );
    colnames(agg.res) <- c('phase', 'dose', 'median', 'min', 'max', 'n');
    kableExtra::kable_styling(kableExtra::kable(
      agg.res,
      row.names = FALSE
      ))
    }
  );
