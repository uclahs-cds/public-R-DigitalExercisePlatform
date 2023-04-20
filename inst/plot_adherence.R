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

    adherence.path <- file.path(data.folder, 'Phase1', 'raw_data', 'PRESTO_Adherence_Phase0-1.xlsx');
    adherence <- parse.adherence.xlsx(adherence.path);

    dosage <- read.table(
      file.path(data.folder, 'Phase1', 'raw_data', 'PRESTO_Dose_levels.tsv'),
      header = TRUE
      )

#     setdiff(dosage$study.id[dosage$phase == '1a'], adherence$study.id[adherence$phase == '1a'])

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
    adherence.long.phase1 <- adherence.long[adherence.long$phase %in% c('1a', '0B - Prostate') , ];

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

    dosages <- sort(unique(adherence.long.phase1$dose), na.last = NA)
    dosage.colors <- colour.gradient('orange', length(dosages))
    names(dosage.colors) <- dosages

    adherence.long.phase1$Variable.factor.long <- adherence.long.phase1$Variable.factor
    levels(adherence.long.phase1$Variable.factor.long) <- c('Exercise Therapy', 'Watch', 'Blood Pressure', 'Scale');

    adherence.long.phase1$dose.fct <- as.factor(adherence.long.phase1$dose);
    dose.adherence.boxplot <- adherence.boxplot(
      x = adherence.long.phase1,
      formula = Percent ~ dose.fct | Variable.factor.long,
      extension = extension,
      phase = 'phase1',
      variable.names = dosages,
      use.gotham.font = FALSE,
      points.col = dosage.colors,
      xlab.label = 'Dose'
      );

    write.plot(
      dose.adherence.boxplot,
      width = 12,
      height = 10,
      resolution = 100,
      filename = file.path(
        plot.path,
        generate.filename('digIT-EX', file.core = paste0('1a_1b-prostate', '_adherence_dosage'), extension = 'png')
        )
      )
    }
  );
