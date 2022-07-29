library(EXONC.DEXP);
library(BoutrosLab.plotting.general);

script.name <- 'adherence';
data.folder <- Sys.getenv('EXONC_DEXP_HOME');
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
    perc.phase0a.long <- read.table(system.file('extdata', 'adherence_perc_phase0a.tsv', package = 'EXONC.DEXP'), header = TRUE, sep = '\t');
    perc.phase0b.long <- read.table(system.file('extdata', 'adherence_perc_phase0a.tsv', package = 'EXONC.DEXP'), header = TRUE, sep = '\t');

    perc.phase0a.long <- perc.phase0a.long[perc.phase0a.long$Variable != 'Sleep', ];
    perc.phase0b.long <- perc.phase0b.long[perc.phase0b.long$Variable != 'Sleep', ];

    variable.names <- c('Exercise\nTherapy', 'Watch', 'Blood\nPressure', 'Scale');

    adherence.boxplot(
      x = perc.phase0a.long,
      plot.path = plot.path,
      extension = extension,
      phase = 'phase0a',
      use.gotham.font = TRUE,
      variable.names = variable.names
      );

    adherence.boxplot(
      x = perc.phase0b.long,
      plot.path = plot.path,
      extension = extension,
      phase = 'phase0b',
      use.gotham.font = TRUE,
      variable.names = variable.names
      );
    }
  );
