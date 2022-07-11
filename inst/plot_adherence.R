library(DigITx);
library(BoutrosLab.plotting.general);

script.name <- 'adherence';
data.folder <- Sys.getenv('DIGITX_HOME');
if (data.folder == '') data.folder <- here::here('results');

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
    perc.phase0a.long <- adherence.perc.phase0a;
    perc.phase0b.long <- adherence.perc.phase0b;

    perc.phase0a.long <- perc.phase0a.long[perc.phase0a.long$Variable != "Sleep", ];
    perc.phase0b.long <- perc.phase0b.long[perc.phase0b.long$Variable != "Sleep", ];

    adherence.boxplot(
      x = perc.phase0a.long,
      plot.path = plot.path,
      extension = extension,
      phase = 'phase0a',
      gotham.font = TRUE,
      variable.names = c('Exercise\nAttendance', 'Watch', 'Blood\nPressure', 'Scale')
      );

    adherence.boxplot(
      x = perc.phase0b.long,
      plot.path = plot.path,
      extension = extension,
      phase = 'phase0b',
      gotham.font = TRUE,
      variable.names = c('Exercise\nAttendance', 'Watch', 'Blood\nPressure', 'Scale')
      );
    }
  );
