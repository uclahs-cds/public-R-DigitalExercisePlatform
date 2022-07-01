library(DigITx);
library(BoutrosLab.plotting.general);

script.name <- 'adherence';
data.folder <- Sys.getenv('DIGITX_HOME');
if(data.folder == "") data.folder <- here::here('results');

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    extension <- 'png';
    plot.path <- file.path(data.folder, 'plots', script.name);

    # Exercise adherence and adherence to health devices for the 3 patients in phase 0a.
    # Boxplot with the points shown (stripplot with a barplot overlaid), with columns for each data-type, and then outlier points labeled with their patient numbers
    # Make a copy of the adherence data for Phase 0a patients
    perc.phase0a.long <- adherence.perc.phase0a;
    perc.phase0b.long <- adherence.perc.phase0b;

    # Order by median percentage
    perc.phase0a.long$Variable.factor <- factor(
      perc.phase0a.long$Variable,
      levels = unique(perc.phase0a.long$Variable)[order(unlist(lapply(perc.phase0a, median)), decreasing = TRUE)]
      );

    create.boxplot(
      formula = Percent ~ Variable.factor,
      data = perc.phase0a.long,
      add.stripplot = TRUE,
      xlab.label = '',
      points.cex = 1,
      ylab.cex = 1.75,
      ylab.axis.padding = 3,
      height = 8,
      width = 12,
      filename = file.path(
        plot.path,
        generate.filename('digIT-EX', file.core = 'phase0a_adherence', extension = extension)
        )
      );
    }
  );
