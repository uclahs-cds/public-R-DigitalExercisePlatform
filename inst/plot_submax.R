library(EXOC.DPEx);
library(BoutrosLab.plotting.general);

script.name <- 'submax_analysis';
data.folder <- Sys.getenv('EXONC_HOME');
if (data.folder == '') data.folder <- 'DEXP_results';
plot.path <- file.path(data.folder, 'digIT-EX', 'plots', script.name);

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    submax.data <- read.table(
      system.file('extdata', 'submax.tsv', package = 'EXOC.DPEx'),
      sep = '\t',
      header = TRUE
      );

    submax.long.data <- read.table(
      system.file('extdata', 'submax_long.tsv', package = 'EXOC.DPEx'),
      sep = '\t',
      header = TRUE
      );

    submax.analysis(
      submax.long.data = submax.long.data,
      plot.path = plot.path,
      color.points = TRUE,
      use.gotham.font = FALSE,
      phase0b = TRUE,
      extension = 'png'
      );
    }
  );
