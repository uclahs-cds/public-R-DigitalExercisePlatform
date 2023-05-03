library(EXONC.DEXP);
library(BoutrosLab.plotting.general);

script.name <- 'daily_summary_states';
data.folder <- Sys.getenv('EXONC_HOME');
if (data.folder == '') data.folder <- 'DEXP_results';
plot.path <- file.path(data.folder, 'plots', script.name);

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    daily.summary <- read.table(
      system.file('extdata', 'daily_summary.tsv', package = 'EXONC.DEXP'),
      sep = '\t',
      header = TRUE
      );

    daily.summary.percentile.plot(
      daily.summary = daily.summary,
      plot.path = plot.path,
      use.gotham.font = FALSE,
      extension = 'png'
      );
    }
  );
