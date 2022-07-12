library(DigITx);
library(BoutrosLab.plotting.general);

script.name <- 'daily_summary_states';
data.folder <- Sys.getenv('DIGITX_HOME');
if (data.folder == '') data.folder <- here::here('results');
plot.path <- file.path(data.folder, 'plots', script.name);

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    daily.summary <- read.table(
      here::here('inst/data-raw/daily_summary.tsv'),
      sep = '\t',
      header = TRUE
      );

    daily.summary.percentile.plot(
      daily.summary = daily.summary,
      plot.path = plot.path,
      use.gotham.font = TRUE,
      extension = 'png'
      );
    }
  );
