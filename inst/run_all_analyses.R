library(DigITx);
library(BoutrosLab.plotting.general);

script.name <- 'run_all_analyses';
data.folder <- Sys.getenv('DIGITX_HOME');
if (data.folder == '') data.folder <- here::here('results');

analysis.init(
  data.folder = data.folder,
  script.name = script.name,
  split.stdout = TRUE,
  expr = {
    run.scripts <- here::here(list.files('inst', pattern = '*\\.R$', full.names = TRUE));
    # Remove this script
    run.scripts <- run.scripts[! grepl(script.name, run.scripts)];
    for (s in run.scripts) {
      source(s);
      }
    }
  )
