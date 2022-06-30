## code to prepare `adherence.perc.phase0a` and `adherence.perc.phase0b` dataset goes here

adherence.phase0a <- read.table(here::here('inst/data-raw/adherence_phase0a.tsv'), header = TRUE, sep = '\t');
adherence.phase0b <- read.table(here::here('inst/data-raw/adherence_phase0b.tsv'), header = TRUE, sep = '\t');

adherence.perc.phase0a <- adherence.to.long(adherence.phase0a);
adherence.perc.phase0b <- adherence.to.long(adherence.phase0b);

usethis::use_data(adherence.perc.phase0a, overwrite = TRUE);
usethis::use_data(adherence.perc.phase0b, overwrite = TRUE);
