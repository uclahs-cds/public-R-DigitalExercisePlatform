## code to prepare `adherence.perc.phase0a` and `adherence.perc.phase0b` dataset goes here

adherence.phase0a <- read.table(here::here('inst/data-raw/adherence_phase0a.tsv'), header = TRUE, sep = '\t');
adherence.phase0b <- read.table(here::here('inst/data-raw/adherence_phase0b.tsv'), header = TRUE, sep = '\t');

cancer.types <- read.table(here::here('inst/data-raw/patient_cancer_types.tsv'), header = TRUE, sep = '\t');

# adherence.phase0b$Patient.ID <- c('EX004', 'EX005', 'EX006', 'EX008',
#                                   'EX010', 'EX011', 'EX012', 'EX014', 'EX015', 'EX016', 'EX017',
#                                   'EX018', 'EX019')

adherence.perc.phase0a <- adherence.to.long(adherence.phase0a);
adherence.perc.phase0b <- adherence.to.long(adherence.phase0b);

# Order by median percentage in phase0b
adherence.phase0b.perc.cols <- adherence.phase0b[, grepl('\\.Percent', colnames(adherence.phase0b))];
adherence.order <- c('Attendance', 'Watch', 'BP', 'Scale', 'Sleep');

adherence.perc.phase0a$Variable.factor <- factor(
  adherence.perc.phase0a$Variable,
  levels = adherence.order
  );

adherence.perc.phase0b$Variable.factor <- factor(
  adherence.perc.phase0b$Variable,
  levels = adherence.order
  );

adherence.perc.phase0a <- merge(adherence.perc.phase0a, cancer.types, by = 'Patient.ID', all.x = TRUE);
adherence.perc.phase0b <- merge(adherence.perc.phase0b, cancer.types, by = 'Patient.ID', all.x = TRUE);

usethis::use_data(adherence.perc.phase0a, overwrite = TRUE);
usethis::use_data(adherence.perc.phase0b, overwrite = TRUE);
