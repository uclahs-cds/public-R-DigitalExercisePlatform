model_linear_daily_summary <- function(daily.summary) {
  physiological.vars <- c(
    'rest.hr.sleep.mean',
    'rest.cgm.sleep.mean',
    'mass',
    'fat.proportion',
    'systolic',
    'diastolic'
    );
  state.vars <- paste0("state.alt", c("Sleep", "Active", "Sedentary"));

  mods <- lapply(c(state.vars, physiological.vars), function(v) {
    formula <- as.formula(sprintf('scale(%s) ~ nday + (1 | patient)', v));
    lmerTest::lmer(formula, data = daily.summary);
    });
  names(mods) <- c(state.vars, physiological.vars);

  mods.summary <- do.call('rbind.data.frame', lapply(mods, function(m) {
    summary(m)$coefficients['nday', c('Estimate', 'Pr(>|t|)')]
    }));
  colnames(mods.summary) <- c('estimate', 'pvalue');

  mods.summary$qvalue <- p.adjust(mods.summary$pvalue);
  mods.summary$var.type <- c(rep('State', length(state.vars)), rep('Physiological', length(physiological.vars)));
  mods.summary$variable <- c(state.vars, physiological.vars);
  mods.summary;
  }
