#' Builds linear mixed models for physiological and lifestyle state endpoints
#'
#' @param daily.summary data frame which contains `nday` column for study day and each of the variables in `physiological.vars` and `state.vars`
#' @param random.slopes Should random slope be used per patient for `nday`? If the model is singular with a random slope then drop it from the model and refit.
#' @param physiological.vars All of the physiological variables to build a LMM for
#' @param physiological.vars All of the lifestyle state variables to build a LMM for
#' @return a list with `models` which contains the models and `model.summary` which contains a data frame summarising the model results
model.linear.daily.summary <- function(
  daily.summary,
  random.slopes = TRUE,
  physiological.vars = c(
    'rest.hr.sleep.mean',
    'rest.cgm.sleep.mean',
    'mass',
    'fat.proportion',
    'systolic',
    'diastolic'
  ),
  state.vars = paste0("state.alt", c("Sleep", "Active", "Sedentary"))
  ) {
  mods <- lapply(c(state.vars, physiological.vars), function(v) {
    formula.intercept <- as.formula(sprintf('scale(%s) ~ nday + Age.at.Consent + (1 | patient)', v));
    if(random.slopes) {
      formula <- as.formula(sprintf('scale(%s) ~ nday + Age.at.Consent + (1 + scale(nday) || patient)', v));
      }
    else {
      formula <- formula.intercept;
      }

    mod <- lmerTest::lmer(formula, data = daily.summary, REML = TRUE);
    if(isSingular(mod)) {
      mod <- lmerTest::lmer(formula.intercept, data = daily.summary, REML = TRUE);
      }
    return(mod);
    });
  names(mods) <- c(state.vars, physiological.vars);

  mods.summary <- daily.summary.model.summary(mods);
  mods.summary <- mods.summary[mods.summary$coefficient == "nday", ];
  mods.varcomp.summary <- daily.summary.varcomp.summary(mods);

  mods.summary$qvalue <- p.adjust(mods.summary$p_value);
  mods.summary$var.type <- ifelse(grepl('state\\.', mods.summary$variable), 'State', 'Physiological');
  mods.summary$variable <- c(state.vars, physiological.vars);
  list(
    models = mods,
    model.summary = merge(mods.summary, mods.varcomp.summary, by = 'variable')
    );
  }

#' Create a summary data frame for the fixed effects of a set of daily summary models
#' @param cohort.models Daily summary models for physiological/lifestyle state variables
daily.summary.model.summary <- function(cohort.models) {
  do.call('rbind.data.frame', lapply(names(cohort.models), function(variable.name) {
    m <- cohort.models[[variable.name]];
    m.summary <- summary(m);
    raw.coefs <- as.data.frame(m.summary$coefficients);
    # if(!("watch_on_minutes" %in% colnames(raw.coefs))) {
    #   raw.coefs['watch_on_minutes', ] <- NA;
    #   }
    if('df' %in% colnames(raw.coefs)) {
      raw.coefs$df <- NULL;
    }
    colnames(raw.coefs) <- c('estimate', 'std_error', 't_value', 'p_value');
    coef.df <- data.frame(raw.coefs, variable = variable.name);
    coef.df$coefficient <- rownames(coef.df);
    rownames(coef.df) <- NULL;

    coef.df;
  }));
}

#' Create a summary data frame for the variance components of a set of daily summary models
#' @param cohort.models Daily summary models for physiological/lifestyle state variables
daily.summary.varcomp.summary <- function(cohort.models) {
  do.call(
    what = plyr::rbind.fill,
    args = lapply(names(cohort.models), function(variable.name) {
      m <- cohort.models[[variable.name]];
      varcomp <- as.data.frame(VarCorr(m));
      varcomp$id <- 1;
      varcomp$name <- gsub(
        pattern = '\\.1',
        replacement =  '.slope',
        x = varcomp$grp
      );

      varcomp.wide <- reshape(
        data = varcomp[, c('id', 'name', 'vcov', 'sdcor')],
        idvar = "id",
        timevar = "name",
        direction = "wide"
      );
      # Adjusted and conditional ICC
      if(requireNamespace("performance", quietly = TRUE)) {
        icc.results <- performance::icc(m);
        if(length(icc.results) > 1 && !is.na(icc.results)) {
          varcomp.wide <- cbind.data.frame(varcomp.wide, c(icc.results));
          }
        }

      varcomp.wide$variable <- variable.name;
      # Drop dummy id column
      varcomp.wide[, -1];
      })
    );
  }
