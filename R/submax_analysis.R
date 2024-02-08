#' Runs the submax t-test analysis and plotting
#'
#' @param submax.long.data data frame with columns Session (values BL/FU), Time.to.submax, Study.ID (patient id)
#' @param plot.path path where plots should be plotted
#' @param extension extension for the plots
#' @param color.points Should the point/lines be colored according to the Cancer.Type column?
#' @param use.gotham.font Should Gotham font be used?
#' @param ... Additional parameters for create.scatterplot
#'
#' @return invisible submax.plot
#' @export
submax.analysis <- function(
  submax.long.data,
  plot.path,
  extension = 'png',
  color.points = FALSE,
  use.gotham.font = FALSE,
  phase0b.only = TRUE,
  ...) {

  suffix <- '_fullcohort';
  if (phase0b.only) {
    submax.long.data <- submax.long.data[! submax.long.data$Study.ID %in% c('EX001', 'EX002', 'EX003'), ];
    suffix <- '_phase0b.only';
  }

  bl.submax <- submax.long.data$Time.to.submax[submax.long.data$Session == 'BL'];
  fu.submax <- submax.long.data$Time.to.submax[submax.long.data$Session == 'FU'];

  submax.long.data$Time.to.submax.minutes <- submax.long.data$Time.to.submax / 60;

  fontfamily <- if (use.gotham.font) gotham.font else BoutrosLab.plotting.general::get.defaults(property = 'fontfamily');
  key <- NULL;
  if (color.points) {
    key <- list(
      text =  list(
        lab = names(cancer.type.colors)
        ),
      lines = list(
        pch = 19,
        type = 'b',
        size = 3,
        cex = 0.75,
        col = unlist(cancer.type.colors)
        ),
      fontfamily = fontfamily,
      border = TRUE,
      divide = 1,
      corner = c(0.05, 0.05)
      );
    }

  filename <- file.path(
    plot.path,
    generate.filename('digIT-EX', file.core = paste0('full_cohort_submax', suffix), extension = extension)
    );
  print(sprintf('Plotting to: %s', filename));

  submax.plot <- create.scatterplot(
    Time.to.submax.minutes ~ as.factor(Session),
    data = submax.long.data,
    groups = submax.long.data$Study.ID,
    col = if (color.points) submax.long.data$plot.color[submax.long.data$Session == 'BL'] else 'black',
    group.specific.colouring = color.points,
    type = 'b',
    main.cex = 1.75,
    ylimits = c(4, 15),
    ylab.cex = 1.5,
    xaxis.lab = c('Baseline', 'Follow-up'),
    ylab.label = 'Time to Submax (minutes)',
    xaxis.cex = 1.25,
    ylab.axis.padding = 3,
    xlab.label = '',
    resolution = 200,
    key = key,
    ...
    );

  t.test.results <- t.test(
    x = fu.submax,
    y = bl.submax,
    paired = TRUE,
    alternative = 'two.sided'
    );

  cohens.d <- lsr::cohensD(fu.submax, bl.submax, method = 'paired');

  t.test.ci <- t.test.results$conf.int;
  t.test.estimate <- unname(t.test.results$estimate);
  test.text.labels <- c(
    # sprintf('t-test p = ', t.test.results$p.value),
    sprintf('mean difference (seconds): %.1f', t.test.estimate),
    sprintf('95%% CI [%.1f, %.1f]', t.test.ci[1], t.test.ci[2]),
    sprintf('Cohen\'s d = %.2f', cohens.d)
    );

  t.test.text <- latticeExtra::layer(
    panel.text(
      x = 2,
      y = .y,
      labels = .labels,
      cex = 0.85,
      fontface = 'bold',
      fontfamily = .fontfamily
      ),
      # Needs to be passed in via data rather than directly to panel
      data = list(
        .labels = test.text.labels,
        .fontfamily = fontfamily,
        .y = 7 - seq_along(test.text.labels) * 0.5
        ),
      ...
    );

  if (use.gotham.font) {
    submax.plot <- replace.font(submax.plot, font = gotham.font);
    }

  height <- 6;
  width <- 6;

  submax.plot <- submax.plot + t.test.text;
  BoutrosLab.plotting.general::write.plot(
    trellis.object = submax.plot,
    filename = filename,
    height = height,
    width = width
    );

  invisible(submax.plot);
  }

submax.delta.summary <- function(submax.long.data, dosage, results.path) {

  doses <- sort(unique(dosage$dose), na.last = NA)

  submax.long.data$dose.fct <- factor(submax.long.data$dose, levels = doses)

  dose.diff.model <- lm(delta ~ 0 + dose.fct, data = submax.long.data)
  dose.diff.table <- cbind.data.frame(dose = doses, summary(dose.diff.model)$coefficient)

  dose.diff.model.all <- lm(delta ~ 1, data = wide.phase1.0b.submax)
  dose.diff.table.all <- cbind.data.frame(dose = 'All', summary(dose.diff.model.all)$coefficient)
  dose.diff.table <- rbind(dose.diff.table, dose.diff.table.all)

  # reformat table for output
  dose.diff.table$dose.fct <- factor(dose.diff.table$dose, levels = c('All', doses))
  dose.diff.table <- dose.diff.table[order(dose.diff.table$dose.fct), ]
  dose.diff.string <- paste0(round(dose.diff.table$Estimate, 1),
                              ' (', round(dose.diff.table$Estimate - dose.diff.table[, 'Std. Error'] * 1.96, 1),
                              ' - ', round(dose.diff.table$Estimate + dose.diff.table[, 'Std. Error'] * 1.96, 1),
                              ')')
  dose.diff.df <- as.data.frame(matrix(dose.diff.string, ncol = length(dose.diff.string)))
  colnames(dose.diff.df) <- paste0('Dose: ', dose.diff.table$dose, ' (minutes)')
  dose.diff.df <- cbind(Variable = 'Seconds to Submax', dose.diff.df)

  filename <- print(file.path(
      results.path,
      generate.filename(
          'phase1_0b-prostate', 'submax_diff_mean_ci', 'tsv'
          )
      ))
  write.table(dose.diff.df, filename, quote = F, sep = '\t', row.names = F, col.names = T)

}
