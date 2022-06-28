#' Creates the estimated study hours plot for DigITx

#' @param cost.data the cost data frame.
#' @param extension the extension for the plot
#'
#' @export
plot.study.hours <- function(cost.data, plot.path, extension, ...) {
  digitx.hours <- cbind(cost.data[, c('Patient', 'patient.id', 'Digitx.Time.total.hrs')], study = 1);
  colnames(digitx.hours) <- c('Patient', 'patient.id', 'hours', 'study');
  trad.hours <- cbind(cost.data[, c('Patient', 'patient.id', 'Traditional.Time.total.hrs')], study = 0);
  colnames(trad.hours) <- c('Patient', 'patient.id', 'hours', 'study');

  time.df <- rbind(
    digitx.hours,
    trad.hours
    );

  time.df$phase0b <- ifelse(as.numeric(time.df$Patient >= 4), TRUE, FALSE);

  create.boxplot(
    formula = hours ~ as.factor(study),
    data = time.df[time.df$phase0b == 1, ],
    add.stripplot = TRUE,
    ylab.label = 'Total Patient Hours',
    xlab.label = '',
    ylimits = c(-10, 175),
    filename = file.path(
      plot.path,
      generate.filename('ExOnc', file.core = 'phase0b_boxplot.study.hours', extension = extension)
      ),
      ...
    );
  }

#' Creates the estimated total cost boxplot plot for DigITx

#' @param cost.data the cost data frame.
#' @param extension the extension for the plot
#'
#' @export
plot.total.cost <- function(cost.data, plot.path, extension, ...) {
  phase.0b.costs <- cbind(cost.data[cost.data$phase0b, 'Total.Cost', drop = FALSE], group = 1);
  # Add the DigITx costs as '0'
  phase.0b.costs <- rbind(phase.0b.costs, c(0, 2));

  # phase.0b.costs
  create.boxplot(
    Total.Cost ~ as.factor(group),
    data = phase.0b.costs,
    add.stripplot = TRUE,
    ylab.label = 'Estimated Patient Cost ($)',
    xlab.label = '',
    filename = file.path(
      plot.path,
      generate.filename('ExOnc', file.core = 'phase0b_boxplot.total.cost', extension = extension)
    ),
    ...
  );
  }

#' Full cost/hour analysis
#' @param cost.data hours and cost data
cost_hour_analysis <- function(cost.data, plot.path) {
  # BPG Parameters
  bpg.shared <- list(
    points.cex = 1,
    ylab.cex = 1.75,
    ylab.axis.padding = 3,
    xaxis.lab = c('Traditional', 'DigITx'),
    extension = 'png'
    );

  cost.data$phase0b <- ifelse(as.numeric(cost.data$Patient >= 4), TRUE, FALSE);

  bpg.shared$cost.data <- cost.data;
  bpg.shared$plot.path <- plot.path;

  # Plot the study hours boxplot
  do.call(plot.study.hours, bpg.shared);

  # Plot the total study cost boxplot
  do.call(plot.total.cost, bpg.shared);
  }
