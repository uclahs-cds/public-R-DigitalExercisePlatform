#' Creates the estimated study hours plot for DigITx

#' @param cost_data the cost data frame.
#' @param extension the extension for the plot
#'
#' @export
plot_study_hours <- function(cost_data, extension, ...) {
  digitx_hours <- cbind(cost_data[, c('Patient', 'patient.id', 'Digitx.Time.total.hrs')], study = 1);
  colnames(digitx_hours) <- c('Patient', 'patient.id', 'hours', 'study');
  trad_hours <- cbind(cost_data[, c('Patient', 'patient.id', 'Traditional.Time.total.hrs')], study = 0);
  colnames(trad_hours) <- c('Patient', 'patient.id', 'hours', 'study');

  time_df <- rbind(
    digitx_hours,
    trad_hours
    );

  time_df$phase0b <- ifelse(as.numeric(time_df$Patient >= 4), TRUE, FALSE);

  create.boxplot(
    formula = hours ~ as.factor(study),
    data = time_df[time_df$phase0b == 1, ],
    add.stripplot = TRUE,
    ylab.label = 'Total Patient Hours',
    xlab.label = '',
    ylimits = c(-10, 175),
    filename = file.path(
      plot.path,
      generate.filename('ExOnc', file.core = 'phase0b_boxplot_study_hours', extension = extension)
      ),
      ...
    );
  }

#' Creates the estimated total cost boxplot plot for DigITx

#' @param cost_data the cost data frame.
#' @param extension the extension for the plot
#'
#' @export
plot_total_cost <- function(cost_data, extension, ...) {
  phase_0b_costs <- cbind(cost_data[cost_data$phase0b, 'Total.Cost', drop = FALSE], group = 1);
  # Add the DigITx costs as '0'
  phase_0b_costs <- rbind(phase_0b_costs, c(0, 2));

  # phase_0b_costs
  create.boxplot(
    Total.Cost ~ as.factor(group),
    data = phase_0b_costs,
    add.stripplot = TRUE,
    ylab.label = 'Estimated Patient Cost ($)',
    xlab.label = '',
    filename = file.path(
      plot.path,
      generate.filename('ExOnc', file.core = 'phase0b_boxplot_total_cost', extension = extension)
    ),
    ...
  );
  }

#' Full cost/hour analysis
#' @param cost_data hours and cost data
cost_hour_analysis <- function(cost_data) {
  # BPG Parameters
  bpg.shared <- list(
    points.cex = 1,
    ylab.cex = 1.75,
    ylab.axis.padding = 3,
    xaxis.lab = c('Traditional', 'DigITx'),
    extension = 'png'
    );

  cost_data$phase0b <- ifelse(as.numeric(cost_data$Patient >= 4), TRUE, FALSE);

  bpg.shared$cost_data <- cost_data;

  # Plot the study hours boxplot
  do.call(plot_study_hours, bpg.shared);

  # Plot the total study cost boxplot
  do.call(plot_total_cost, bpg.shared);
  }
