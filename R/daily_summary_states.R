#' Creates the 'Mean: XX' text for the multipanel percentile/boxplot for lifestyle states
#' @param text.labels
#' @param text.x
#' @param text.y
state.week.text <- function(
  text.labels,
  text.x,
  text.y,
  max.study.day = 49
  ) {
  latticeExtra::layer(
    panel.text(
      x = .text.x,
      y = .text.y,
      fontfamily = gotham.font,
      labels = .labels,
      cex = 1.5
      ),
      data = list(
        # I don't really know how layer environment works
        # I need to name it something different than a variable in this environment
        .labels = text.labels,
        .text.x = text.x,
        .text.y = text.y
      )
    );
  }

#' Percentile plot
#' @export
daily.summary.percentile.plot <- function(
  daily.summary,
  plot.path,
  extension = 'png',
  max.study.day = 49,
  watch.on.min = 1440 - 180,
  smooth.percentiles = 1,
  use.gotham.font = TRUE,
  mean.line = FALSE
  ) {
  daily.summary.watch.on <- daily.summary[with(daily.summary, nday <= max.study.day & watch.on.minutes > watch.on.min), ];

  daily.summary.watch.on$patient_factor <- as.factor(daily.summary.watch.on$patient);

  state.percentile.plots <- lapply(c('Sleep', 'Active', 'Sedentary'), function(v) {
    colname <- sprintf('state.alt%s', v)
    percentile.formula <- as.formula(sprintf('%s ~ as.factor(nday)', colname))
    max.study.day <- max(daily.summary.watch.on$nday)

    # Moving mean text into percentile plots
    scatter.formula <- as.formula(sprintf('%s / 60 ~ nday', colname));
    week.agg.formula <- as.formula(sprintf('%s ~ nweek', colname));
    week.mean <- aggregate(week.agg.formula, daily.summary, mean);
    # Hours rather than minutes
    week.mean[[colname]] <- week.mean[[colname]] / 60;

    text.xat <- seq(3.5, max.study.day, by = 7);
    text.labels <- sprintf('Mean: %.01f', week.mean[[colname]]);

    agg.formula <- as.formula(sprintf('%s ~ nday', colname))
    study.day.percentiles <- do.call(
      what = 'cbind.data.frame',
      args = aggregate(
        formula = agg.formula,
        data = daily.summary.watch.on,
        FUN = function(x) {
          c(quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) / 60,
            mean = mean(x) / 60)
          }
        )
      );

    study.day.percentiles[, -1] <- lapply(study.day.percentiles[, -1], slider::slide_mean, before = smooth.percentiles, after = smooth.percentiles)

    xat <- c(1, seq(7, max.study.day, by = 7))
    if (v == 'Sedentary') {
      xaxis.lab <- xat
      xaxis.tck <- c(1, 0)
    } else {
      xaxis.lab <- rep('', length(xat))
      xaxis.tck <- 0
    }
    ylimits <- c(-1, ceiling(max(daily.summary.watch.on[[colname]] / 60, na.rm = TRUE) + 1));

    agg.formula.hours <- as.formula(sprintf('%s / 60 ~ nday', colname));

    lifestyle.plot <- create.scatterplot(
      agg.formula.hours,
      data = daily.summary.watch.on,
      col = 'darkgrey',
      alpha = 0.5,
      xat = xat,
      xaxis.lab = xaxis.lab,
      xaxis.tck = xaxis.tck,
      xlimits = c(0, max.study.day + 1),
      ylimits = ylimits,
      ylab.cex = 1.75,
      xlab.label = '',
      ylab.label = paste0(v, ', Hours'),
      abline.v = c(1, seq(7, max.study.day, by = 7)),
      abline.lty = 2,
      abline.col = 'grey',
      # Extra padding for weekly mean text
      top.padding = 1,
      # Only use this for default font
      # add.text = FALSE,
      # text.x = text.xat,
      # text.y = ylimits[2] + 0.3,
      # text.labels = text.labels
      );

    # Add the text at the same relative distance above plot
    text.y <- ylimits[2] + (ylimits[2] - ylimits[1]) * 0.05;

    # Add the gotham font weekly text
    lifestyle.plot <- lifestyle.plot + state.week.text(
      text.labels,
      text.x = text.xat,
      text.y = text.y);

    # Add the 5% to 95% percentiles
    lifestyle.plot <- lifestyle.plot +
      create.polygonplot(
        NA ~ nday,
        data = study.day.percentiles,
        min = study.day.percentiles[, paste0(colname, '.5%')],
        max = study.day.percentiles[, paste0(colname, '.95%')],
        groups = rep(1, length(study.day.percentiles$nday)),
        col = simplified.state.colour.scheme[[v]],
        alpha = 0.25,
        border.col = 'transparent',
        grid.col = 'transparent'
        )

    # Add the 25% to 75% percentiles
    lifestyle.plot <- lifestyle.plot +
      create.polygonplot(
        NA ~ nday,
        data = study.day.percentiles,
        min = study.day.percentiles[, paste0(colname, '.25%')],
        max = study.day.percentiles[, paste0(colname, '.75%')],
        groups = rep(1, length(study.day.percentiles$nday)),
        col = simplified.state.colour.scheme[[v]],
        alpha = 0.5,
        border.col = 'transparent',
        grid.col = 'transparent'
        );

    if (mean.line) {
      lifestyle.plot <- lifestyle.plot +
        create.scatterplot(
          as.formula(paste0(colname, '.mean ~ nday')),
          data = study.day.percentiles,
          type = 'l',
          col = simplified.state.colour.scheme[[v]],
          lwd = 3
          );
      }

    # Allow text to be written outside of the panel
    # Needed for weekly mean text
    lifestyle.plot$par.settings$clip <- list(panel = FALSE);

    lifestyle.plot;
    });

  width <- 18;
  height <- 14;
  filename <- file.path(
    plot.path,
    generate.filename('digIT-EX', file.core = 'daily_percentile_states', extension = extension)
    );
  print(sprintf('Plotting to: %s', filename));

  plot.objects <- list(
      state.percentile.plots[[1]],
      state.percentile.plots[[2]],
      state.percentile.plots[[3]]
    );

  if (use.gotham.font) {
    plot.objects <- replace.font(plot.objects, font = gotham.font);
    }

  percentile.plot <- create.multipanelplot(
    plot.objects = plot.objects,
    plot.objects.heights = rep(1, 3),
    layout.width = 1,
    layout.height = 3,
    resolution = 600,
    width = width,
    height = height,
    y.spacing = 0,
    xlab.label = 'Study day'
    );

  if (use.gotham.font) {
    percentile.plot <- replace.font(percentile.plot, font = 'iCiel Gotham Medium');
    }

  BoutrosLab.plotting.general::write.plot(
    trellis.object = percentile.plot,
    filename = filename,
    height = height,
    width = width
    );

  invisible(percentile.plot);
  }
