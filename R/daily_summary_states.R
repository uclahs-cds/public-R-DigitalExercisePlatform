daily.summary.states.analysis <- function(
    daily.summary,
    plot.path,
    extension = 'png',
    max.study.day = 49,
    watch.on.min = 1440 - 180,
    color.points = FALSE
  ) {
  daily.summary.watch.on <- daily.summary[with(daily.summary, nday <= max.study.day & watch.on.minutes > watch.on.min), ];

  daily.summary.watch.on$patient_factor <- as.factor(daily.summary.watch.on$patient)

  state.boxplots <- lapply(c("Sleep", "Active", "Sedentary"), function(v) {
    colname <- sprintf("state.alt%s", v)
    boxplot.formula <- as.formula(sprintf("%s / 60 ~ as.factor(nday)", colname))
    max.study.day <- max(daily.summary.watch.on$nday)

    agg.formula <- as.formula(sprintf("%s ~ nday", colname))
    study.day.mean <- aggregate(agg.formula, daily.summary.watch.on, mean)
    study.day.mean$ma5 <- slider::slide_mean(study.day.mean[[colname]], before = 2, after = 2) / 60
    study.day.mean$ma7 <- slider::slide_mean(study.day.mean[[colname]], before = 3, after = 3) / 60

    week.agg.formula <- as.formula(sprintf("%s ~ nweek", colname))
    week.mean <- aggregate(week.agg.formula, daily.summary.watch.on, mean)
    # Hours rather than minutes
    week.mean[[colname]] <- week.mean[[colname]] / 60

    xat <- seq(0, max.study.day, by = 7)
    if(v == 'Sedentary') {
      xaxis.lab <- xat
      xaxis.tck <- c(1, 0)
      } else {
      xaxis.lab <- rep('', length(xat))
      xaxis.tck <- 0
      }

    create.boxplot(
      boxplot.formula,
      data = daily.summary.watch.on,
      col = scales::alpha(simplified.state.colour.scheme[[v]], 0.7),
      ylab.label = v,
      xlab.label = '',
      xaxis.lab = xaxis.lab,
      xaxis.tck = xaxis.tck,
      xat = xat,
      abline.v = seq(0, max.study.day, by = 7),
      abline.lty = 2,
      abline.col = 'grey',
      # Add weekly mean text above plots
      text.x = seq(3.5, 49, length.out = 7),
      text.y = max(daily.summary.watch.on[[colname]]) + 1,
      text.labels = paste0('Mean: ', round(week.mean[[colname]], 1)),
      add.text = TRUE,
      add.stripplot = TRUE
    ) +
    create.scatterplot(
      ma7 ~ nday,
      data = study.day.mean,
      type = 'l',
      col = simplified.state.colour.scheme[[v]],
      lwd = 3
      );
    });

  # Just text indicating the means of the the weeks
  state.week_text <- lapply(c("Sleep", "Active", "Sedentary"), function(v) {
    colname <- sprintf("state.alt%s", v);
    scatter.formula <- as.formula(sprintf("%s / 60 ~ nday", colname));
    week.agg.formula <- as.formula(sprintf("%s ~ nweek", colname))
    week.mean <- aggregate(week.agg.formula, daily.summary.watch.on, mean)
    # Hours rather than minutes
    week.mean[[colname]] <- week.mean[[colname]] / 60

    xat <- seq(0, max.study.day, by = 7)
    xaxis.lab <- rep('', length(xat))
    create.scatterplot(
      formula = scatter.formula,
      data = daily.summary.watch.on,
      type = 'n',
      xaxis.tck = 0,
      xlab.label = '',
      ylab.label = '',
      ylimits = c(0, 0.1),
      axes.lwd = 0,
      xaxis.lab = xaxis.lab,
      yaxis.lab = NULL,
      xat = xat,
      text.cex = 1.75,
      xlimits = c(0, 50),
      text.x = seq(3.5, 49, by = 7),
      text.y = 0.05,
      text.labels = paste0('Mean: ', round(week.mean[[colname]], 1)),
      add.text = TRUE
      )
    })

  print(file.path(
    plot.path,
    generate.filename('digIT-EX', file.core = 'daily_boxplots_states', extension = extension)
  ))
  create.multipanelplot(
    plot.objects = list(
      state.week_text[[1]],
      state.boxplots[[1]],
      state.week_text[[2]],
      state.boxplots[[2]],
      state.week_text[[3]],
      state.boxplots[[3]]
      ),
    plot.objects.heights = rep(c(0.5, 1), 3),
    layout.width = 1,
    layout.height = 6,
    resolution = 400,
    width = 18,
    height = 14,
    y.spacing = -0.5,
    ylab.label = 'Hours',
    xlab.label = 'Study day',
    filename = file.path(
      plot.path,
      generate.filename('digIT-EX', file.core = 'daily_boxplots_states', extension = extension)
      )
    );
  }
