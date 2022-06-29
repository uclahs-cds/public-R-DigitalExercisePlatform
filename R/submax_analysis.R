submax_analysis <- function(submax.long.data, plot.path, extension = 'png', color.points = FALSE) {
  t_test_results <- t.test(
    x = submax.long.data$Time.to.submax[submax.long.data$Session == "BL"],
    y = submax.long.data$Time.to.submax[submax.long.data$Session == "FU"],
    paired = TRUE,
    alternative = "two.sided"
    );
  test_text_labels <- paste0('t-test p = ', round(t_test_results$p.value, 3))

  create.scatterplot(
    Time.to.submax ~ as.factor(Session),
    data = submax.long.data,
    groups = submax.long.data$Study.ID,
    col = submax.long.data$plot.color[submax.long.data$Session == "BL"],
    type = 'b',
    main.cex = 1.75,
    ylimits = c(250, 900),
    ylab.cex = 1.5,
    xaxis.lab = c('Baseline', 'Follow-up'),
    ylab.label = 'Time to submax (seconds)',
    xaxis.cex = 1.25,
    ylab.axis.padding = 3,
    xlab.label = '',
    add.text = TRUE,
    text.x = 1.5,
    text.y = 300,
    text.cex = 1,
    resolution = 200,
    text.labels = test_text_labels,
    filename = file.path(
      plot.path,
      generate.filename('ExOnc', file.core = 'phase0b_submax', extension = extension)
      )
    );
  }
