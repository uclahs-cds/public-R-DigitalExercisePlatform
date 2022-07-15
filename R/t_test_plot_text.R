#' Generates a lattice layer with plot text used to change fontfamily in BPG plots
#'
#' @param panel.x X Position on original plot to draw text
#' @param panel.y Y Position on original plot to draw text
#' @param paired Is the data paired?
#' @param use.gotham.font Should gotham font be used?
#' @param ... Passed into both stats::t.test and lsr::cohensD
#'
#' @return latticeExtra::layer with panel of t-test result text
#' @export
t.test.plot.text <- function(
    panel.x,
    panel.y,
    paired = FALSE,
    use.gotham.font = TRUE,
    ...
    ) {
  method <- if (paired) 'paired' else 'pooled'

  t.test.results <- stats::t.test(
    paired = paired,
    ...
    );

  cohens.d <- lsr::cohensD(
    method = method,
    ...
    );


  t.test.ci <- t.test.results$conf.int;
  t.test.estimate <- unname(t.test.results$estimate);
  if (! paired) {
    t.test.estimate <- t.test.estimate[1] - t.test.estimate[2];
    }

  # Always reports positive, make it have the same sign as mean difference
  cohens.d <- cohens.d * sign(t.test.estimate);

  test.text.labels <- c(
    sprintf('t-test p = %.5f', t.test.results$p.value),
    sprintf('mean difference (hours): %.1f', t.test.estimate),
    sprintf('95%% CI [%.1f, %.1f]', t.test.ci[1], t.test.ci[2]),
    sprintf("Cohen's d = %.1f", cohens.d)
    );

  test.text.labels <- paste0(test.text.labels, collapse = '\n');

  if (use.gotham.font) {
    fontfamily <- gotham.font;
    }
  else {
    fontfamily <- BoutrosLab.plotting.general::get.defaults(property = 'fontfamily');
    }

  t.test.text <- latticeExtra::layer(
    panel.text(
      x = .panel.x,
      y = .panel.y,
      labels = .labels,
      cex = 0.75,
      fontface = 'bold',
      fontfamily = .fontfamily
      ),
    # Needs to be passed in via data rather than directly to panel
    # Scoping is weird with layer
    data = list(
      .panel.x = panel.x,
      .panel.y = panel.y,
      .labels = test.text.labels,
      .fontfamily = fontfamily
      ),
    ...
    );
  }
