#' @export
remove.axis <- function(x, side = c('left', 'right', 'top', 'bottom')) {
  UseMethod('remove.axis');
  }

#' Removes a given axis from a trellis plot
#'
#' Note: Not recommended to use if a custom axis is in place
#'
#' @param x a trellis object
#' @param side one or more of `left`, `right`, `top`, `bottom`
#'
#' @return trellis object with new axis function
#' @exportS3Method remove.axis trellis
remove.axis.trellis <- function(x, side = c('left', 'right', 'top', 'bottom')) {
  user.side <- match.arg(side, several.ok = TRUE);
  .axis.func <- function(side, line.col, ...) {
    if (side == "left" && ! side %in% user.side) {
      grid.lines(
        x = c(0, 0),
        y = c(0, 1),
        default.units = "npc"
        )
    } else if (side == "right" && ! side %in% user.side) {
      grid.lines(
        x = c(1, 1),
        y = c(0, 1),
        default.units = "npc"
        )
    } else if (side == "bottom" && ! side %in% user.side) {
      grid.lines(
        x = c(0, 1),
        y = c(0, 0),
        default.units = "npc"
        )
    } else if (side == 'top' && ! side %in% user.side) {
      grid.lines(
        x = c(0, 1),
        y = c(0, 0),
        default.units = "npc"
        )
    }
    axis.default(side = side, line.col = "black", ...)
    }
    x$axis <- .axis.func;
    x$par.settings$axis.line$col <- 'transparent';
    return(x);
}
