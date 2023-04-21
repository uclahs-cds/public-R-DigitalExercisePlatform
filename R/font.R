#' Named Gotham font
gotham.font <- 'iCiel Gotham Medium';

#' Replaces the font in a BPG object with Gotham Medium font
#'
#' @param x BPG plot
#' @param font Desired font
#' @return Same BPG plot but with all fonts replaced with Gotham Medium
replace.font <- function(x, font = gotham.font) {
  # Get the same default font
  default.fontfamily <- BoutrosLab.plotting.general::get.defaults(property = 'fontfamily');
  .replace.single <- function(y) {
    if (length(y) == 1 && y == default.fontfamily) {
      return(font);
      }
    else {
      return(y);
      }
    }

  # Apply recursively
  rapply(x, .replace.single, classes = 'character', how = 'replace');
  }
