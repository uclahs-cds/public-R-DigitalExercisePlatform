#' Writes the total memory used, the total process time elapsed and the session info of the R session.
#'
#' @param filename Name of the file to write session profile information to. If stdout = TRUE, the session profile information is also printed to the screen.
#' @param stdout Print session profile information to screen. Default is FALSE.
#' @details Session profile includes memory used from gc(), process time elapsed from proc.time(), the output of ls() in the global environment, and the session info from sessionInfo()
#' @return No return value, just writes output to file (and possibly screen).
#'
#' @export
save.session.profile <- function (filename, stdout = FALSE) {
	sink(file = filename, split = stdout)
	cat("### Memory #########################################################################################\n")
	print(gc())
	cat("\n### Time ###########################################################################################\n")
	print(proc.time())
	cat("\n### Full list of objects ###########################################################################\n")
	print(ls.objects(order.by = "Size", n = length(ls(pos = 1))))
	cat("\n### Session Info ###################################################################################\n")
	print(sessionInfo())
	sink()
}

#' Improved listing of objects in R session
#'
#' @export
ls.objects <- function (pos = 1, order.by, decreasing = TRUE, n = 10)
{
	napply <- function(ls.names, fn) {
		sapply(ls.names, function(x) {
			fn(get(x, pos = pos))
		})
	}
	ls.names <- ls(pos = pos)
	obj.class <- napply(ls.names, function(x) {
		as.character(class(x))[1]
	})
	obj.mode <- napply(ls.names, mode)
	obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
	obj.prettysize <- napply(ls.names, function(x) {
		capture.output(print(object.size(x), units = "auto"))
	})
	obj.size <- napply(ls.names, object.size)
	obj.dim <- t(napply(ls.names, function(x) {
		as.numeric(dim(x))[1:2]
	}))
	if (length(obj.dim) > 0) {
		vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
		obj.dim[vec, 1] <- napply(ls.names, length)[vec]
		ls.results <- data.frame(obj.type, obj.size, obj.prettysize,
														 obj.dim)
		names(ls.results) <- c("Type", "Size", "PrettySize",
													 "Rows", "Columns")
		if (!missing(order.by)) {
			ls.results <- ls.results[order(ls.results[[order.by]],
																		 decreasing = decreasing), ]
		}
		return(head(ls.results, n))
	}
	else {
		return("Nothing to list")
	}
}

#' Creates a filename according to the date_project_core.extension lab standard.
#'
#' @param project.stem PARAM_DESCRIPTION
#' @param file.core PARAM_DESCRIPTION
#' @param extension PARAM_DESCRIPTION
#' @param file.date PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @export
generate.filename <- function (project.stem, file.core, extension, file.date = Sys.Date()) {
	file.name <- paste(project.stem, sep = '_')
	file.name <- paste(file.name, file.core, sep = '_')
	file.name <- paste(file.name, extension, sep = '.')
	if (file.date != FALSE) {
		file.name <- paste(file.date, file.name, sep = '_')
	}
	return(file.name)
}
