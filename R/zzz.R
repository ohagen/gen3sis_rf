# Copyright (c) 2020, ETH Zurich

## Minimal package startup and namespace helpers.
## Keep side-effects out of .onLoad; only register imports and declare globals
## that avoid spurious R CMD check notes.

#' @importFrom methods new
#' @importFrom stats na.omit
#' @importFrom graphics title
NULL

## Declare non-standard-evaluation variables used across plotting helpers
## to avoid 'no visible binding for global variable' notes during R CMD check.
utils::globalVariables(c(
	"value", "title", "x", "y", "shape", "space"
))

.onLoad <- function(libname, pkgname) {
	# Intentionally minimal. Do not perform heavy work here.
	# Use .onAttach only for user-facing messages (kept empty for CRAN friendliness).
	invisible()
}

.onAttach <- function(libname, pkgname) {
	if (interactive()) {
		packageStartupMessage(
			"gen3sis2: General Engine for Eco-Evolutionary SimulationS2.\n",
			"Copyright (c) 2020, ETH Zurich.\n",
			"This program comes with ABSOLUTELY NO WARRANTY; it is free software and",
			" you are welcome to contribute and redistribute it under certain conditions.\n"
		)
	}
}
