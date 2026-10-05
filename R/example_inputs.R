#' Copy the example inputs into an analysis folder
#'
#' Creates an `inputs/` folder in `path` holding the example input files that
#' come with the package: the two transition matrices, the cohort, entry
#' states, costs, utilities, `settings.yaml` and a README describing them. Run
#' it once in a new analysis folder, then run [tm_run()] there, or replace the
#' files with your own (same names and layout).
#'
#' The example iABC matrix, costs and utilities are invented; see the README.
#'
#' @param path The analysis folder. Defaults to the working directory.
#' @param overwrite If `FALSE` (the default), stops when any of the files already
#'   exist in `path/inputs/`, so your own files are never replaced by accident.
#' @return The paths of the copied files, invisibly.
#' @export
#' @examples
#' analysis <- file.path(tempdir(), "my_analysis")
#' tm_example_inputs(analysis)
#' list.files(file.path(analysis, "inputs"))
tm_example_inputs <- function(path = ".", overwrite = FALSE) {
  source_dir <- system.file("extdata", "example_inputs", package = "affirmoTM")
  if (!nzchar(source_dir)) {
    stop("The example inputs were not found in the installed package.", call. = FALSE)
  }
  files <- list.files(source_dir)
  target_dir <- file.path(path, "inputs")
  existing <- files[file.exists(file.path(target_dir, files))]
  if (length(existing) > 0 && !overwrite) {
    stop("These files already exist in ", target_dir, ": ", paste(existing, collapse = ", "),
         ". Use overwrite = TRUE to replace them.", call. = FALSE)
  }
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  copied <- file.path(target_dir, files)
  ok <- file.copy(file.path(source_dir, files), copied, overwrite = TRUE)
  if (!all(ok)) {
    stop("Could not copy: ", paste(files[!ok], collapse = ", "), call. = FALSE)
  }
  message("Example inputs copied to ", normalizePath(target_dir), ".")
  invisible(copied)
}
