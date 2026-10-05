# Reference levels of the SNPR multinomial model. They have no column in the design
# matrix, so they cannot be read from the files.
ref_from_state <- "NoEvent"
ref_sex <- "women"
ref_agegrp <- "65-69"
ref_cluster <- "Unspecified"

# Prefixes of the dummy columns in the design matrix.
prefix_from_state <- "CurrentEvent"
prefix_agegrp <- "age_group"
prefix_cluster <- "mypredclass7"

# States that appear only in the entry-state matrix, not in the transition model.
entry_only_states <- c("PIS_PHS", "DTH")

default_files <- c(
  coef    = "Present_coef_model.rds",
  vcov    = "Present_vcov_model.rds",
  design  = "Present_design_matrix.rds",
  initial = "Initial_Transition.csv"
)

#' Load the default transition model
#'
#' Reads the default model shipped with the package: the coefficients, the
#' variance-covariance matrix and the design matrix of the SNPR multinomial
#' transition model, and the entry-state matrix. Each input is checked for
#' consistency with the others, and an error names any mismatch.
#'
#' The model is one optional source of transition probabilities; see
#' [tm_matrix_from_model()]. A transition matrix can also be supplied directly
#' with [tm_matrix()].
#'
#' @param path Folder holding the four files. Defaults to the copies installed
#'   with the package. Point it elsewhere to use a refitted model with the same
#'   file names and layout.
#'
#' @return An object of class `tm_model`: a list with
#'   * `coef`: coefficient matrix, one row per next state (`NoEvent` is the
#'     reference and has no row), one column per model term.
#'   * `vcov`: variance-covariance matrix of `coef`, ordered outcome by outcome.
#'   * `design`: design matrix, one row per stratum.
#'   * `strata`: `data.table` decoding each `design` row into `from`, `sex`,
#'     `agegrp` and `mm_cluster`.
#'   * `initial`: `data.table` of entry-state probabilities by `mm_cluster`,
#'     `sex` and `agegrp`.
#'   * `path`: the folder the files were read from.
#' @export
#' @examples
#' model <- tm_default_model()
#' model
#' head(model$strata)
tm_default_model <- function(path = system.file("extdata", package = "affirmoTM")) {
  files <- file.path(path, default_files)
  names(files) <- names(default_files)
  missing <- files[!file.exists(files)]
  if (length(missing) > 0) {
    stop("Default input file(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  coef <- readRDS(files[["coef"]])
  vcov <- readRDS(files[["vcov"]])
  design <- readRDS(files[["design"]])
  initial <- data.table::fread(files[["initial"]])

  check_coef_vcov_design(coef, vcov, design)
  strata <- decode_design(design)
  check_initial(initial, strata, to_states = c(ref_from_state, rownames(coef)))

  structure(
    list(coef = coef, vcov = vcov, design = design, strata = strata, initial = initial, path = path),
    class = "tm_model"
  )
}

check_coef_vcov_design <- function(coef, vcov, design) {
  if (!is.matrix(coef) || is.null(rownames(coef)) || is.null(colnames(coef))) {
    stop("`coef` must be a matrix with next states as row names and model terms as column names.", call. = FALSE)
  }
  if (!identical(colnames(design), colnames(coef))) {
    stop("Design matrix columns do not match the coefficient terms.", call. = FALSE)
  }
  # vcov must follow the order of as.vector(t(coef)): all terms of the first outcome, then the next.
  expected <- as.vector(t(outer(rownames(coef), colnames(coef), paste, sep = ":")))
  if (!identical(colnames(vcov), expected) || !identical(rownames(vcov), expected)) {
    stop("vcov names do not follow the coefficient order (outcome by outcome, as 'state:term').", call. = FALSE)
  }
  # The default vcov is symmetric only up to floating-point noise (largest difference 2e-7
  # against entries up to 627), so symmetry is checked with a relative tolerance.
  if (!isSymmetric(unname(vcov), tol = 1e-8)) {
    stop("vcov is not symmetric (relative tolerance 1e-8).", call. = FALSE)
  }
  invisible(TRUE)
}

# Turns the 0/1 dummy columns of the design matrix back into labels, by column name.
decode_design <- function(design) {
  level_of <- function(prefix, reference) {
    cols <- colnames(design)[startsWith(colnames(design), prefix)]
    dummies <- design[, cols, drop = FALSE]
    n_on <- rowSums(dummies)
    if (any(!n_on %in% c(0, 1))) {
      stop("Design matrix rows have more than one '", prefix, "' dummy set.", call. = FALSE)
    }
    out <- rep(reference, nrow(design))
    on <- n_on == 1
    out[on] <- substring(cols, nchar(prefix) + 1)[max.col(dummies[on, , drop = FALSE], ties.method = "first")]
    out
  }

  strata <- data.table::data.table(
    from = level_of(prefix_from_state, ref_from_state),
    sex = ifelse(design[, "sexM"] == 1, "men", ref_sex),
    agegrp = level_of(prefix_agegrp, ref_agegrp),
    mm_cluster = level_of(prefix_cluster, ref_cluster)
  )
  if (anyDuplicated(strata) > 0) {
    stop("Design matrix has duplicated strata.", call. = FALSE)
  }
  strata
}

check_initial <- function(initial, strata, to_states) {
  id_cols <- c("mm_cluster", "sex", "agegrp")
  if (!all(id_cols %in% names(initial))) {
    stop("Entry-state matrix needs columns: ", paste(id_cols, collapse = ", "), call. = FALSE)
  }
  state_cols <- setdiff(names(initial), id_cols)
  unknown <- setdiff(state_cols, c(to_states, entry_only_states))
  if (length(unknown) > 0) {
    stop("Entry-state matrix has unknown states: ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  probs <- as.matrix(initial[, state_cols, with = FALSE])
  if (anyNA(probs) || any(probs < 0 | probs > 1)) {
    stop("Entry-state probabilities must be between 0 and 1, with no missing values.", call. = FALSE)
  }
  bad_rows <- which(abs(rowSums(probs) - 1) > 1e-8)
  if (length(bad_rows) > 0) {
    stop("Entry-state rows do not sum to 1: rows ", paste(bad_rows, collapse = ", "), call. = FALSE)
  }

  if (anyDuplicated(initial[, id_cols, with = FALSE]) > 0) {
    stop("Entry-state matrix has duplicated strata.", call. = FALSE)
  }
  for (col in id_cols) {
    extra <- setdiff(initial[[col]], strata[[col]])
    if (length(extra) > 0) {
      stop("Entry-state matrix `", col, "` has labels not in the transition model: ",
           paste(extra, collapse = ", "), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' @export
print.tm_model <- function(x, ...) {
  s <- x$strata
  init_states <- setdiff(names(x$initial), c("mm_cluster", "sex", "agegrp"))
  cat("<tm_model> affirmoTM transition model (coefficients + vcov)\n")
  cat("  Read from:      ", x$path, "\n")
  cat("  Coefficients:   ", nrow(x$coef), "next states x", ncol(x$coef), "terms (reference: NoEvent)\n")
  cat("  Vcov:           ", nrow(x$vcov), "x", ncol(x$vcov), "\n")
  cat("  Design:         ", nrow(x$design), "strata =",
      data.table::uniqueN(s$from), "current states x", data.table::uniqueN(s$sex), "sexes x",
      data.table::uniqueN(s$agegrp), "age groups x", data.table::uniqueN(s$mm_cluster), "clusters\n")
  cat("  Entry states:   ", nrow(x$initial), "strata x", length(init_states), "states\n")
  next_states <- c(ref_from_state, rownames(x$coef))
  agegrps <- unique(s$agegrp)
  cat("  Current states: ", paste(intersect(next_states, s$from), collapse = ", "), "\n")
  cat("  Next states:    ", paste(next_states, collapse = ", "), "\n")
  cat("  Age groups:     ", paste(agegrps[order(as.integer(sub("-.*", "", agegrps)))], collapse = ", "), "\n")
  cat("  Clusters:       ", paste(sort(unique(s$mm_cluster)), collapse = ", "), "\n")
  invisible(x)
}
