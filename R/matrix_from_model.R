#' Build a transition matrix from a fitted multinomial model
#'
#' Turns a multinomial logit transition model (coefficients, vcov and design
#' matrix, as loaded by [tm_default_model()]) into a [tm_matrix()]. With
#' `n_sim = 0` it returns the point estimate. With `n_sim > 0` it returns
#' `n_sim` probabilistic draws, numbered in a `sim` column.
#'
#' @details
#' Each draw samples all coefficients at once from a multivariate normal
#' distribution (mean = `coef`, covariance = `vcov`) and applies a softmax to
#' every design row, with `NoEvent` as the reference outcome.
#'
#' Before drawing, `vcov` is made exactly symmetric and its negative
#' eigenvalues are set to 0.
#'
#' Moves that the allowed-transitions table marks as impossible are set to 0
#' and each row is rescaled to sum to 1.
#'
#' The model has rows for the living states only. Each death state is given
#' rows that always go back to itself, as [tm_matrix()] requires.
#'
#' @param model A `tm_model`, from [tm_default_model()].
#' @param n_sim Number of probabilistic draws. 0 gives the point estimate.
#' @param seed Optional seed for the draws. Your own random-number state is
#'   restored afterwards.
#' @param allowed Allowed-transitions table: a `data.frame` or CSV path with a
#'   `from` column and one 0/1 column per next state. Defaults to the table
#'   shipped with the package. `NULL` applies no mask.
#'
#' @return A `tm_matrix` with stratifiers `agegrp`, `sex` and `mm_cluster`.
#'   `model_info` records the draws, the seed, whether the mask was applied,
#'   and what the vcov repair changed.
#' @export
#' @examples
#' point <- tm_matrix_from_model(tm_default_model())
#' point
tm_matrix_from_model <- function(model = tm_default_model(), n_sim = 0, seed = NULL,
                                 allowed = system.file("extdata", "allowed_transitions.csv",
                                                       package = "affirmoTM")) {
  if (!inherits(model, "tm_model")) {
    stop("`model` must be a tm_model, as returned by tm_default_model().", call. = FALSE)
  }
  if (length(n_sim) != 1 || is.na(n_sim) || n_sim < 0 || n_sim != round(n_sim)) {
    stop("`n_sim` must be a whole number, 0 or more.", call. = FALSE)
  }
  to_states <- c(ref_from_state, rownames(model$coef))
  mask <- if (is.null(allowed)) NULL else allowed_mask(allowed, model$strata$from, to_states)

  linear_probs <- function(beta) {
    p <- softmax_rows(model$design %*% t(rbind(0, beta)))
    if (!is.null(mask)) {
      p <- p * mask
      empty <- which(rowSums(p) == 0)
      if (length(empty) > 0) {
        stop("The allowed-transitions mask leaves no probability in design rows ",
             format_rows(empty), call. = FALSE)
      }
      p <- p / rowSums(p)
    }
    p
  }

  n_rows <- nrow(model$design)
  vcov_repair <- NULL
  if (n_sim == 0) {
    probs <- linear_probs(model$coef)
    out <- data.table::copy(model$strata)
  } else {
    vcov_repair <- repair_vcov(model$vcov)
    if (!is.null(seed)) withr::local_seed(seed)
    draws <- MASS::mvrnorm(n_sim, mu = as.vector(t(model$coef)), Sigma = vcov_repair$vcov)
    draws <- matrix(draws, nrow = n_sim)
    probs <- matrix(NA_real_, nrow = n_sim * n_rows, ncol = length(to_states))
    for (s in seq_len(n_sim)) {
      beta <- matrix(draws[s, ], nrow = nrow(model$coef), byrow = TRUE)
      probs[(s - 1) * n_rows + seq_len(n_rows), ] <- linear_probs(beta)
    }
    out <- data.table::data.table(sim = rep(seq_len(n_sim), each = n_rows),
                                  model$strata[rep(seq_len(n_rows), n_sim)])
  }
  colnames(probs) <- to_states
  out <- cbind(out, data.table::as.data.table(probs))

  # The model has rows for the living states only. Each death state gets one row per stratum (and draw)
  # that always goes back to itself.
  id_cols <- c(if (n_sim > 0) "sim", "agegrp", "sex", "mm_cluster")
  death_states <- setdiff(to_states, model$strata$from)
  strata_rows <- unique(out[, id_cols, with = FALSE])
  death_rows <- strata_rows[rep(seq_len(nrow(strata_rows)), each = length(death_states))]
  death_rows[, from := rep(death_states, times = nrow(strata_rows))]
  stay <- diag(length(to_states))[match(death_rows$from, to_states), , drop = FALSE]
  colnames(stay) <- to_states
  out <- data.table::rbindlist(list(out, cbind(death_rows, data.table::as.data.table(stay))), use.names = TRUE)
  data.table::setcolorder(out, c(id_cols, "from"))

  result <- tm_matrix(out, strata = c("agegrp", "sex", "mm_cluster"))
  result$source <- if (n_sim == 0) "model, point estimate" else "model, probabilistic draws"
  result$model_info <- list(
    n_sim = n_sim,
    seed = seed,
    mask_applied = !is.null(mask),
    vcov_repair = vcov_repair[c("max_asymmetry", "n_negative_eigen", "min_eigen")]
  )
  if (n_sim == 0) result$model_info$vcov_repair <- NULL
  result
}

# Row-wise softmax, shifted by each row's maximum so exp() cannot overflow.
softmax_rows <- function(scores) {
  scores <- scores - scores[cbind(seq_len(nrow(scores)), max.col(scores, ties.method = "first"))]
  e <- exp(scores)
  e / rowSums(e)
}

# Makes vcov exactly symmetric and positive semi-definite, and reports what changed.
repair_vcov <- function(vcov) {
  sym <- (vcov + t(vcov)) / 2
  eig <- eigen(sym, symmetric = TRUE)
  n_negative <- sum(eig$values < 0)
  fixed <- if (n_negative > 0) {
    eig$vectors %*% (pmax(eig$values, 0) * t(eig$vectors))
  } else {
    sym
  }
  fixed <- (fixed + t(fixed)) / 2
  dimnames(fixed) <- dimnames(vcov)
  list(
    vcov = fixed,
    max_asymmetry = max(abs(vcov - t(vcov))),
    n_negative_eigen = n_negative,
    min_eigen = min(eig$values)
  )
}

# Expands the allowed-transitions table to a 0/1 matrix with one row per design row.
allowed_mask <- function(allowed, from, to_states) {
  if (is.character(allowed) && length(allowed) == 1) {
    if (!nzchar(allowed) || !file.exists(allowed)) {
      stop("Allowed-transitions file not found: ", allowed, ". Pass an existing file or table, ",
           "or `allowed = NULL` to apply no mask.", call. = FALSE)
    }
    allowed <- data.table::fread(allowed)
  }
  allowed <- data.table::as.data.table(allowed)
  if (!"from" %in% names(allowed)) {
    stop("The allowed-transitions table needs a `from` column.", call. = FALSE)
  }
  if (!setequal(setdiff(names(allowed), "from"), to_states)) {
    stop("Allowed-transitions columns must match the model's next states.", call. = FALSE)
  }
  missing_from <- setdiff(unique(from), allowed$from)
  if (length(missing_from) > 0 || anyDuplicated(allowed$from) > 0) {
    stop("The allowed-transitions table needs exactly one row per current state; missing: ",
         paste(missing_from, collapse = ", "), call. = FALSE)
  }
  m <- as.matrix(allowed[, to_states, with = FALSE])
  if (any(!m %in% c(0, 1))) {
    stop("Allowed-transitions values must be 0 or 1.", call. = FALSE)
  }
  m[match(from, allowed$from), , drop = FALSE]
}
