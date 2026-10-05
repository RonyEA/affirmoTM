#' @keywords internal
"_PACKAGE"

#' @import data.table
NULL

# Column names used inside data.table and ggplot2 expressions, declared so that R CMD check does not
# report them as undefined variables.
utils::globalVariables(c(
  ".", "agegrp", "arm", "cost", "cost_disc", "cost_total", "cycle", "delta_cost", "delta_qaly",
  "discount", "entry_state", "from", "i.n", "icer", "id", "intervention_cost", "intervention_cost_total", "label", "patients", "qaly", "qaly_disc", "qaly_total",
  "quadrant", "run", "since_entry", "state", "value_arm", "value_year", "wtp", "x_end", "y_end", "year"
))
