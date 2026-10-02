# ============================================================================
# Suppress R CMD check NOTEs for non-standard-evaluation (NSE) column names
# ============================================================================
#
# None of the names below are genuine undefined globals. They are data frame
# column names referenced unquoted inside dplyr verbs (e.g. dplyr::arrange(Gini))
# or ggplot2::aes() calls (e.g. aes(x = cum_pop, y = cum_wealth)) throughout
# R/plots_ggplot.R, R/inequality.R, R/pipeline.R, R/bias_simulation.R, and
# R/comparison.R. R's static code analysis cannot distinguish "column name
# used via NSE" from "missing variable", so it flags every one of them as
# "no visible binding for global variable" during R CMD check. Declaring them
# here (the standard fix recommended in Writing R Extensions) tells the check
# to treat these specific names as known, without changing any behaviour.
#
# If you add a new ggplot2/dplyr pipeline that references a column by bare
# name and R CMD check flags it, add the column name to this list rather than
# silencing the warning some other way.
utils::globalVariables(c("line_group", "panel", "long", "group", "size_val", "lng", "lat",
  "Direction", "Gini", "Grave_ID", "TOT", "Type", "Type_display", "Value",
  "abs_g", "bayes_est", "bias_hi", "bias_lo", "dataset", "case", "case_label", "combined_total", "cum_pop", "cum_wealth",
  "cumulative_wealth", "direction", "gini", "iqr_high", "iqr_low", "is_sub",
  "label", "lower", "lowerCI", "measure", "mid_point", "n_in_decile",
  "orig_gini", "series_group", "sim_id", "total_wealth", "upper", "upperCI",
  "case_a", "case_b", "colour", "comparison", "diff", "lower_95", "median_diff", "txt", "upper_95", "edge_zone", "share", "type_key", "value", "wealth_rank", "wealth_share", "x", "x_pos", "y_pos"
))
