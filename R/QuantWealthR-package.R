#' QuantWealthR: Quantitative Wealth and Inequality Analysis for Archaeology
#'
#' QuantWealthR provides R functions for all the analytical operations
#' available in the QuantWealth online application
#' (<https://quantwealth.eu>), without requiring a web browser or internet
#' connection.
#'
#' ## Main workflow -- count data (grave goods, hoard types, house objects)
#'
#' ```
#' library(QuantWealthR)
#'
#' # 1. Load your data
#' df <- read_count_data("my_graves.csv")   # or pass a data frame directly
#'
#' # 2. Calculate prestige values
#' pres <- calculate_prestige(df, method = "bayesian")
#'
#' # 3. Compute Gini + inequality summary
#' ineq <- inequality_summary(pres)
#' ineq$gini_combined        # main Gini
#' ineq$abs_gini_combined    # absolute Gini (G x mean wealth)
#'
#' # 4. Visualise
#' plot_prestige(pres)           # prestige boxplot (ggplot2)
#' plot_lorenz(pres)             # Lorenz curve
#' plot_density(pres)            # wealth density curves
#' plot_decile(pres)             # decile distribution
#'
#' # 5. Category bias simulation
#' bias <- simulate_category_bias(df, n_sims = 200)
#' plot_bias(bias)
#' ```
#'
#' ## Physical measure data (house floor area, mound volume, etc.)
#'
#' ```
#' sizes <- c(40, 55, 80, 120, 200, 350)   # e.g. house areas in m^2
#' physical_measure_gini(sizes)
#' plot_physical_lorenz(sizes)
#' ```
#'
#' ## Batch pipeline across multiple periods / phases
#'
#' ```
#' result <- qw_pipeline(df, group_col = "phase", id_col = "grave_id")
#' result$summary    # data frame with one row per phase
#' ```
#'
#' ## Key functions
#'
#' | Function | Purpose |
#' |---|---|
#' | [calculate_prestige()] | Prestige values + combined wealth |
#' | [inequality_summary()] | Gini coefficients and wealth statistics |
#' | [lorenz_data()] | Lorenz curve data frame |
#' | [decile_data()] | Decile wealth distribution |
#' | [physical_measure_gini()] | Gini for continuous measure data |
#' | [simulate_category_bias()] | Monte Carlo bias simulation |
#' | [qw_pipeline()] | Batch analysis across groups |
#' | [plot_prestige()] | Prestige boxplot (ggplot2) |
#' | [plot_lorenz()] | Lorenz curve (ggplot2) |
#' | [plot_density()] | Density curves (ggplot2) |
#' | [plot_decile()] | Decile chart (ggplot2) |
#' | [plot_bias()] | Bias simulation (ggplot2) |
#' | [plot_bias_wealth_density()] | Per-grave wealth density across simulations (ggplot2) |
#'
#' ## Tutorial vignette
#'
#' A step-by-step introduction is available as a package vignette:
#'
#' ```
#' vignette("introduction", package = "QuantWealthR")
#' ```
#'
#' The vignette ships with the installed and CRAN versions of the