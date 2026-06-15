#' QuantWealthR: Quantitative Wealth and Inequality Analysis for Archaeology
#'
#' QuantWealthR provides R functions for all the analytical operations
#' available in the QuantWealth online application
#' (<https://quantwealth.eu>), without requiring a web browser or internet
#' connection.
#'
#' ## Main workflow — count data (grave goods, hoard types, house objects)
#'
#' ```
#' library(QuantWealthR)
#'
#' # 1. Load your data
#' df <- read_qw_data("my_graves.csv")   # or pass a data frame directly
#'
#' # 2. Calculate prestige values
#' pres <- calculate_prestige(df, method = "bayesian")
#'
#' # 3. Compute Gini + inequality summary
#' ineq <- inequality_summary(pres)
#' ineq$gini_combined        # main Gini
#' ineq$abs_gini_combined    # absolute Gini (G × mean wealth)
#'
#' # 4. Visualise
#' plot_prestige(pres)           # prestige boxplot (ggplot2)
#' plot_lorenz(pres)             # Lorenz curve
#' plot_density(pres)            # wealth density curves
#' plot_decile(pres)             # decile distribution
#'
#' # Interactive (plotly) equivalents
#' plot_prestige_interactive(pres)
#' plot_inequality_panel(pres)   # combined three-panel subplot
#'
#' # 5. Category bias simulation
#' bias <- simulate_category_bias(df, n_sims = 200)
#' plot_bias(bias)
#' ```
#'
#' ## Physical measure data (house floor area, mound volume, etc.)
#'
#' ```
#' sizes <- c(40, 55, 80, 120, 200, 350)   # e.g. house areas in m²
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
#' | [plot_inequality_panel()] | Combined panel (plotly) |
#'
#' @seealso
#' * QuantWealth web app: <https://quantwealth.eu>
#' * Accompanying paper: <https://doi.org/10.31235/osf.io/wzcqy_v2>
#'
#' @keywords internal
"_PACKAGE"
