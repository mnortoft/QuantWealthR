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
#' bias <- simulate_category_bias(df, pres, n_sims = 200)
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
#' ## Several phases and datasets
#'
#' ```
#' ph  <- read_count_data(example_data_path("phases"))   # simulated example
#' res <- count_pipeline(split(ph, ph$culture), group_col = "phase",
#'                       id_col = "Grave_ID", date_col = "date_BCE",
#'                       run_bias_sim = TRUE)
#' res$summary                                   # one row per case
#' plot_gini_timeline(res, series = c("gini", "top_share", "median_wealth"),
#'                    bias_band = TRUE)
#' compare_cases(res, "A: A2", "B: B2")          # is A2 more unequal than B2?
#' ```
#'
#' ## Functions by task
#'
#' **Reading data**
#'
#' | Function | Purpose |
#' |---|---|
#' | [read_count_data()] | Read grave-good counts (CSV/Excel) |
#' | [read_physical_data()] | Read physical measures (e.g. house areas) |
#' | [example_data_path()] | Paths to the bundled example data |
#' | [identify_count_columns()] | Which columns count as grave goods |
#' | [impute_phases()] | Multiple imputation of phases for undated graves |
#' | [add_soil_preservation()] | Soil preservation scores at each grave (user-supplied ESDAC map) |
#'
#' **Wealth of single graves**
#'
#' | Function | Purpose |
#' |---|---|
#' | [calculate_prestige()] | Type values, prestige and combined wealth per grave |
#' | [get_TOT_range()], [get_TOT_means()], [bayesian_prestige()] | The steps behind the type values |
#' | [apply_prestige_mean()], [apply_prestige_bayesian()], [sqrt_count_bonus()] | Applying type values and the count bonus |
#'
#' **Inequality of one assemblage**
#'
#' | Function | Purpose |
#' |---|---|
#' | [inequality_summary()] | Gini (relative and absolute) and wealth statistics |
#' | [calculate_gini()] | Gini with bootstrap interval |
#' | [lorenz_data()], [decile_data()] | Data behind the Lorenz and decile plots |
#' | [physical_measure_gini()], [physical_measure_summary()] | The same for physical measures |
#' | [plot_prestige()], [plot_lorenz()], [plot_density()], [plot_decile()] | Plots of one assemblage |
#' | [plot_physical_lorenz()], [plot_physical_density()], [plot_physical_decile()] | Plots for physical measures |
#'
#' **Recording bias**
#'
#' | Function | Purpose |
#' |---|---|
#' | [simulate_category_bias()] | Random splitting and lumping of types |
#' | [plot_bias()], [plot_bias_groups()] | Gini distributions across simulations |
#' | [plot_bias_wealth_density()], [plot_bias_wealth_density_groups()], [plot_bias_wealth_lorenz()] | Wealth distributions across simulations |
#'
#' **Several phases or datasets**
#'
#' | Function | Purpose |
#' |---|---|
#' | [count_pipeline()], [phys_pipeline()] | Run everything per phase/group |
#' | [print_pipeline_summary()], [cases_summary()] | Summary tables |
#' | [plot_gini_comparison()], [plot_gini_timeline()] | Inequality and wealth statistics per case / over time |
#' | [plot_lorenz_comparison()] | Lorenz curves with crossings marked |
#' | [plot_wealth_boxplot()], [plot_decile_groups()], [plot_type_prevalence()] | Distributions and types per case |
#' | [plot_case_map()] | Map of the sites or graves of each case |
#'
#' **Comparing cultures from different sources**
#'
#' | Function | Purpose |
#' |---|---|
#' | [compare_cases()], [plot_compare_cases()] | Is case A more unequal than case B, with sampling, recording and imputation uncertainty |
#' | [multiverse_compare()], [plot_multiverse()] | How the answer depends on analysis choices |
#' | [has_age_info()] | Row filter: graves with recorded age (for `row_filters`) |
#'
#' ## Symbols in plot labels and PDF output
#'
#' Some plot labels contain symbols (epsilon, square root, arrow) that the
#' standard [grDevices::pdf()] device cannot draw. Save PDF figures with
#' `ggplot2::ggsave(..., device = grDevices::cairo_pdf)`, or set
#' `options(QuantWealthR.unicode = FALSE)` before creating the plots to get
#' ASCII labels (`e`, `sqrt(count)`, `->`). While a `pdf()` device is open,
#' the ASCII labels are used automatically.
#'
#' ## Tutorial vignette
#'
#' Two step-by-step vignettes come with the package:
#'
#' ```
#' vignette("introduction", package = "QuantWealthR")  # one assemblage, step by step
#' vignette("comparing", package = "QuantWealthR")     # phases, cultures, uncertainty
#' ```
#'
#' The vignettes ship with the installed and CRAN versions of the package. If
#' you installed from source and they are missing, rebuild and install it with:
#'
#' ```
#' devtools::install(build_vignettes = TRUE)
#' browseVignettes("QuantWealthR")
#' ```
#'
#' @seealso
#' * QuantWealth web app: <https://quantwealth.eu>
#' * Accompanying paper: <https://doi.org/10.31235/osf.io/wzcqy_v3>
#'
#' @keywords internal
#' @importFrom stats median qgamma var
#' @importFrom utils read.csv
"_PACKAGE"
