# QuantWealthR

**Quantitative wealth and inequality analysis for archaeological data.**

QuantWealthR brings the full analytical toolkit of the [QuantWealth online app](https://quantwealth.eu) into R, so you can run analyses programmatically, batch-process multi-period datasets, integrate results into your own scripts, and produce publication-quality ggplot2 figures.

---

## Installation

```r
# Install devtools if needed
install.packages("devtools")

# Install QuantWealthR from GitHub
devtools::install_github("mnortoft/QuantWealthR", build_vignettes = TRUE)

library(QuantWealthR)
```

Required packages are installed automatically as dependencies:
`dplyr`, `ggplot2`, `gglorenz`, `DescTools`, `ggtext`, `tidyr`.

A small example dataset is bundled with the package — see `example_data_path()` below to try the functions immediately without your own data.

---

## What does this package do?

The package handles two types of archaeological data:

| Data type | Examples | Key functions |
|-----------|----------|---------------|
| **Count data** | Grave goods, hoard types, house objects | `calculate_prestige()`, `inequality_summary()`, `simulate_category_bias()` |
| **Physical measures** | House floor area (m²), mound volume (m³), grave pit depth (cm) | `physical_measure_summary()`, `plot_physical_lorenz()` |

Both types can be processed in bulk across multiple periods or sites using `qw_pipeline()`.

---

## Quick start

### Count data (e.g. grave goods)

```r
library(QuantWealthR)

# Load your data — first column = unit ID, rest = counts
df <- read_count_data("my_graves.csv")   # or read.csv() / readxl::read_excel()

# Or try the bundled example dataset
df <- read_count_data(example_data_path())

# Step 1: Calculate prestige values
pres <- calculate_prestige(df, method = "bayesian")

# Step 2: Inequality summary
ineq <- inequality_summary(pres)
ineq$gini_combined      # Gini (prestige + sqrt-count bonus)
ineq$abs_gini_combined  # Absolute Gini (G × mean wealth)
ineq$mean_wealth

# Step 3: Plots
plot_prestige(pres, case = "Bell Beaker", dates = format_dates(-2500, -2200))
plot_lorenz(pres, ineq, case = "Bell Beaker", dates = format_dates(-2500, -2200))
plot_density(pres)
plot_decile(pres, case = "Bell Beaker", dates = format_dates(-2500, -2200))
```

### Physical measures (e.g. house floor area)

```r
areas <- house_df$area_m2   # numeric vector

ineq  <- physical_measure_summary(areas)
ineq$gini
ineq$abs_gini

plot_physical_lorenz(areas, data_type = "house size (m2)",
                     case = "LBK", dates = "5500-4900 BCE")
plot_physical_density(areas, data_type = "house size (m2)")
plot_physical_decile(areas, unit = "m2")
```

### Category bias simulation

```r
set.seed(42)
bias <- simulate_category_bias(df, n_sims = 200, graves = "adults")

# Static 2×2 facet plot of simulated Gini values
plot_bias(bias, case = "Bell Beaker", dates = format_dates(-2500, -2200))

# Store per-grave wealth scores for every simulation to see how splitting/
# lumping reshapes the whole wealth distribution, not just the Gini
bias_w <- simulate_category_bias(df, n_sims = 100, store_wealth = TRUE)
plot_bias_wealth_density(bias_w, case = "Bell Beaker")
plot_bias_wealth_lorenz(bias_w, case = "Bell Beaker")
```

### Multi-case batch pipeline

```r
# Dataset with a grouping column (phase, period, site, etc.)
result <- qw_pipeline(df,
                       group_col = "phase",
                       id_col    = "grave_id",
                       method    = "bayesian")

# Summary table: one row per case
cases_summary(result)

# Compare Gini values across cases
plot_gini_comparison(result)
plot_lorenz_comparison(result)
```

---

## Data format

### Count data

| grave_id | sword | shield | bronze_pin | adult_count | subadult_count |
|----------|-------|--------|------------|-------------|----------------|
| G001     | 1     | NA     | 2          | 1           | 0              |
| G002     | NA    | 1      | NA         | 0           | 1              |
| G003     | 2     | 1      | NA         | 2           | 0              |

- **First column**: unit ID (grave, feature, hoard, etc.)
- **Count columns**: numeric counts (0 or absent → NA)
- **Optional**: `adult_count`, `subadult_count` — enables per-capita wealth and age-separated analysis
- **Optional**: `lat`, `lng` — ignored by analysis functions, preserved in output

### Physical measure data

| house_id | area_m2 |
|----------|---------|
| H001     | 45.2    |
| H002     | 72.8    |
| H003     | 120.0   |

First column = unit ID, second column = the measure value.

---

## Key functions reference

### Data utilities
| Function | Description |
|----------|-------------|
| `read_count_data(filepath)` | Read count data (CSV/Excel), auto-detect separator, convert 0→NA |
| `read_physical_data(filepath)` | Read physical-measure data as-is (no zero cleaning) |
| `identify_count_columns(df)` | Detect which columns are count data |
| `format_dates(start, end)` | Format BCE/CE date ranges as strings |

### Count data analysis
| Function | Description |
|----------|-------------|
| `calculate_prestige(df, method, use_age_sep)` | Main function: computes prestige values and combined wealth. Returns original df with `raw_TOT`, `prestige_total`, `combined_total` columns added |
| `get_TOT_range(df)` | Compute long-format TOT range table (used internally) |
| `bayesian_prestige(tot_range)` | Bayesian empirical posterior estimates per artefact type |
| `apply_prestige_bayesian(bayes_df, df)` | Apply Bayesian weights to data frame |
| `apply_prestige_mean(means_df, df)` | Apply mean-based weights to data frame |
| `sqrt_count_bonus(df)` | Compute √(count)−1 bonus per unit |
| `inequality_summary(prestige_df)` | Gini coefficients (combined, prestige, TOT), absolute Gini, mean/median wealth, sample size, optional subadult Gini |
| `lorenz_data(prestige_df)` | Long-format Lorenz curve data for three wealth measures |
| `decile_data(prestige_df)` | Decile wealth distribution data |
| `simulate_category_bias(df, graves, n_sims)` | Monte Carlo split/lump simulation |

### Physical measure analysis
| Function | Description |
|----------|-------------|
| `physical_measure_gini(sizes)` | Gini coefficient with 80% bootstrap CI |
| `physical_measure_summary(sizes)` | Full summary: Gini, absolute Gini, mean, median, min, max, n |

### ggplot2 plots (static, publication-quality)
| Function | Description |
|----------|-------------|
| `plot_prestige(prestige_df, method, case, dates)` | Boxplot of TOT scores with Bayesian credible intervals per type |
| `plot_lorenz(prestige_df, ineq_summary, case, dates)` | Lorenz curves for combined, prestige, and TOT+1 measures |
| `plot_density(prestige_df, graves, case, dates)` | Wealth density curves |
| `plot_decile(prestige_df, case, dates)` | Decile wealth distribution |
| `plot_bias(bias_result, case, dates)` | 2×2 faceted jitter plot of simulated Ginis |
| `plot_physical_lorenz(sizes, data_type, case, dates)` | Lorenz curve for physical measures |
| `plot_physical_density(sizes, data_type, case, dates)` | Density curve for physical measures |
| `plot_physical_decile(sizes, data_type, case, dates, unit)` | Decile chart for physical measures |
| `plot_bias_wealth_density(bias_result, measure, case, dates, alpha)` | Multiline density plot of per-grave wealth scores across all simulations, coloured by split/lump |
| `plot_bias_wealth_lorenz(bias_result, measure, case, dates, alpha)` | Multiline Lorenz curve plot across all simulations, coloured by split/lump |

### Multi-case pipeline
| Function | Description |
|----------|-------------|
| `qw_pipeline(df, group_col, id_col, data_type, method, run_bias_sim)` | Batch analysis across groups |
| `cases_summary(pipeline_result)` | Formatted summary table |
| `print_pipeline_summary(pipeline_result)` | Print to console |
| `plot_gini_comparison(pipeline_result, measure, case_order)` | Point + CI plot comparing Gini across cases (cases on x-axis) |
| `plot_lorenz_comparison(pipeline_result, cases, case_order)` | Overlaid Lorenz curves per case |

---

## Methods

### Prestige values

Artefact types are assigned a prestige weight based on their **exclusivity**: types found in graves with many other types (high TOT — Total Object Types) get lower prestige; types found only in richly furnished graves get higher prestige.

**Bayesian method (recommended)**: For types with n ≥ 2 occurrences, fits a Gamma prior using method of moments from the type's own TOT distribution (local empirical prior). For unique types (n = 1) a weak prior is used.

**Mean method**: Prestige weight = mean TOT of graves containing that type.

The **combined wealth score** adds a square-root count bonus (√count − 1) to the prestige total, rewarding quantity without letting it dominate: `combined_total = prestige_total + Σ(√count − 1)`.

When `adult_count` and `subadult_count` columns are present, prestige weights are estimated from adult graves only, and wealth is divided by the number of adults per grave (per-capita wealth).

### Gini coefficient

Three Gini measures are computed:
- **G (prestige + √count)** — main recommended measure (`combined_total`)
- **G (prestige only)** — without count bonus (`prestige_total`)
- **G (TOT + 1)** — simple count of types present

All use `DescTools::Gini()` with 80% bootstrap CI (n ≤ 500).

The **absolute Gini** (G × mean wealth) captures both inequality and overall wealth level.

### Category bias simulation

Artefact categories are researcher-defined choices. The simulation tests whether Gini values are robust to different recording decisions by randomly splitting types into sub-types and lumping related types together across 100–1000 Monte Carlo iterations.

---

## Citation

If you use QuantWealthR in published research, please cite the accompanying paper:

> Nørtoft, M. (2024). QuantWealth — a tool for multivariate wealth display and inequality analysis in archaeology. *SocArXiv*. https://doi.org/10.31235/osf.io/wzcqy_v2

--