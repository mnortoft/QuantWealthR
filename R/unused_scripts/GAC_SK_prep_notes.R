library(QuantWealthR)
library(dplyr)
library(readr)

Gross_SK_Thur <- read_count_data("C:/Users/mattb/_Postdoc/wealth data/CWC_grave_data/Grossmann2016_exportClaude/data/Gross_SK_ThurBurg_clust.csv")

GAC_files <- list.files(path = "C:/Users/mattb/_Postdoc/wealth data/CWC_grave_data/Grossmann2016_exportClaude/data", pattern = "Beier", full.names = TRUE)
GAC_all <- read_csv(GAC_files, id = "source_file")  
str(GAC_all)

GAC_prep <- as.data.frame(GAC_all)
str(GAC_prep)
GAC_prep$Zeitstufen_final <- sub("^'", "", GAC_prep$phase_date_BCE)      # "'3250-2900" -> "3250-2900"
GAC_prep$date_median_BCE  <- vapply(strsplit(GAC_prep$Zeitstufen_final, "-"),
                                    function(r) mean(as.numeric(r)), numeric(1))

# Put the grave ID first and drop source_file (see point 1 below)
GAC_prep <- GAC_prep[, c("context_id", setdiff(names(GAC_prep), c("context_id", "source_file")))]
unique(Gross_SK_Thur$Zeitstufen_final)
GAC_prep$Zeitstufen_final <- factor(GAC_prep$Zeitstufen_final, levels = c("3250-2900", "2900-2680"))

Gross_SK_Thur$Zeitstufen_final <- factor(Gross_SK_Thur$Zeitstufen_final, 
                                         levels = c("2800-2620", "2620-2480", "2480-2300", "2300-2200"))

Gross_QW <- count_pipeline(Gross_SK_Thur, group_col = c("Kultur", "Zeitstufen_final"),
                           date_col = "date_median_BCE", run_bias_sim = TRUE)

Gross_SK_Thur <- Gross_SK_Thur %>% 
  dplyr::rename(context_id = ID_BurialEvent)
GAC_prep$context_id
sum(duplicated(GAC_prep$context_id))
sum(duplicated(Gross_SK_Thur$context_id))
sum(duplicated(Gross_SK_Thur$ID_Individuum))



QW_multi <- count_pipeline(list(SK = Gross_SK_Thur, GAC = GAC_prep),
                           id_col = "context_id",
                           group_col = "Zeitstufen_final",
                           date_col  = "date_median_BCE",
                           run_bias_sim = TRUE)



GAC_test <- GAC_prep
names(GAC_test) <- sub("^cer_(.*)$", "\\1_pottery", names(GAC_test))   # cer_KA -> KA_pottery
QW_test <- count_pipeline(list(GAC = GAC_test), group_col = "Zeitstufen_final",
                          date_col = "date_median_BCE", run_bias_sim = TRUE)
plot_bias_groups(QW_test)


b    <- QW_multi$by_group[["GAC: 3250-2900"]]$bias
cols <- b$col_names                                    # the columns the simulation used
lapply(.find_lump_groups(cols), function(i) cols[i])  # one list entry per lump group

sum(duplicated(Gross_SK_Thur$ID_Individuum))   # check it's unique first
Gross_SK_Thur$context_id <- paste(Gross_SK_Thur$context_id, Gross_SK_Thur$ID_Individuum, sep = "_ind")


d    <- GAC_prep[GAC_prep$Zeitstufen_final == "3250-2900", ]
cols <- setdiff(identify_count_columns(d), "date_median_BCE")   # same rule as the pipeline
pres <- sapply(d[cols], function(x) !is.na(x) & x > 0)          # TRUE = type present in grave
tot  <- rowSums(pres)                                           # TOT per grave
rich <- tot >= quantile(tot, 0.75)                              # richest quarter by TOT
pref <- tolower(sub("_.*", "", cols))                           # prefix of each column
per_prefix <- sapply(unique(pref), function(p) rowSums(pres[, pref == p, drop = FALSE]))

# Share of each group's TOT coming from each prefix
round(rbind(poorer_75pct  = colSums(per_prefix[!rich, ]) / sum(tot[!rich]),
            richest_25pct = colSums(per_prefix[rich, ])  / sum(tot[rich])), 2)

table(Gross_SK_Thur$adult_count > 1)
plot_bias_wealth_density_groups(Gross_QW)
plot_bias_wealth_density_groups(QW_multi)

sum(duplicated(Gross_SK_Thur$context_id))
sum(duplicated(Gross_SK_Thur$ID_BurialEvent))
sum(duplicated(GAC_prep$context_id))

plot_bias_groups(QW_multi)
plot_bias_wealth_density_groups(Gross_QW)
plot_bias_wealth_density_groups(Gross_QW, measure = "prestige")
plot_bias_wealth_density_groups(Gross_QW,
                                groups = c("SK | 2800-2620", "SK | 2620-2480", "SK | 2480-2300", "SK | 2300-2200"), ncol = 2)

plot_bias_wealth_density_groups(Gross_QW, scales = "free_x")
plot_bias_wealth_density_groups(Gross_QW, relative = TRUE) 
