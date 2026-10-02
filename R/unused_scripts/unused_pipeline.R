# --- Settings: pick one group with a clear offset ----------------------------
grp_name <- "SK: 2480-2300"          # key in QW_multi$by_group
phase    <- "2480-2300"              # the value in Zeitstufen_final
raw_data <- Gross_SK_Thur            # the dataset that group comes from

# --- Rebuild the table the pipeline gave to the simulation -------------------
g      <- QW_multi$by_group[[grp_name]]
raw    <- raw_data[which(raw_data$Zeitstufen_final == phase), ]
extra  <- intersect(names(raw), c("adult_count", "subadult_count", "lat", "lng"))
sub_df <- raw[, c("context_id", g$count_cols, extra)]   # ID first, as in the pipeline

# --- Run the observed calculation both ways ----------------------------------
set.seed(1)
b_int <- suppressWarnings(simulate_category_bias(sub_df, n_sims = 2))          # A: internal
b_pip <- suppressWarnings(simulate_category_bias(sub_df, prestige_result = g$prestige_df,
                                                 n_sims = 2))                  # B: pipeline

# --- Compare --------------------------------------------------------------
o_int <- b_int$wealth_scores$orig
o_pip <- b_pip$wealth_scores$orig
cmp <- data.frame(
  id            = o_int$grave_id,
  TOT           = o_int$TOT,
  prestige_int  = o_int$prestige,      prestige_pip  = o_pip$prestige,
  combined_int  = o_int$prestige_sqrt, combined_pip  = o_pip$prestige_sqrt
)
cmp$diff_prestige <- round(cmp$prestige_pip - cmp$prestige_int, 3)
cmp$diff_combined <- round(cmp$combined_pip - cmp$combined_int, 3)

# 1. Do the Gini values differ, and in which measure?
round(rbind(internal = c(prestige = b_int$orig_gini_prestige, combined = b_int$orig_gini_prestige_sqrt),
            pipeline = c(prestige = b_pip$orig_gini_prestige, combined = b_pip$orig_gini_prestige_sqrt)), 4)

# 2. How many graves differ, and by how much?
sum(abs(cmp$diff_combined) > 0.01); summary(cmp$diff_combined)

# 3. The graves that differ most
head(cmp[order(-abs(cmp$diff_combined)), ], 10)


tab <- table(sub_df$context_id)
tab[tab > 1]                                   # which IDs repeat, and how often
sum(duplicated(Gross_SK_Thur$context_id))      # across the whole dataset







d       <- Gross_SK_Thur
dup_ids <- unique(d$context_id[duplicated(d$context_id)])
dups    <- d[d$context_id %in% dup_ids, ]
dups    <- dups[order(dups$context_id), ]
cat(nrow(dups), "rows share", length(dup_ids), "IDs\n")
cat(sum(duplicated(dups)), "rows are exact copies of another row (all columns identical)\n")

# A. Which columns differ between rows with the same ID? (NA vs a value counts as different)
differs <- sapply(setdiff(names(dups), "context_id"), function(col)
  sum(tapply(dups[[col]], dups$context_id, function(v) length(unique(v)) > 1)))
differs <- sort(differs[differs > 0], decreasing = TRUE)
data.frame(column = names(differs), IDs_where_it_differs = differs,
           out_of = length(dup_ids), row.names = NULL)

# B. How are the grave goods spread across the rows of each ID?
gcols <- setdiff(identify_count_columns(d), "date_median_BCE")
goods <- apply(dups[gcols], 1, function(r) paste(ifelse(is.na(r), 0, r), collapse = ","))
n_types <- rowSums(sapply(dups[gcols], function(x) !is.na(x) & x != 0))
pattern <- tapply(seq_len(nrow(dups)), dups$context_id, function(i) {
  if (all(n_types[i] == 0))              "no goods on any row"
  else if (length(unique(goods[i])) == 1) "identical goods on every row"
  else if (sum(n_types[i] > 0) == 1)     "goods on one row only"
  else                                   "different goods on different rows"
})
table(pattern)






d <- Gross_SK_Thur
gcols   <- setdiff(identify_count_columns(d), "date_median_BCE")    # grave-good columns
goods   <- apply(d[gcols], 1, function(r) paste(ifelse(is.na(r), 0, r), collapse = ","))
n_types <- rowSums(sapply(d[gcols], function(x) !is.na(x) & x != 0))  # types per row
dup_id  <- d$context_id %in% d$context_id[duplicated(d$context_id)]

# --- 1. Identical rows: same grave + same phase + same goods ------------------
merge_key <- paste(d$context_id, d$Zeitstufen_final, goods, sep = "|")
set_size  <- ave(seq_len(nrow(d)), merge_key, FUN = length)
ident <- d[set_size > 1, c("context_id", "ID_Individuum", "Zeitstufen_final",
                           "adult_count", "subadult_count", "Alter")]
ident$n_types <- n_types[set_size > 1]
ident[order(ident$context_id), ]
cat(length(unique(merge_key[set_size > 1])), "sets of identical rows,",
    sum(set_size > 1), "rows in total\n")

Gross_GrabID <- readxl::read_excel("C:/Users/mattb/_Postdoc/wealth data/CWC_grave_data/Grossmann2016_exportClaude/data/ID_Grab.xlsx")



# --- 2. Graves where some rows have goods and others are empty ----------------
ids <- unique(d$context_id[dup_id & n_types == 0 &
                             ave(n_types > 0, d$context_id, FUN = any)])
duplicate_summary <- data.frame(
  context_id = ids,
  rows       = sapply(ids, function(i) sum(d$context_id == i)),
  with_goods = sapply(ids, function(i) sum(d$context_id == i & n_types > 0)),
  empty      = sapply(ids, function(i) sum(d$context_id == i & n_types == 0)),
  phases     = sapply(ids, function(i) length(unique(d$Zeitstufen_final[d$context_id == i]))),
  row.names  = NULL
)

orig <- Gross_GrabID                                                # your original grave table
first_number <- function(x) sub("^\\D*(\\d+).*$", "\\1", as.character(x))   # "GRAB_1025_shared" -> "1025"

# One Hinweise text per grave (in case the original has several rows per grave)
orig$grab_nr <- first_number(orig$ID_Grab)
hinweise <- tapply(orig$Hinweise, orig$grab_nr,
                   function(h) paste(unique(na.omit(h)), collapse = " | "))

# Attach to the Part 2 summary
duplicate_summary$grab_nr  <- first_number(duplicate_summary$context_id)
duplicate_summary$Hinweise <- hinweise[duplicate_summary$grab_nr]

sum(is.na(duplicate_summary$Hinweise))   # graves that didn't match; should be 0
View(duplicate_summary)                  # long text is easier to read in the viewer

ids <- duplicate_summary$context_id
sel <- Gross_SK_Thur$context_id %in% ids
chk <- Gross_SK_Thur[sel, c("context_id", "ID_Individuum", "Zeitstufen_final", "dating_method",
                            "Individuumsspezifisch", "Alter", "adult_count", "subadult_count")]
chk$n_types <- n_types[sel]            # from the earlier script (same row order)
chk[order(chk$context_id, -chk$n_types), ]
table(Gross_SK_Thur$Individuumsspezifisch)
