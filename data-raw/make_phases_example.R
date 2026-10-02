# Simulated example data for QuantWealthR examples and the vignette.
# Two FICTITIOUS cultures (A, B), three phases each, plus some undated graves.
# Not real archaeological data. Re-create with: source("data-raw/make_phases_example.R")
set.seed(2026)
types <- c("c_beaker", "c_amphora", "c_bowl", "c_cup", "c_jar",
           "f_blade", "f_axe", "f_arrowhead", "s_battleaxe", "s_hammer",
           "cu_awl", "cu_ring", "o_tooth_pendant", "o_shell_bead", "o_amber_bead",
           "b_awl", "CON_stone_cist")
high <- c("o_tooth_pendant", "o_shell_bead", "o_amber_bead", "f_arrowhead")
spec <- list(
  A = list(phases = c("A1", "A2", "A3"), dates = c(-2850, -2650, -2450),
           n = c(30, 45, 40), richness = c(1.4, 2.2, 2.6), cv = c(0.9, 0.8, 0.6)),
  B = list(phases = c("B1", "B2", "B3"), dates = c(-3150, -2950, -2750),
           n = c(35, 40, 30), richness = c(2.6, 2.4, 2.8), cv = c(0.7, 0.8, 0.8)))
base_p <- c(c_beaker = .5, c_amphora = .4, c_bowl = .3, c_cup = .2, c_jar = .15,
            f_blade = .35, f_axe = .2, f_arrowhead = .15, s_battleaxe = .12, s_hammer = .05,
            cu_awl = .05, cu_ring = .04, o_tooth_pendant = .12, o_shell_bead = .1,
            o_amber_bead = .06, b_awl = .1, CON_stone_cist = .1)
rows <- list(); id <- 0
for (cul in names(spec)) {
  s <- spec[[cul]]
  for (k in seq_along(s$phases)) {
    for (i in seq_len(s$n[k])) {
      id <- id + 1
      status <- stats::rlnorm(1, log(s$richness[k]), s$cv[k])
      n_types <- min(stats::rpois(1, status), length(types))
      chosen <- if (n_types > 0) sample(types, n_types, prob = base_p * (1 + status / 4)) else character(0)
      cnt <- stats::setNames(rep(0L, length(types)), types)
      for (t in chosen) cnt[t] <- if (t %in% high) 1L + stats::rpois(1, 2 + 3 * status) else 1L + stats::rbinom(1, 2, 0.2)
      if (cul == "A") cnt["CON_stone_cist"] <- 0L          # culture A: constructions not recorded
      sub <- stats::runif(1) < 0.2
      rows[[length(rows) + 1]] <- data.frame(
        Grave_ID = sprintf("%s%03d", cul, id), culture = cul, phase = s$phases[k],
        date_BCE = s$dates[k], dating_method = sample(c("radiocarbon", "typology"), 1, prob = c(.3, .7)),
        adult_count = if (sub) 0L else 1L + stats::rbinom(1, 1, 0.08),
        subadult_count = if (sub) 1L else stats::rbinom(1, 1, 0.05),
        t(cnt), check.names = FALSE)
    }
  }
}
d <- do.call(rbind, rows)
# 12 graves of culture A whose phase is unknown (to be imputed)
und <- sample(which(d$culture == "A"), 12)
d$phase[und] <- NA; d$date_BCE[und] <- NA; d$dating_method[und] <- "random imputation"
# Fictitious sites with coordinates (own seed, so the graves above stay the same)
set.seed(7)
sites <- data.frame(
  Site = c(paste0("A-site ", 1:5), paste0("B-site ", 1:5)),
  culture = rep(c("A", "B"), each = 5),
  lat = c(50.1 + stats::rnorm(5, 0, 0.25), 51.5 + stats::rnorm(5, 0, 0.3)),
  lng = c(14.3 + stats::rnorm(5, 0, 0.4), 11.8 + stats::rnorm(5, 0, 0.4)))
d$Site <- vapply(d$culture, function(cl) sample(sites$Site[sites$culture == cl], 1,
                 prob = c(.35, .25, .2, .12, .08)), character(1))
d$lat <- round(sites$lat[match(d$Site, sites$Site)], 4)
d$lng <- round(sites$lng[match(d$Site, sites$Site)], 4)
d <- d[, c("Grave_ID", "Site", "lat", "lng", setdiff(names(d), c("Grave_ID", "Site", "lat", "lng")))]
d[d == 0] <- NA            # absent types as NA, as read_count_data() expects
d$adult_count[is.na(d$adult_count)] <- 0L; d$subadult_count[is.na(d$subadult_count)] <- 0L
write.csv(d, "inst/extdata/quantwealth_phases_example.csv", row.names = FALSE, na = "NA")
