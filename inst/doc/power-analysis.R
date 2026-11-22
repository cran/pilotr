## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = FALSE, comment = "")
# Console colour carries no meaning on a rendered page. pkgdown turns it on for
# its own build, and the escape sequences then reach the reader as literal text,
# so colour is switched off here for a plain vignette render and a site build
# alike. The fixed width keeps printed output inside the documentation column.
options(cli.num_colors = 1, cli.hyperlink = FALSE, crayon.enabled = FALSE,
        width = 80)

## ----setup--------------------------------------------------------------------
library(pilotr)

## -----------------------------------------------------------------------------
spec <- build_spec(list(
  name = "two_group", seed = 1, design_kind = "between", n_subject = 64,
  factor_name = "group", lev1 = "control", lev2 = "treatment",
  intercept = 100, effect = 5, family = "gaussian",
  resp_name = "score", sigma = 10))

pw <- power_design(spec, n_sims = 500)
unlist(pw[c("power", "type_s", "type_m", "true_effect", "mean_estimate")])

## -----------------------------------------------------------------------------
pw_greater <- power_design(spec, n_sims = 500, alternative = "greater")
unlist(pw_greater[c("power", "type_s", "type_m")])

## ----priming-spec-------------------------------------------------------------
spec_c <- build_spec(list(
  name = "priming", seed = 1, design_kind = "within", include_items = TRUE,
  n_subject = 24, n_item = 18,
  factor_name = "condition", lev1 = "related", lev2 = "unrelated",
  intercept = 6, effect = 0.06,
  subj_int_sd = 0.12, subj_slope_sd = 0.04, subj_corr = 0.2,
  item_int_sd = 0.08, item_slope_sd = 0.02, item_corr = -0.1,
  family = "shifted_lognormal", resp_name = "RT", sigma = 0.3, shift = 200))

## -----------------------------------------------------------------------------
model_formula(spec_c)

## ----power-mixed, eval = FALSE------------------------------------------------
# # A tiny replicate count keeps the vignette fast. Use 200 or more for real planning.
# pm <- power_mixed(spec_c, n_sims = 20)
# pm

## ----include = FALSE----------------------------------------------------------
# The result of running exactly the chunk above, precomputed by tools/vignette-caches.R and
# shipped with the package so that the vignette builds within CRAN's check-time budget.
pm <- readRDS("power-mixed-cache.rds")

## ----echo = FALSE-------------------------------------------------------------
pm

## -----------------------------------------------------------------------------
model_formula(spec_c, "zcp")
model_formula(spec_c, "intercepts")

## ----eval = FALSE-------------------------------------------------------------
# pm_rule <- power_mixed(spec_c, formula = c("maximal", "zcp", "intercepts"),
#                        fallback = "singular", n_sims = 200)
# pm_rule$rung_shares

## ----eval = FALSE-------------------------------------------------------------
# sweep_spec(spec_c, "fixed$coefficients", design_conditions(effect = 0.06),
#            power_mixed, formula = c("maximal", "zcp", "intercepts"), n_sims = 200)

## ----power-curve, eval = FALSE------------------------------------------------
# curve <- power_curve_mixed(
#   spec_c,
#   subject_ns = c(8, 12, 16, 24, 32, 44, 56),
#   n_sims = 50)
# print(curve[, c("n_subject", "power", "power_lo", "power_hi", "type_m",
#                 "n_returned", "n_converged", "n_singular")], digits = 3)

## ----include = FALSE----------------------------------------------------------
# The result of running exactly the chunk above, precomputed by tools/vignette-caches.R and
# shipped with the package so that the vignette builds within CRAN's check-time budget.
curve <- read.csv("power-curve-cache.csv")

## ----echo = FALSE-------------------------------------------------------------
print(curve[, c("n_subject", "power", "power_lo", "power_hi", "type_m",
                "n_returned", "n_converged", "n_singular")], digits = 3)

## -----------------------------------------------------------------------------
solved <- target_n(curve, target = 0.8)
unlist(solved[c("n", "n_lo", "n_hi")])

## ----fig.width = 6, fig.height = 3.6, dev.args = list(bg = "transparent")-----
# The transparent device canvas is what lets the page colour through. A ggplot
# theme alone cannot do it, since the device paints white underneath. The
# website's dark mode then inverts the figure's ink, so the axes and labels
# follow the theme and the figure carries no opaque matte.
library(ggplot2)
# Each power estimate is a proportion over the replicates that returned an
# estimate, and the curve carries its 95% Wilson interval, which the shaded
# band draws.
ggplot(curve, aes(n_subject, power)) +
  geom_hline(yintercept = 0.8, linetype = 2, colour = "grey60") +
  # The solved sample size and its interval. The dashed horizontal line marks
  # the target and the band marks where the curve reaches it.
  annotate("rect", xmin = solved$lo, xmax = solved$hi, ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = .15) +
  geom_vline(xintercept = solved$value, linetype = 2, colour = "grey60") +
  geom_ribbon(aes(ymin = power_lo, ymax = power_hi),
              alpha = .15, fill = "#2C6FB0") +
  geom_line(colour = "#2C6FB0", linewidth = 0.8) +
  geom_point(colour = "#2C6FB0", size = 2.6) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = expression(italic(N) ~ "subjects"), y = "Power") +
  theme_minimal(base_size = 12) +
  # theme_minimal still paints a white plot.background over the transparent
  # canvas, so both surfaces have to be cleared for the page colour to reach the
  # figure. The ink is left at its default, because the website inverts the
  # figure in dark mode, which turns the dark axis text light, whereas a fixed
  # mid-grey would be inverted into a muddy tan.
  theme(plot.background  = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        panel.grid       = element_line(colour = "grey80"))

## -----------------------------------------------------------------------------
spec_file <- file.path(tempdir(), "priming.json")
writeLines(spec_json(spec_c), spec_file)
power_report(pm, solved = solved, spec_file = spec_file,
             justification = c(effect = paste(
               "A difference of 0.06 on the log scale, about 6% of the reaction time less",
               "its shift, the smallest effect of theoretical interest.")))

## ----eval = FALSE-------------------------------------------------------------
# at_n <- spec_c
# at_n$units$subject$n <- solved$n
# writeLines(spec_json(at_n), "priming.json")
# null_n <- at_n
# null_n$fixed$coefficients$effect <- 0
# report <- power_report(power_mixed(at_n, n_sims = 500), solved = solved,
#                        spec_file = "priming.json",
#                        justification = c(effect = "Why an effect of this size matters."),
#                        null_result = power_mixed(null_n, n_sims = 500))
# cat(report, file = "justification.md")

## ----include = FALSE----------------------------------------------------------
# The quantities the paragraphs below quote, read from the specification. recipe_subjects holds
# the subject counts that the recipe at the end of this section sweeps.
n_item <- spec_c$units$item$n
item_slope_sd <- spec_c$random$item$slopes$effect
subj_slope_sd <- spec_c$random$subject$slopes$effect
true_effect <- spec_c$fixed$coefficients$effect
se_floor <- item_slope_sd / sqrt(n_item)
recipe_subjects <- c(24, 36, 48)
subj_floor <- subj_slope_sd / sqrt(min(recipe_subjects))

## ----eval = FALSE-------------------------------------------------------------
# # Not run here, since it fits 3,000 models: three subject counts, five item counts and
# # 200 replicates at each. Each solve gives the number of items that reaches a power of
# # 0.8 with that many subjects.
# pairs <- lapply(c(24, 36, 48), function(n_subject) {
#   spec <- spec_c
#   spec$units$subject$n <- n_subject
#   items <- sweep_spec(spec, "units$item$n", c(8, 12, 18, 27, 40), power_mixed,
#                       n_sims = 200, .name = "n_item")
#   solved <- target_n(items, target = 0.8)
#   data.frame(n_subject = n_subject, n_item = solved$n,
#              n_item_lo = solved$n_lo, n_item_hi = solved$n_hi)
# })
# do.call(rbind, pairs)

## ----eval = FALSE-------------------------------------------------------------
# power_curve_mixed(
#   spec_c, subject_ns = seq(20, 60, 10), n_sims = 500, workers = 8)

## -----------------------------------------------------------------------------
brms_bridge(spec_c)

