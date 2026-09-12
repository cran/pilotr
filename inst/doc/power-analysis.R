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
spec_c <- build_spec(list(
  name = "priming", seed = 1, design_kind = "within", include_items = TRUE,
  n_subject = 24, n_item = 18,
  factor_name = "condition", lev1 = "related", lev2 = "unrelated",
  intercept = 6, effect = 0.06,
  subj_int_sd = 0.12, subj_slope_sd = 0.04, subj_corr = 0.2,
  item_int_sd = 0.08, item_slope_sd = 0.02, item_corr = -0.1,
  family = "shifted_lognormal", resp_name = "RT", sigma = 0.3, shift = 200))

## ----eval = FALSE-------------------------------------------------------------
# # A tiny replicate count keeps the vignette fast. Use 200 or more for real planning.
# pm <- power_mixed(spec_c, n_sims = 20)
# unlist(pm[c("power", "type_s", "type_m", "n_converged")])

## ----include = FALSE----------------------------------------------------------
# The result of running exactly the chunk above, precomputed and shipped with
# the package so that the vignette builds within CRAN's check-time budget.
pm <- as.list(read.csv("power-mixed-cache.csv"))

## ----echo = FALSE-------------------------------------------------------------
unlist(pm[c("power", "type_s", "type_m", "n_converged")])

## ----eval = FALSE-------------------------------------------------------------
# curve <- power_curve_mixed(
#   spec_c,
#   subject_ns = c(8, 12, 16, 24, 32, 44, 56),
#   n_sims = 50)
# curve

## ----include = FALSE----------------------------------------------------------
# The result of running exactly the chunk above, precomputed and shipped with
# the package so that the vignette builds within CRAN's check-time budget.
curve <- read.csv("power-curve-cache.csv")

## ----echo = FALSE-------------------------------------------------------------
curve

## -----------------------------------------------------------------------------
solved <- target_n(curve, target = 0.8)
unlist(solved[c("n", "n_lo", "n_hi")])

## ----fig.width = 6, fig.height = 3.6, dev.args = list(bg = "transparent")-----
# The transparent device canvas is what lets the page colour through. A ggplot
# theme alone cannot do it, since the device paints white underneath. The
# website's dark mode then inverts the figure's ink, so the axes and labels
# follow the theme and the figure carries no opaque matte.
library(ggplot2)
# Each power estimate is a proportion over the converged replicates, so it
# carries a binomial Monte Carlo standard error. The shaded band is the 95%
# interval.
curve$se <- sqrt(curve$power * (1 - curve$power) / curve$n_converged)
ggplot(curve, aes(n_subject, power)) +
  geom_hline(yintercept = 0.8, linetype = 2, colour = "grey60") +
  # The solved sample size and its interval. The dashed horizontal line marks
  # the target and the band marks where the curve reaches it.
  annotate("rect", xmin = solved$lo, xmax = solved$hi, ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = .15) +
  geom_vline(xintercept = solved$value, linetype = 2, colour = "grey60") +
  geom_ribbon(aes(ymin = pmax(0, power - 1.96 * se),
                  ymax = pmin(1, power + 1.96 * se)),
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

## ----eval = FALSE-------------------------------------------------------------
# power_curve_mixed(
#   spec_c, subject_ns = seq(20, 60, 10), n_sims = 500, workers = 8)

## -----------------------------------------------------------------------------
brms_bridge(spec_c)

