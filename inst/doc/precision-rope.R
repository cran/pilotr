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
spec_c <- build_spec(list(
  name = "priming", seed = 1, design_kind = "within", include_items = TRUE,
  n_subject = 24, n_item = 18,
  factor_name = "condition", lev1 = "related", lev2 = "unrelated",
  intercept = 6, effect = 0.05,
  subj_int_sd = 0.12, subj_slope_sd = 0.04, subj_corr = 0.2,
  item_int_sd = 0.08, item_slope_sd = 0.02, item_corr = -0.1,
  family = "shifted_lognormal", resp_name = "RT", sigma = 0.3, shift = 200))

## -----------------------------------------------------------------------------
model_formula(spec_c)

## -----------------------------------------------------------------------------
head(model_data(spec_c, simulate_design(spec_c)))

## -----------------------------------------------------------------------------
pr <- precision_design(
  spec_c, focal = c(effect = 0.05), rope = 0.02, n_sims = 25)
pr

## ----eval = FALSE-------------------------------------------------------------
# prc <- precision_curve(spec_c, focal = c(effect = 0.05),
#                        subject_ns = c(15, 30, 60, 100, 140, 180, 220, 260),
#                        rope = 0.02, n_sims = 200)
# prc[, c("n_subject", "p_meaningful", "p_equivalent", "mean_ci_width",
#         "n_converged")]

## ----include = FALSE----------------------------------------------------------
# The result of running exactly the chunk above, precomputed and shipped with
# the package so that the vignette builds within CRAN's check-time budget. All
# 200 replicates converged at every sample size.
prc <- read.csv("precision-curve-cache.csv")

## ----echo = FALSE-------------------------------------------------------------
prc[, c("n_subject", "p_meaningful", "p_equivalent", "mean_ci_width",
        "n_converged")]

## -----------------------------------------------------------------------------
solved <- solve_curve(prc, target = 0.9)
unlist(solved[c("value", "lo", "hi", "dispersion")])

## ----fig.width = 6.5, fig.height = 3.2, dev.args = list(bg = "transparent")----
library(ggplot2)
# Each probability is a proportion over the converged replicates, so it carries
# a binomial Monte Carlo standard error, and the band is its 95% interval. The
# two panels are on different scales, hence the free y axis.
prc$se <- sqrt(prc$p_meaningful * (1 - prc$p_meaningful) / prc$n_converged)
panels <- c("P(CI outside the ROPE)", "Mean 95% CI width")
long <- rbind(
  data.frame(n_subject = prc$n_subject, panel = panels[1], y = prc$p_meaningful,
             lo = pmax(0, prc$p_meaningful - 1.96 * prc$se),
             hi = pmin(1, prc$p_meaningful + 1.96 * prc$se)),
  data.frame(n_subject = prc$n_subject, panel = panels[2],
             y = prc$mean_ci_width,
             lo = NA, hi = NA))
long$panel <- factor(long$panel, levels = panels)
# 0.90 is the target decision probability. 0.06 is the width at which a CI
# centred on the true effect just clears the ROPE, that is 2 * (0.05 - 0.02).
refs <- data.frame(panel = factor(panels, levels = panels), ref = c(0.90, 0.06))
# The solved sample size and its interval, drawn only on the decision panel,
# since the width panel is on a different scale and answers a different question.
solve_band <- data.frame(panel = factor(panels[1], levels = panels),
                         lo = solved$lo, hi = solved$hi, at = solved$value)

ggplot(long, aes(n_subject, y)) +
  geom_hline(data = refs, aes(yintercept = ref), linetype = 2,
             colour = "grey60") +
  geom_rect(data = solve_band, inherit.aes = FALSE,
            aes(xmin = lo, xmax = hi, ymin = -Inf, ymax = Inf),
            fill = "grey60", alpha = .15) +
  geom_vline(data = solve_band, aes(xintercept = at), linetype = 2,
             colour = "grey60") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .15, fill = "#2C6FB0",
              na.rm = TRUE) +
  geom_line(colour = "#2C6FB0", linewidth = 0.8) +
  geom_point(colour = "#2C6FB0", size = 2.2) +
  facet_wrap(~ panel, scales = "free_y") +
  labs(x = expression(italic(N) ~ "subjects"), y = NULL) +
  theme_minimal(base_size = 12) +
  # theme_minimal still paints a white plot.background over the transparent
  # canvas, so both surfaces have to be cleared for the page colour to reach the
  # figure.
  theme(plot.background  = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        panel.grid       = element_line(colour = "grey80"),
        strip.background = element_rect(fill = NA, colour = NA))

