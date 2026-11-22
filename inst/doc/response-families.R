## ----include = FALSE----------------------------------------------------------
# The graphics device canvas, not the ggplot theme, is what makes a figure's
# background opaque, so the transparent device is what lets the page colour show
# through. On the package website that matters twice over: the light page is
# warm off-white rather than pure white, and the dark page inverts the figure,
# where an opaque matte would read as a slab in either mode.
knitr::opts_chunk$set(
  collapse = FALSE, comment = "", fig.width = 6, fig.height = 3.4,
  dev.args = list(bg = "transparent")
)
# Console colour carries no meaning on a rendered page. pkgdown turns it on for
# its own build, and the escape sequences then reach the reader as literal text,
# so colour is switched off here for a plain vignette render and a site build
# alike. The fixed width keeps printed output inside the documentation column.
options(cli.num_colors = 1, cli.hyperlink = FALSE, crayon.enabled = FALSE,
        width = 80)

## ----setup--------------------------------------------------------------------
library(pilotr)

## -----------------------------------------------------------------------------
demo <- function(family, intercept, effect, n = 4000, ...) {
  spec <- build_spec(list(
    name = family, seed = 1, design_kind = "between", n_subject = n,
    factor_name = "group", lev1 = "control", lev2 = "treatment",
    intercept = intercept, effect = effect, family = family,
    resp_name = "", ...))
  d <- simulate_design(spec)
  y <- d[[spec$response$name]]
  list(spec = spec, data = d, y = y, by_group = tapply(y, d$group, mean))
}

library(ggplot2)
fam_hist <- function(y, fill, title, xlab) {
  ggplot(data.frame(y = y), aes(y)) +
    geom_histogram(bins = 40, fill = fill, colour = NA) +
    labs(title = title, x = xlab, y = "count") +
    theme_minimal(base_size = 12) +
    # theme_minimal still paints a white plot.background over the transparent
    # device canvas, so both surfaces have to be cleared for the page colour to
    # reach the figure. The ink stays at its default, because the website
    # inverts the figure in dark mode, which turns the dark axis text light of
    # its own accord.
    theme(plot.background  = element_rect(fill = NA, colour = NA),
          panel.background = element_rect(fill = NA, colour = NA),
          panel.grid       = element_line(colour = "grey80"))
}

## -----------------------------------------------------------------------------
g <- demo("gaussian", intercept = 100, effect = 5, sigma = 10)
round(g$by_group, 2)
fam_hist(g$y, "#2C6FB0", "Gaussian", "score")

## -----------------------------------------------------------------------------
rt <- demo(
  "shifted_lognormal", intercept = 6, effect = 0.1, sigma = 0.3, shift = 200
)
round(rt$by_group, 1)
fam_hist(rt$y, "#B0402C", "Shifted lognormal (RT)", "RT (ms)")

## -----------------------------------------------------------------------------
ln <- demo("lognormal", intercept = 6, effect = 0.1, sigma = 0.3)
round(ln$by_group, 1)
fam_hist(ln$y, "#7A4FB0", "Lognormal", "reading time (ms)")

## -----------------------------------------------------------------------------
acc <- demo("bernoulli", intercept = 0, effect = 0.5)
round(acc$by_group, 3)   # P(correct) by group

## -----------------------------------------------------------------------------
cts <- demo("poisson", intercept = 1.5, effect = 0.3)
round(cts$by_group, 2)   # mean count by group
table(cts$y)[1:8]

## -----------------------------------------------------------------------------
ord <- build_spec(list(
  name = "likert", seed = 1, design_kind = "between", n_subject = 4000,
  factor_name = "group", lev1 = "control", lev2 = "treatment",
  intercept = 0, effect = 0.8, family = "ordinal", resp_name = "rating",
  thresholds = "-2, -0.6, 0.6, 2"))
r <- simulate_design(ord)
# category proportions by group
round(prop.table(table(r$group, r$rating), 1), 2)

## -----------------------------------------------------------------------------
bt <- demo("beta", intercept = 0, effect = 0.8, phi = 8)
round(bt$by_group, 3)   # mean proportion by group
fam_hist(bt$y, "#2E8B57", "Beta", "proportion")

## -----------------------------------------------------------------------------
spec <- build_spec(list(
  name = "reading", seed = 1, design_kind = "within", include_items = TRUE,
  n_subject = 12, n_item = 24,
  factor_name = "condition", lev1 = "related", lev2 = "unrelated",
  intercept = 6, effect = 0.05,
  subj_int_sd = 0.12, subj_slope_sd = 0.04, subj_corr = 0.2,
  item_int_sd = 0.08, item_slope_sd = 0.02, item_corr = -0.1,
  family = "lognormal", resp_name = "RT", sigma = 0.25))

spec$predictors <- list(
  list(name = "freq", varies_by = "item", mean = 0, sd = 1)
)
# an interaction with the effect
spec$fixed$coefficients[["effect:freq"]] <- 0.02
# each subject sees 10 of the 24 items
spec$units$item$per_subject <- 10
# subjects nested in classes
spec$random$class <- list(over = "subject", n = 6, intercept_sd = 0.05)

head(simulate_design(spec))

## -----------------------------------------------------------------------------
model_formula(spec)

