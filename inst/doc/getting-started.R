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
  name = "priming", seed = 2024,
  design_kind = "within", include_items = TRUE,
  n_subject = 24, n_item = 20,
  factor_name = "condition", lev1 = "related", lev2 = "unrelated",
  intercept = 6.0, effect = 0.05,
  subj_int_sd = 0.12, subj_slope_sd = 0.04, subj_corr = 0.2,
  item_int_sd = 0.08, item_slope_sd = 0.02, item_corr = -0.1,
  family = "shifted_lognormal", resp_name = "RT", sigma = 0.30, shift = 200
))

## -----------------------------------------------------------------------------
cat(spec_json(spec))

## -----------------------------------------------------------------------------
data <- simulate_design(spec)
nrow(data)
head(data)

## -----------------------------------------------------------------------------
identical(simulate_design(spec), simulate_design(spec))

## -----------------------------------------------------------------------------
cat(generate_r_script(spec))

