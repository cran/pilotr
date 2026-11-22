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
  name = "demo", seed = 2024, design_kind = "between", n_subject = 200,
  factor_name = "group", lev1 = "a", lev2 = "b",
  intercept = 100, effect = 5, family = "gaussian",
  resp_name = "score", sigma = 10))

identical(simulate_design(spec), simulate_design(spec))

## -----------------------------------------------------------------------------
spec2 <- spec; spec2$seed <- spec$seed + 1L
identical(simulate_design(spec)$score, simulate_design(spec2)$score)

## ----echo = FALSE-------------------------------------------------------------
# Output of python/examples/parity_check.py --csv, precomputed and shipped with
# the package: the comparison needs both a Python and an R installation, so it
# cannot run at vignette build time.
# max_abs_diff is read as text: as a number, kable would round 4.885e-15 to 0
# and the table would claim a stricter result than the check actually produced.
knitr::kable(read.csv("parity-summary.csv",
                      colClasses = c(max_abs_diff = "character")),
             format = "html", table.attr = 'class="table data-output"')

