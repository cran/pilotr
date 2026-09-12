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

## ----list---------------------------------------------------------------------
pilotr_example()

## ----examples, results = "asis"-----------------------------------------------
desc <- c(
  between_2group_gaussian = "Two-group between-subjects Gaussian.",
  crossed_mixed_rt        =
    "Crossed by-subject and by-item reaction times, shifted lognormal.",
  beta_proportion         = "Bounded proportions through the Beta family.",
  ordinal_likert_between  =
    "Five-point Likert responses via a cumulative-logit model.",
  poisson_counts_between  = "Count outcomes through a log link.",
  reading_time_continuous =
    "A continuous predictor with a lognormal reading-time outcome.",
  nested_clusters         =
    "Subjects nested in higher-level clusters, an extra grouping factor.",
  partial_crossing        = "Each subject sees a sampled subset of items."
)

for (name in pilotr_example()) {
  spec <- load_spec(pilotr_example(name))
  d <- simulate_design(spec)
  blurb <- if (name %in% names(desc)) desc[[name]] else ""
  cat(sprintf("\n### %s\n\n", name))
  cat(sprintf("%s The `%s` family, %d rows.\n\n",
              blurb, spec$response$family, nrow(d)))
  # These tables are printed data, so they take the site's code size rather
  # than the prose size a pipe table would inherit. The class is what the
  # stylesheet keys on, and table.attr reaches the output only for
  # format = "html", since pipe output discards it.
  cat(knitr::kable(head(d, 4), format = "html",
                   table.attr = 'class="table data-output"'), sep = "\n")
  cat("\n\n")
}

