# Changelog

## oolong 0.7.0

- CRAN release

## oolong 0.6.2

- Add content for MH
- Use `icr` for the calculation of Krippendorff’s Alpha

## oolong 0.6.1

CRAN release: 2024-04-15

- Add fixes for quanteda 4.0.0.

## oolong 0.6.0

CRAN release: 2024-02-10

- Use seededlda instead of keyATM as demo, so that we can reduce the
  version requirement.

## oolong 0.5.1

- Transfer ownership to `gesistsa`.
- Add pkgdown website and clean up many documents.

## oolong 0.5.0

CRAN release: 2023-08-25

- Potential breaking change: the parameter `difficulty` is deprecated.
  Instead, please use the respective `frewweight` (for STM) and `lambda`
  (for Warp LDA). For legacy code explicitly using the `difficulty`
  parameter, this change does not break your code. However, if you use
  the following pattern, you might need to change your legacy code
  accordingly.

``` r

## This code is okay
set.seed(123)
wsi(abstracts_stm, use_frex_words = TRUE, difficulty = 0.8)

set.seed(123)
## You will get different results with oolong 0.5.0
wsi(abstracts_stm, use_frex_words = TRUE)

## You need to explicitly use the old default, which is quite high
set.seed(123)
wsi(abstracts_stm, use_frex_words = TRUE, frexweight = 1)
```

- The package-level documentation is removed.

- Increase the required R version to 4.0 because of `keyATM`

## oolong 0.4.3

CRAN release: 2023-06-11

- Upgrade Shiny test cases to `shinytest2`
- Clarify the coding task can be paused, saved, and resumed in the
  Vignette
- Package maintenance

## oolong 0.4.1

CRAN release: 2021-11-09

- Eliminate `miniUI` as a dependency.
- Update the documentation to reflect newly published papers, e.g. Ying
  et al.

## oolong 0.4.0

CRAN release: 2021-05-31

- Add `export_oolong` and `deploy_oolong` for online deployment \[thanks
  Marius Sältzer, Daniel Braby (and his friend Louis), Johannes Gruber
  and Felicia Loecherbach for testing this feature; thanks SAGE Ocean
  for the concept grant to support the development of this feature\]
- Support models from `seededlda` \[thanks Marius Sältzer\]
- Support Naive Bayes models from `quanteda.textmodels` \[thanks Marius
  Sältzer\]
- Support generation of word set intrusion test (Ying et
  al. forthcoming)
- Support generation of oolong object with only topic intrusion test
- Add new wrappers: `wi`, `ti`, `witi`, `wsi`, and `gs`
- Add `userid` as an suggested parameter
- Total revamp of the object of all oolong tests; add more meta data.
  Add `update_oolong` for updating object created by older versions of
  oolong
- Update the print method of all oolong tests; it is now based on `cli`
- Various bug fixes; all Shiny components are now automatically tested

## oolong 0.3.11

CRAN release: 2020-11-13

- Support BTM \[thanks Marius Sältzer\]
- Update Shiny UI (with jump button)
- Various bug fixes

## oolong 0.3.4

CRAN release: 2020-03-21

- Initial CRAN version.
