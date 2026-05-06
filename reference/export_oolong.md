# Export a deployable Shiny app from an oolong object into a directory

This function exports your oolong test into a launched Shiny app that is
ideal for online deployment. Deploying the Shiny app online allows
coders to conduct the test online with their browser, rather than having
to install R on their own computer. In contrast to the testing
interfaces launched with methods such as `$do_word_intrusion_test()`,
the deployable version provides data download after the coder finished
coding. Downloaded data can then revert back to a locked oolong object
using `revert_oolong`. Further version might provide solutions to
permanent storage. The deployable Shiny app will be in a directory. The
Shiny app is both launchable with shiny::runApp() and deployable with
rsconnect::deployApp(). Please refer to
[`vignette("deploy", package = "oolong")`](https://gesistsa.github.io/oolong/articles/deploy.md)
for more details.

## Usage

``` r
export_oolong(
  oolong,
  dir = base::tempdir(),
  verbose = TRUE,
  use_full_path = TRUE
)
```

## Arguments

- oolong:

  an oolong object to be exported. Please note that the "witi" type,
  i.e. oolong object with both word and topic intrusion tests, cannot be
  exported. Also the object must not be locked and ever coded.

- dir:

  character string, the directory to be exported. Default to a temporary
  directory

- verbose:

  logical, whether to display information after exporting

- use_full_path:

  logical, whether to expand dir into full path

## Value

directory exported, invisible

## Author

Chung-hong Chan

## Examples

``` r
# Please try this example in interactive R sessions only.
if (interactive()) {
   data(abstracts_stm)
   x <- wi(abstracts_stm)
   export_oolong(x)
}
```
