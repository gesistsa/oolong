# Deploy an oolong test

In most of the time, you should not use this function. You should write
the deployable version of your app into a directory using
`export_oolong` instead. Please refer to
[`vignette("deploy", package = "oolong")`](https://gesistsa.github.io/oolong/articles/deploy.md)
for more details.

## Usage

``` r
deploy_oolong(oolong)
```

## Arguments

- oolong:

  an oolong object to be deployed. Please note that the "witi" type,
  i.e. oolong object with both word and topic intrusion tests, cannot be
  deployed. Also the object must not be locked and ever coded.

## Value

Nothing, it launches a deployable version of the coding interface

## Author

Chung-hong Chan

## Examples

``` r
# Please try this example in interactive R sessions only.
if (interactive()) {
   data(abstracts_stm)
   x <- wi(abstracts_stm)
   deploy_oolong(x)
}
```
