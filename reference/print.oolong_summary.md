# Print and plot oolong summary

These functions print or plot a useful summary of the results from
[`summarize_oolong`](https://gesistsa.github.io/oolong/reference/summarize_oolong.md).
For details, please see the overview vignette:
[`vignette("overview", package = "oolong")`](https://gesistsa.github.io/oolong/articles/overview.md)

## Usage

``` r
# S3 method for class 'oolong_summary'
print(x, ...)

# S3 method for class 'oolong_summary'
plot(x, ...)
```

## Arguments

- x:

  an oolong_summary

- ...:

  other parameters

## Value

None

## Summary

Print function displays the following information:

- Mean model precision:

  (wi, wsi) Higher value indicates better topic interpretability

- Quantiles of model precision:

  (wi) Higher value indicates better topic interpretability

- P-value of the model precision:

  (wi) Model precision's p-value calculated by one-sample binomial test
  and Fisher's Omnibus method.

- Krippendorff's alpha:

  (wi, wsi, gs) Krippendorff's Alpha, if more than one oolong object is
  analyzed.

- K Precision:

  (wi, wsi) Model precision for each topic.

- Mean TLO:

  (ti) Mean topic log odds, higher value indicates better
  interpretability

- Median TLO:

  (ti) Median topic log odds, higher value indicates better
  interpretability

- Quantiles of TLO:

  (ti) Quantiles of topic log odds

- P-Value of the median TLO:

  (ti) Median topic log odds's p-value calculated by permutation test.

- Correlation (average answer):

  (gs) Pearson's correlation between average answer and target value

- Corrlation (content length):

  (gs) Pearson's correlation between content length and target value

## Diagnostic plot

Plot function displays a diagnostic plot with the following subplots (gs
only).

- Top left:

  Correlation between answer from coders and target value to check for
  correlation between two values. Both axes are minmax transformed.

- Top right:

  Bland-altman plot of answer from coders and target value to check for
  agreement between two values.

- Bottom left:

  Correlation between target value and content length to check for the
  influence of content length.

- Bottom right:

  Cook's distance to check for influential observations.

## Author

Chung-hong Chan
