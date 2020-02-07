
<!-- README.md is generated from README.Rmd. Please edit that file -->
oolong <img src="man/figures/oolong_logo.png" align="right" height="200" />
===========================================================================

<!-- badges: start -->
<!-- badges: end -->
The goal of oolong [1] is to generate and administrate validation tests easily for typical automated content analysis tools such as topic models and dictionary-based tools.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("chainsawriot/oolong")
```

Example
-------

`newsgroup_stm` is an example topic model trained with the data `newsgroup5` using the `stm` package. Currently, this package supports structural topic models / correlated topic models from `stm` and Wrap LDA models from `text2vec`.

``` r
library(oolong)
library(stm)
#> stm v1.3.5 successfully loaded. See ?stm for help. 
#>  Papers, resources, and other materials at structuraltopicmodel.com
newsgroup_stm
#> A topic model with 10 topics, 4182 documents and a 8920 word dictionary.
```

To create an oolong test, use the function `create_oolong_test`.

``` r
oolong_test <- create_oolong(newsgroup_stm)
oolong_test
#> An oolong test object with k = 10, 0 coded. (0%  precision)
#>  Use the method $do_word_intrusion_test() to start word instrusion test.
```

As instructed, use the method `$do_word_intrusion_test()` to start coding. If you are running this in RStudio, you should see a test screen similar to this:

<img src="man/figures/oolong_demo.gif" align="center" />

After the coding, you can look at the accuracy rate by printing the oolong test.

``` r
oolong_test
```

[1] /ˈuːlʊŋ/ 烏龍, literally means "Dark Dragon", is a semi-oxidized tea from Asia. It is very popular in Taiwan, Japan and Hong Kong
