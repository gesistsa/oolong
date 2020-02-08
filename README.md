
<!-- README.md is generated from README.Rmd. Please edit that file -->

# oolong <img src="man/figures/oolong_logo.png" align="right" height="200" />

<!-- badges: start -->

<!-- badges: end -->

The goal of oolong \[1\] is to generate and administrate validation
tests easily for typical automated content analysis tools such as topic
models and dictionary-based tools.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("chainsawriot/oolong")
```

## Example

### Word intrusion test

`newsgroup_stm` is an example topic model trained with the data
`newsgroup5` using the `stm` package. Currently, this package supports
structural topic models / correlated topic models from `stm` and Wrap
LDA models from `text2vec`.

``` r
library(oolong)
library(stm)
#> stm v1.3.3 (2018-1-26) successfully loaded. See ?stm for help. 
#>  Papers, resources, and other materials at structuraltopicmodel.com
newsgroup_stm
#> A topic model with 10 topics, 4182 documents and a 8920 word dictionary.
```

To create an oolong test, use the function `create_oolong_test`.

``` r
oolong_test <- create_oolong(newsgroup_stm)
oolong_test
#> An oolong test object with k = 10, 0 coded.
#> Use the method $do_word_intrusion_test() to do word intrusion test.
#> Use the method $finalize() to finalize this object and see the results.
```

As instructed, use the method `$do_word_intrusion_test()` to start
coding. If you are running this in RStudio, you should see a test screen
similar to this:

<img src="man/figures/oolong_demo.gif" align="center" />

After the coding, you need to first finalize the test. Then, you can
look at the model precision by printing the oolong test.

``` r
oolong_test$finalize()
oolong_test
```

### Topic intrusion test

For example, `newsgroup_stm` was generated with the corpus
`newsgroup5$text`

``` r
library(tibble)
newsgroup5
#> # A tibble: 4,182 x 3
#>    text                                              title           ntoken
#>    <chr>                                             <chr>            <int>
#>  1 "\nI agree.  So why is Cylink the only (and expe… sci.crypt          302
#>  2 "[This is a co-authored report from two of us wh… talk.politics.…   2900
#>  3 "\njubilee, Pope >Leo the 12th had a medallion c… soc.religion.c…    463
#>  4 "\n\nAnd the 'Turkish Karabag' is next. As for '… talk.politics.…    929
#>  5 "The subject says what I would like to do, here … comp.sys.mac.h…    218
#>  6 "\n\n\n\nAre you related to 'Arromdian' of ASALA… talk.politics.…    920
#>  7 "\n#Rick Anderson replied to my letter with...\n… talk.religion.…   1760
#>  8 "                                      ^^^^^^\nN… rec.sport.base…    302
#>  9 "\nWhy thanks for your reply to my post.  By the… talk.politics.…    158
#> 10 " >When some types of client windows are display… comp.windows.x     158
#> # … with 4,172 more rows
```

Create the oolong test object with the corpus used for training the
topic model will generate topic intrusion test cases.

``` r
oolong_test <- create_oolong(newsgroup_stm, newsgroup5$text)
oolong_test
#> An oolong test object with k = 10, 0 coded.
#> Use the method $do_word_intrusion_test() to do word intrusion test.
#> With 15 cases of topic intrusion test. 0 coded.
#> Use the method $do_topic_intrusion_test() to do topic intrusion test.
#> Use the method $finalize() to finalize this object and see the results.
```

1.  /ˈuːlʊŋ/ 烏龍, literally means “Dark Dragon”, is a semi-oxidized tea
    from Asia. It is very popular in Taiwan, Japan and Hong Kong
