
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

### Word intrusion test

`newsgroup_stm` is an example topic model trained with the data `newsgroup5` using the `stm` package. Currently, this package supports structural topic models / correlated topic models from `stm` and Warp LDA models from `text2vec`.

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
#> An oolong test object with k = 10, 0 coded.
#> Use the method $do_word_intrusion_test() to do word intrusion test.
#> Use the method $lock() to finalize this object and see the results.
```

As instructed, use the method `$do_word_intrusion_test()` to start coding. If you are running this in RStudio, you should see a test screen similar to this:

<img src="man/figures/oolong_demo.gif" align="center" />

After the coding, you need to first lock the test. Then, you can look at the model precision by printing the oolong test.

``` r
oolong_test$lock()
oolong_test
```

### Topic intrusion test

For example, `newsgroup_stm` was generated with the corpus `newsgroup5$text`

``` r
library(tibble)
newsgroup5
#> # A tibble: 4,182 x 3
#>    text                                                  title            ntoken
#>    <chr>                                                 <chr>             <int>
#>  1 "\nI agree.  So why is Cylink the only (and expensiv… sci.crypt           302
#>  2 "[This is a co-authored report from two of us who we… talk.politics.g…   2900
#>  3 "\njubilee, Pope >Leo the 12th had a medallion cast … soc.religion.ch…    463
#>  4 "\n\nAnd the 'Turkish Karabag' is next. As for 'Cypr… talk.politics.m…    929
#>  5 "The subject says what I would like to do, here are … comp.sys.mac.ha…    218
#>  6 "\n\n\n\nAre you related to 'Arromdian' of ASALA/SDP… talk.politics.m…    920
#>  7 "\n#Rick Anderson replied to my letter with...\n#\n#… talk.religion.m…   1760
#>  8 "                                      ^^^^^^\nNo ar… rec.sport.baseb…    302
#>  9 "\nWhy thanks for your reply to my post.  By the way… talk.politics.g…    158
#> 10 " >When some types of client windows are displayed, … comp.windows.x      158
#> # … with 4,172 more rows
```

Creating the oolong test object with the corpus used for training the topic model will generate topic intrusion test cases.

``` r
oolong_test <- create_oolong(newsgroup_stm, newsgroup5$text)
oolong_test
#> An oolong test object with k = 10, 0 coded.
#> Use the method $do_word_intrusion_test() to do word intrusion test.
#> With 41 cases of topic intrusion test. 0 coded.
#> Use the method $do_topic_intrusion_test() to do topic intrusion test.
#> Use the method $lock() to finalize this object and see the results.
```

Similarly, use the `$do_topic_intrusion_test` to code the test cases, lock the test with `$lock()` and then you can look at the TLO (topic log odds) value by printing the oolong test.

``` r
oolong_test$lock()
oolong_test
```

Suggested workflow
------------------

The test makes more sense if more than one coder is involved. A suggested workflow is to create the test, then clone the oolong object. Ask multiple coders to do the test(s) and then summarize the results.

Create a new oolong object.

``` r
oolong_test_rater1 <- create_oolong(newsgroup_stm)
```

Clone the oolong object to be used by other raters.

``` r
oolong_test_rater2 <- clone_oolong(oolong_test_rater1)
```

Ask different coders to code each object and then lock the object.

``` r
## Let rater 1 do the test.
oolong_test_rater1$do_word_intrusion_test()
oolong_test_rater1$lock()

## Let rater 2 do the test.
oolong_test_rater2$do_word_intrusion_test()
oolong_test_rater2$lock()
```

Get a summary of the two objects.

``` r
summarize_oolong(oolong_test_rater1, oolong_test_rater2)
#> Mean model precision: 0.95
#> Quantiles of Model precision: 0.9, 0.925, 0.95, 0.975, 1
#> Krippendorf's alpha: 0
#> K Precision: 0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1
```

About Warp LDA
--------------

There is a subtle difference between the support for `stm` and for `text2vec`.

`newsgroup_warplda` is a Warp LDA object trained with the same dataset as the `newsgroup_stm`

``` r
newsgroup_warplda
#> <WarpLDA>
#>   Inherits from: <LDA>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     components: 23 355 0 26 59 0 0 0 0 0 0 10 0 48 0 106 0 0 0 19 909 7  ...
#>     fit_transform: function (x, n_iter = 1000, convergence_tol = 0.001, n_check_convergence = 10, 
#>     get_top_words: function (n = 10, topic_number = 1L:private$n_topics, lambda = 1) 
#>     initialize: function (n_topics = 10L, doc_topic_prior = 50/n_topics, topic_word_prior = 1/n_topics) 
#>     plot: function (lambda.step = 0.1, reorder.topics = FALSE, doc_len = private$doc_len, 
#>     topic_word_distribution: 0.000355366027007818 0.00318117461512268 0 0.00036824587 ...
#>     transform: function (x, n_iter = 1000, convergence_tol = 0.001, n_check_convergence = 5, 
#>   Private:
#>     calc_pseudo_loglikelihood: function (ptr = private$ptr) 
#>     check_convert_input: function (x) 
#>     components_: 23 355 0 26 59 0 0 0 0 0 0 10 0 48 0 106 0 0 0 19 909 7  ...
#>     doc_len: 96 1016 142 401 55 364 548 89 48 66 109 61 88 82 163 67  ...
#>     doc_topic_distribution: function () 
#>     doc_topic_distribution_with_prior: function () 
#>     doc_topic_matrix: 4 189 13 3 3 15 11 33 0 0 0 0 0 0 10 0 66 0 0 0 0 20 1 0 ...
#>     doc_topic_prior: 0.1
#>     fit_transform_internal: function (model_ptr, n_iter, convergence_tol, n_check_convergence, 
#>     get_c_all: function () 
#>     get_c_all_local: function () 
#>     get_topic_word_count: function () 
#>     init_model_dtm: function (x, ptr = private$ptr) 
#>     internal_matrix_formats: list
#>     is_initialized: FALSE
#>     n_topics: 10
#>     ptr: externalptr
#>     reset_c_local: function () 
#>     run_iter_doc: function (update_topics = TRUE, ptr = private$ptr) 
#>     run_iter_word: function (update_topics = TRUE, ptr = private$ptr) 
#>     set_c_all: function (x) 
#>     set_internal_matrix_formats: function (sparse = NULL, dense = NULL) 
#>     topic_word_distribution_with_prior: function () 
#>     topic_word_prior: 0.01
#>     vocabulary: agre expens game town note great boss doubl salari buy b ...
```

All the API endpoints are the same, except the one for the creation of topic intrusion test cases. You must supply also the `input_dfm`.

``` r
### Just word intrusion test.
oolong_test <- create_oolong(newsgroup_warplda)
oolong_test
#> An oolong test object with k = 10, 0 coded.
#> Use the method $do_word_intrusion_test() to do word intrusion test.
#> Use the method $lock() to finalize this object and see the results.
```

``` r
newsgroup5_dfm
#> Loading required package: quanteda
#> Package version: 1.9.9009
#> Parallel computing: 2 of 4 threads used.
#> See https://quanteda.io for tutorials and examples.
#> 
#> Attaching package: 'quanteda'
#> The following object is masked from 'package:utils':
#> 
#>     View
#> Document-feature matrix of: 4,182 documents, 8,920 features (98.9% sparse) and 1 docvar.
#>        features
#> docs    agre expens game town note great boss doubl salari buy
#>   text1    1      1    1    1    2     2    1     1      1   2
#>   text2    1      0    1    0    7     1    0     0      0   4
#>   text3    0      0    0    0    0     3    0     0      0   0
#>   text4    0      0    0    0    0     1    0     0      0   0
#>   text5    0      0    0    0    0     0    0     0      0   0
#>   text6    0      0    0    1    0     0    0     0      0   0
#> [ reached max_ndoc ... 4,176 more documents, reached max_nfeat ... 8,910 more features ]
```

``` r
oolong_test <- create_oolong(newsgroup_warplda, newsgroup5$text, input_dfm = newsgroup5_dfm)
#> INFO [2020-02-10 16:46:38] iter 5 loglikelihood = -4756410.989
#> INFO [2020-02-10 16:46:38] iter 10 loglikelihood = -4749312.486
#> INFO [2020-02-10 16:46:38] iter 15 loglikelihood = -4749334.684
#> INFO [2020-02-10 16:46:38] early stopping at 15 iteration
oolong_test
#> An oolong test object with k = 10, 0 coded.
#> Use the method $do_word_intrusion_test() to do word intrusion test.
#> With 41 cases of topic intrusion test. 0 coded.
#> Use the method $do_topic_intrusion_test() to do topic intrusion test.
#> Use the method $lock() to finalize this object and see the results.
```

References
----------

1.  Chang, J., Gerrish, S., Wang, C., Boyd-Graber, J. L., & Blei, D. M. (2009). Reading tea leaves: How humans interpret topic models. In Advances in neural information processing systems (pp. 288-296).

------------------------------------------------------------------------

[1] /ˈuːlʊŋ/ 烏龍, literally means "Dark Dragon", is a semi-oxidized tea from Asia. It is very popular in Taiwan, Japan and Hong Kong
