BTM
================
Chung-hong Chan

The package BTM by Jan Wijffels et al. finds “topics in collections of
short text”. Compared to other topic model packages, BTM requires a
special data format for training. Oolong has no problem generating word
intrusion tests with BTM. However, that special data format can make
creation of topic intrusion tests very tricky.

This guide provides our recommendations on how to use BTM, so that the
model can be used for generating topic intrusion tests.

# Requirement \#1: Keep your quanteda corpus

It is because every document has a unique document id.

``` r
require(BTM)
#> Loading required package: BTM
require(quanteda)
#> Loading required package: quanteda
#> Package version: 3.0.0
#> Unicode version: 13.0
#> ICU version: 66.1
#> Parallel computing: 8 of 8 threads used.
#> See https://quanteda.io for tutorials and examples.
require(oolong)
#> Loading required package: oolong
trump_corpus <- corpus(trump2k)
```

And then you can do regular text cleaning, stemming procedure with
`quanteda`. Instead of making the product a `DFM` object, make it a
`token` object. You may read [this
issue](https://github.com/quanteda/quanteda/issues/1404) by Benoit et
al.

``` r
tokens(trump_corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, split_hyphens = TRUE, remove_url = TRUE) %>% tokens_tolower() %>% tokens_remove(stopwords("en")) %>% tokens_remove("@*")  -> trump_toks
```

# Requirement \#2: Keep your data frame

Use this function to convert the `token` object to a data frame.

``` r
as.data.frame.tokens <- function(x) {
  data.frame(
    doc_id = rep(names(x), lengths(x)),
    tokens = unlist(x, use.names = FALSE)
  )
}

trump_dat <- as.data.frame.tokens(trump_toks)
```

Train a BTM model

``` r
trump_btm <- BTM(trump_dat, k = 8, iter = 500, trace = 10)
#> 2021-05-11 16:38:50 Start Gibbs sampling iteration 1/500
#> 2021-05-11 16:38:50 Start Gibbs sampling iteration 11/500
#> 2021-05-11 16:38:50 Start Gibbs sampling iteration 21/500
#> 2021-05-11 16:38:50 Start Gibbs sampling iteration 31/500
#> 2021-05-11 16:38:50 Start Gibbs sampling iteration 41/500
#> 2021-05-11 16:38:51 Start Gibbs sampling iteration 51/500
#> 2021-05-11 16:38:51 Start Gibbs sampling iteration 61/500
#> 2021-05-11 16:38:51 Start Gibbs sampling iteration 71/500
#> 2021-05-11 16:38:51 Start Gibbs sampling iteration 81/500
#> 2021-05-11 16:38:51 Start Gibbs sampling iteration 91/500
#> 2021-05-11 16:38:52 Start Gibbs sampling iteration 101/500
#> 2021-05-11 16:38:52 Start Gibbs sampling iteration 111/500
#> 2021-05-11 16:38:52 Start Gibbs sampling iteration 121/500
#> 2021-05-11 16:38:52 Start Gibbs sampling iteration 131/500
#> 2021-05-11 16:38:53 Start Gibbs sampling iteration 141/500
#> 2021-05-11 16:38:53 Start Gibbs sampling iteration 151/500
#> 2021-05-11 16:38:53 Start Gibbs sampling iteration 161/500
#> 2021-05-11 16:38:53 Start Gibbs sampling iteration 171/500
#> 2021-05-11 16:38:53 Start Gibbs sampling iteration 181/500
#> 2021-05-11 16:38:54 Start Gibbs sampling iteration 191/500
#> 2021-05-11 16:38:54 Start Gibbs sampling iteration 201/500
#> 2021-05-11 16:38:54 Start Gibbs sampling iteration 211/500
#> 2021-05-11 16:38:54 Start Gibbs sampling iteration 221/500
#> 2021-05-11 16:38:54 Start Gibbs sampling iteration 231/500
#> 2021-05-11 16:38:55 Start Gibbs sampling iteration 241/500
#> 2021-05-11 16:38:55 Start Gibbs sampling iteration 251/500
#> 2021-05-11 16:38:55 Start Gibbs sampling iteration 261/500
#> 2021-05-11 16:38:55 Start Gibbs sampling iteration 271/500
#> 2021-05-11 16:38:55 Start Gibbs sampling iteration 281/500
#> 2021-05-11 16:38:56 Start Gibbs sampling iteration 291/500
#> 2021-05-11 16:38:56 Start Gibbs sampling iteration 301/500
#> 2021-05-11 16:38:56 Start Gibbs sampling iteration 311/500
#> 2021-05-11 16:38:56 Start Gibbs sampling iteration 321/500
#> 2021-05-11 16:38:56 Start Gibbs sampling iteration 331/500
#> 2021-05-11 16:38:57 Start Gibbs sampling iteration 341/500
#> 2021-05-11 16:38:57 Start Gibbs sampling iteration 351/500
#> 2021-05-11 16:38:57 Start Gibbs sampling iteration 361/500
#> 2021-05-11 16:38:57 Start Gibbs sampling iteration 371/500
#> 2021-05-11 16:38:57 Start Gibbs sampling iteration 381/500
#> 2021-05-11 16:38:58 Start Gibbs sampling iteration 391/500
#> 2021-05-11 16:38:58 Start Gibbs sampling iteration 401/500
#> 2021-05-11 16:38:58 Start Gibbs sampling iteration 411/500
#> 2021-05-11 16:38:58 Start Gibbs sampling iteration 421/500
#> 2021-05-11 16:38:58 Start Gibbs sampling iteration 431/500
#> 2021-05-11 16:38:59 Start Gibbs sampling iteration 441/500
#> 2021-05-11 16:38:59 Start Gibbs sampling iteration 451/500
#> 2021-05-11 16:38:59 Start Gibbs sampling iteration 461/500
#> 2021-05-11 16:38:59 Start Gibbs sampling iteration 471/500
#> 2021-05-11 16:38:59 Start Gibbs sampling iteration 481/500
#> 2021-05-11 16:39:00 Start Gibbs sampling iteration 491/500
```

## Pecularities of BTM

This is how you should generate \(\theta_{t}\) . However, there are many
NaN and there are only 1994 rows (`trump2k` has 2000 tweets) due to
empty documents.

``` r
theta <- predict(trump_btm, newdata = trump_dat)
dim(theta)
#> [1] 1994    8
```

``` r
setdiff(docid(trump_corpus), row.names(theta))
#> [1] "text604"  "text633"  "text659"  "text1586" "text1587" "text1761"
```

``` r
trump_corpus[604]
#> Corpus consisting of 1 document.
#> text604 :
#> "http://t.co/PtViAyrO4A"
```

Also, the row order is messed up.

``` r
head(row.names(theta), 100)
#>   [1] "text1"    "text10"   "text100"  "text1000" "text1001" "text1002"
#>   [7] "text1003" "text1004" "text1005" "text1006" "text1007" "text1008"
#>  [13] "text1009" "text101"  "text1010" "text1011" "text1012" "text1013"
#>  [19] "text1014" "text1015" "text1016" "text1017" "text1018" "text1019"
#>  [25] "text102"  "text1020" "text1021" "text1022" "text1023" "text1024"
#>  [31] "text1025" "text1026" "text1027" "text1028" "text1029" "text103" 
#>  [37] "text1030" "text1031" "text1032" "text1033" "text1034" "text1035"
#>  [43] "text1036" "text1037" "text1038" "text1039" "text104"  "text1040"
#>  [49] "text1041" "text1042" "text1043" "text1044" "text1045" "text1046"
#>  [55] "text1047" "text1048" "text1049" "text105"  "text1050" "text1051"
#>  [61] "text1052" "text1053" "text1054" "text1055" "text1056" "text1057"
#>  [67] "text1058" "text1059" "text106"  "text1060" "text1061" "text1062"
#>  [73] "text1063" "text1064" "text1065" "text1066" "text1067" "text1068"
#>  [79] "text1069" "text107"  "text1070" "text1071" "text1072" "text1073"
#>  [85] "text1074" "text1075" "text1076" "text1077" "text1078" "text1079"
#>  [91] "text108"  "text1080" "text1081" "text1082" "text1083" "text1084"
#>  [97] "text1085" "text1086" "text1087" "text1088"
```

# Oolong’s support for BTM

Oolong has no problem generating word intrusion test for BTM like you do
with other topic models.

``` r
oolong <- create_oolong(trump_btm)
oolong
#> 
#> ── oolong (topic model) ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> ✔ WI ✖ TI
#> ℹ WI: k = 8, 0 coded.
#> 
#> ── Methods ──
#> 
#> ● $do_word_intrusion_test(): do word intrusion test
#> ● $lock(): finalize and see the results
```

For generating topic intrusion tests, however, you must provide the data
frame you used for training (in this case `trump_dat`). Your
`input_corpus` must be a quanteda corpus too.

``` r
oolong <- create_oolong(trump_btm, trump_corpus, btm_dataframe = trump_dat)
oolong
#> 
#> ── oolong (topic model) ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> ✔ WI ✔ TI
#> ℹ WI: k = 8, 0 coded.
#> ℹ TI: n = 20, 0 coded.
#> 
#> ── Methods ──
#> 
#> ● $do_word_intrusion_test(): do word intrusion test
#> ● $do_topic_intrusion_test(): do topic intrusion test
#> ● $lock(): finalize and see the results
```

`btm_dataframe` must not be NULL.

``` r
oolong <- create_oolong(trump_btm, trump_corpus)
#> Error in .extract_ingredients.input_model_s3_btm(.convert_input_model_s3(input_model), : You need to provide input_corpus (in quanteda format) and btm_dataframe for generating topic intrusion tests.
```

`input_corpus` must be a quanteda corpus.

``` r
oolong <- create_oolong(trump_btm, trump2k, btm_dataframe = trump_dat)
#> Error in .extract_ingredients.input_model_s3_btm(.convert_input_model_s3(input_model), : You need to provide input_corpus (in quanteda format) and btm_dataframe for generating topic intrusion tests.
```
