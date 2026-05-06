# Generate an oolong test

`create_oolong` generates an oolong test object that can either be used
for validating a topic model or for creating ground truth (gold
standard) of a text corpus. `wi` (word intrusion test), `ti` (topic
intrusion test), `witi` (word and topic intrusion tests), `wsi` (word
set intrusion test) and `gs` are handy wrappers to `create_oolong`. It
is recommended to use these wrappers instead of `create_oolong`.

## Usage

``` r
create_oolong(
  input_model = NULL,
  input_corpus = NULL,
  n_top_terms = 5,
  bottom_terms_percentile = 0.6,
  exact_n = NULL,
  frac = 0.01,
  n_top_topics = 3,
  n_topiclabel_words = 8,
  use_frex_words = FALSE,
  frexweight = 0.5,
  input_dfm = NULL,
  construct = "positive",
  btm_dataframe = NULL,
  n_correct_ws = 3,
  wsi_n_top_terms = 20,
  userid = NA,
  type = "witi",
  lambda = 1,
  difficulty = NULL
)

wi(
  input_model = NULL,
  userid = NA,
  n_top_terms = 5,
  bottom_terms_percentile = 0.6,
  frexweight = 0.5,
  use_frex_words = FALSE,
  lambda = 1,
  difficulty = NULL
)

witi(
  input_model = NULL,
  input_corpus = NULL,
  userid = NA,
  n_top_terms = 5,
  bottom_terms_percentile = 0.6,
  exact_n = NULL,
  frac = 0.01,
  n_top_topics = 3,
  n_topiclabel_words = 8,
  frexweight = 0.5,
  use_frex_words = FALSE,
  input_dfm = NULL,
  btm_dataframe = NULL,
  lambda = 1,
  difficulty = NULL
)

ti(
  input_model = NULL,
  input_corpus = NULL,
  userid = NA,
  exact_n = NULL,
  frac = 0.01,
  n_top_topics = 3,
  n_topiclabel_words = 8,
  frexweight = 0.5,
  use_frex_words = FALSE,
  input_dfm = NULL,
  btm_dataframe = NULL,
  lambda = 1,
  difficulty = NULL
)

wsi(
  input_model = NULL,
  userid = NA,
  n_topiclabel_words = 4,
  n_correct_ws = 3,
  wsi_n_top_terms = 20,
  frexweight = 0.5,
  use_frex_words = FALSE,
  lambda = 1,
  difficulty = NULL
)

gs(
  input_corpus = NULL,
  userid = NA,
  construct = "positive",
  exact_n = NULL,
  frac = 0.01
)
```

## Arguments

- input_model:

  (wi, ti, witi, wsi) a STM, WarpLDA, topicmodels, KeyATM, seededlda,
  textmodel_nb, or BTM object; if it is NULL, create_oolong assumes that
  you want to create gold standard.

- input_corpus:

  (wi, ti, witi, wsi, gs) if input_model is not null, it should be the
  corpus (character vector or quanteda::corpus object) to generate the
  model object. If input_model and input_corpus are not NULL, topic
  intrusion test cases are generated. If input_model is a BTM object,
  this argument is ignored. If input_model is null, it generates gold
  standard test cases.

- n_top_terms:

  (wi, witi) integer, number of top topic words to be included in the
  candidates of word intrusion test.

- bottom_terms_percentile:

  (wi, witi) double, a term is considered to be an word intruder when
  its theta less than the percentile of this theta, must be within the
  range of 0 to 1

- exact_n:

  (ti, witi, gs) integer, number of topic intrusion test cases to
  generate, ignore if frac is not NULL

- frac:

  (ti, witi, gs) double, fraction of test cases to be generated from the
  corpus

- n_top_topics:

  (wi, witi) integer, number of most relevant topics to be shown
  alongside the intruder topic

- n_topiclabel_words:

  (witi, ti, wsi) integer, number of topic words to be shown as the
  topic ("ti" and "witi") / word set ("wsi") label

- use_frex_words:

  (wi, witi, ti, wsi) logical, for a STM object, use FREX words if TRUE,
  use PROB words if FALSE

- frexweight:

  (wi, witi, ti, wsi) double, adjust the \`frexweight\` for STM (see
  \[stm::labelTopics()\]), no effect for STM if use_frex_words is FALSE

- input_dfm:

  (wi, witi, ti, wsi) a dfm object used for training the input_model, if
  input_model is a WarpLDA object

- construct:

  (gs) string, an adjective to describe the construct you want your
  coders to code the the gold standard test cases

- btm_dataframe:

  (witi, ti) dataframe used for training the input_model, if input_model
  is a BTM object

- n_correct_ws:

  (wsi) number of word sets to be shown alongside the intruder word set

- wsi_n_top_terms:

  (wsi) number of top topic words from each topic to be randomized
  selected as the word set label

- userid:

  a character string to denote the name of the coder. Default to NA (no
  userid); not recommended

- type:

  (create_oolong) a character string to denote what you want to create.
  "wi": word intrusion test; "ti": topic intrusion test; "witi": both
  word intrusion test and topic intrusion test; "gs": gold standard
  generation

- lambda:

  (wi, witi, ti, wsi) double, adjust the \`lambda\` for WarpLDA (see
  \[text2vec::LatentDirichletAllocation()\])

- difficulty:

  (wi, witi, ti, wsi) double, deprecated, for backward compatibility

## Value

an oolong test object.

## Usage

Use `wi`, `ti`, `witi`, `wsi` or `gs` to generate an oolong test of your
choice. It is recommended to supply also `userid` (current coder). The
names of the tests (word intrusion test and topic intrusion test) follow
Chang et al (2009). In Ying et al. (2021), topic intrusion test is named
"T8WSI" (Top 8 Word Set Intrusion). Word set intrusion test in this
package is actually the "R4WSI" (Random 4 Word Set Intrusion) in Ying et
al. The default settings of `wi`, `witi`, and `ti` follow Chang et al
(2009), e.g. `n_top_terms` = 5; instead of `n_top_terms` = 4 as in Ying
et al. The default setting of `wsi` follows Ying et al., e.g.
`n_topiclabel_words` = 4. As suggested by Song et al. (2020), 1

## About create_oolong

Because `create_oolong` is not intuitive to use, it is no longer
recommended to use `create_oolong` to generate oolong test.
`create_oolong` is retained only for backward compatibility purposes.
This function generates an oolong test object based on `input_model` and
`input_corpus`. If `input_model` is not NULL, it generates oolong test
for a topic model (tm). If `input_model` is NULL but input_corpus is not
NULL, it generates oolong test for generating gold standard (gs).

## Methods

An oolong object, depends on its purpose, has the following methods:

- `$do_word_intrusion_test()`:

  (tm) launch the shiny-based word intrusion test. The coder should find
  out the intruder word that is not related to other words.

- `$do_topic_intrusion_test()`:

  (tm) launch the shiny-based topic intrusion test. The coder should
  find out the intruder topic that is least likely to be the topic of
  the document.

- `$do_word_set_intrusion_test()`:

  (tm) launch the shiny-based word set intrusion test. The coder should
  find out the intruder word set that is not related to other word sets.

- `$do_gold_standard_test()`:

  (gs) launch the shiny-based test for generating gold standard. The
  coder should determine the level of the predetermined constructs with
  a 5-point Likert scale.

- `$lock(force = FALSE)`:

  (gs/tm) lock the object so that it cannot be changed anymore. It
  enables
  [`summarize_oolong`](https://gesistsa.github.io/oolong/reference/summarize_oolong.md)
  and the following method.

- `$turn_gold()`:

  (gs) convert the oolong object into a quanteda compatible corpus.

For more details, please see the overview vignette:
[`vignette("overview", package = "oolong")`](https://gesistsa.github.io/oolong/articles/overview.md)

## References

Chang, J., Gerrish, S., Wang, C., Boyd-Graber, J. L., & Blei, D. M.
(2009). Reading tea leaves: How humans interpret topic models. In
Advances in neural information processing systems (pp. 288-296).

Song et al. (2020) In validations we trust? The impact of imperfect
human annotations as a gold standard on the quality of validation of
automated content analysis. Political Communication.

Ying, L., Montgomery, J. M., & Stewart, B. M. (2021). Topics, Concepts,
and Measurement: A Crowdsourced Procedure for Validating Topics as
Measures. Political Analysis

## Author

Chung-hong Chan, Marius Sältzer

## Examples

``` r
## Creation of oolong test with only word intrusion test
data(abstracts_seededlda)
data(abstracts)
oolong_test <- wi(input_model = abstracts_seededlda, userid = "Hadley")
## Creation of oolong test with both word intrusion test and topic intrusion test
oolong_test <- witi(input_model = abstracts_seededlda,
input_corpus = abstracts$text, userid = "Julia")
## Creation of oolong test with topic intrusion test
oolong_test <- ti(input_model = abstracts_seededlda,
input_corpus = abstracts$text, userid = "Jenny")
## Creation of oolong test with word set intrusion test
oolong_test <- wsi(input_model = abstracts_seededlda, userid = "Garrett")
## Creation of gold standard
oolong_test <- gs(input_corpus = trump2k, userid = "Yihui")
## Using create_oolong(); not recommended
oolong_test <- create_oolong(input_model = abstracts_seededlda,
input_corpus = abstracts$text, userid = "JJ")
oolong_test <- create_oolong(input_model = abstracts_seededlda,
input_corpus = abstracts$text, userid = "Mara", type = "ti")
oolong_test <- create_oolong(input_corpus = abstracts$text, userid = "Winston", type = "gs")
```
