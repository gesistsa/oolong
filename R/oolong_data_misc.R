
#' Abstracts of communication journals dataset
#'
#' This is a random sample of all abstracts of papers published in high-impact communication journals from 2000 to 2017. The one ends with "dfm" is the same data in quanteda::dfm (document-feature matrix) format. abstracts_dictionary is a list of terms that can be used for semisupervised techniques such as keyATM.
#' @references
#'   Chan, C-h, & Grill, C. (2020). [The Highs in Communication Research: Research Topics With High Supply, High Popularity, and High Prestige in High-Impact Journals.](https://doi.org/10.1177/0093650220944790) Communication Research.
"abstracts"

#' @rdname abstracts
"abstracts_dfm"

#' @rdname abstracts
"abstracts_dictionary"

#' Topic models trained with the abstracts dataset.
#'
#' These are topic models trained with different topic model packages.
"abstracts_stm"

#' @rdname abstracts_stm
"abstracts_warplda"

#' @rdname abstracts_stm
"abstracts_btm"

#' @rdname abstracts_stm
"abstracts_keyatm"


#' AFINN dictionary
#'
#' This is the AFINN sentiment dictionary in quanteda::dictionary format.
#' @references
#'   Nielsen, F. Å. (2011). A new ANEW: Evaluation of a word list for sentiment analysis in microblogs. arXiv preprint arXiv:1103.2903.
"afinn"

#' Trump's tweets dataset
#'
#' This is a random sample of 2000 tweets from @realdonaldtrump account before his assumption of duty as the president of the United States.
"trump2k"

#' @importFrom stats cooks.distance cor.test lm median pchisq quantile sd predict
#' @importFrom utils head
#' @importFrom quanteda print corpus
NULL

utils::globalVariables(c('cookd', 'diffxy', 'index', 'meanxy', 'word_length', 'avg_answer'))

### print the ... if boolean_test is true
.cp <- function(boolean_test, ...) {
    if (boolean_test) {
        cat(paste0(..., "\n"))
    }
}

### stop if boolean_test is true and print the ...
.cstop <- function(boolean_test, ...) {
    if (boolean_test) {
        stop(...)
    }
}
