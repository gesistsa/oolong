
#' Abstracts of communication journals dataset
#'
#' This is a random sample of all abstracts of papers published in high-impact communication journals from 2000 to 2017. The one ends with "dfm" is same data in quanteda::dfm (document-feature matrix) format.
"abstracts"

#' @rdname abstracts
"abstracts_dfm"

#' Topic models trained with the abstracts dataset.
#'
#' These are topic models trained with different topic model packages.
"abstracts_stm"

#' @rdname abstracts_stm
"abstracts_warplda"

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


#' @importFrom stats cooks.distance cor.test lm median pchisq quantile sd
#' @importFrom utils head
#' @importFrom quanteda print corpus
NULL

utils::globalVariables(c('cookd', 'diffxy', 'index', 'meanxy', 'word_length', 'avg_answer'))
