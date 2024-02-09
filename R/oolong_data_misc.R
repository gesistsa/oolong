
#' Abstracts of communication journals dataset
#'
#' This is a random sample of all abstracts of papers published in high-impact communication journals from 2000 to 2017. abstracts_dictionary is a list of terms that can be used for semisupervised techniques such as keyATM.
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
"abstracts_seededlda"

#' @rdname abstracts_seededlda
"abstracts_btm"

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

#' Naive Bayes model trained on 20 newsgroups data
#'
#' This is a Naive Bayes model (of the class 'textmodel_nb') trained on 20 newsgroups data.
#' @references
#'   Lang, K. (1995). Newsweeder: Learning to filter netnews. In Machine Learning Proceedings 1995 (pp. 331-339). Morgan Kaufmann.
"newsgroup_nb"

#' @importFrom stats cooks.distance cor.test lm median pchisq quantile sd predict
#' @importFrom utils head
#' @importFrom quanteda print corpus
NULL

utils::globalVariables(c('cookd', 'diffxy', 'index', 'meanxy', 'word_length', 'avg_answer', 'abstracts_seededlda', 'abstracts'))

### print the ... if boolean_test is true
.cp <- function(boolean_test, ...) {
    if (boolean_test) {
        cli::cli_alert_info(paste0(...))
    }
}

### stop if boolean_test is true and print the ...
.cstop <- function(boolean_test, ...) {
    if (boolean_test) {
        stop(..., call. = FALSE)
    }
}

### to get rid of the stupid behaviour of sample() when length of x is 1.
.safe_sample <- function(x, size) {
    if (length(x) == 1) {
        return(x)
    } else {
        return(sample(x, size))
    }
}

### generate_meta, pirated quanteda::meta_system_defaults
.generate_meta <- function() {
    list("package-version" = utils::packageVersion("oolong"),
         "r-version" = getRversion(),
         "system" = Sys.info()[c("sysname", "machine", "user")],
         "directory" = getwd(),
         "created" = Sys.Date()
         )
}

.sym_flip <- function(bool) {
    ifelse(bool, cli::symbol$tick, cli::symbol$cross)
}

.safe_hash <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }
    return(digest::digest(x, algo = "sha1"))
}
