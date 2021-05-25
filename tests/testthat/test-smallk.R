## context("Edge cases of topic models with a very small k")

smallk <- readRDS("../testdata/abstracts_stm_small.RDS")

test_that("small k stm", {
    ## smallk was trained with k = 4
    expect_error(create_oolong(input_model = smallk, input_corpus = corpus(abstracts$text), n_top_topics = 4))
    ## It should accept n_top_topics = 3
    expect_error(create_oolong(input_model = smallk, input_corpus = corpus(abstracts$text), n_top_topics = 3), NA)
    ## It should accept n_top_topics = 2
    expect_error(create_oolong(input_model = smallk, input_corpus = corpus(abstracts$text), n_top_topics = 2), NA)
    ## how about 1? It should fail
    expect_error(create_oolong(input_model = smallk, input_corpus = corpus(abstracts$text), n_top_topics = 1))
})

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$wi$answer <- obj1$.__enclos_env__$private$test_content$wi$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$ti$answer <- obj1$.__enclos_env__$private$test_content$ti$intruder
    return(obj1)
}


test_that("check_calculation_topic_intrusion_multiobject", {
    obj1 <- create_oolong(smallk, abstracts$text, exact_n = 10)
    obj2 <- clone_oolong(obj1)
    obj1 <- genius_word(obj1)
    obj1 <- genius_topic(obj1)
    obj1$lock()
    obj2 <- genius_word(obj2)
    obj2 <- genius_topic(obj2)
    obj2$lock()
    res <- summarize_oolong(obj1, obj2)
    expect_length(res$tlo_p_value, 1)
    expect_length(res$tlo, 20)
})

smallklda <- readRDS("../testdata/abstracts_topicmodels_small.RDS")

test_that("small k topicmodels", {
    ## smallk was trained with k = 3
    expect_error(create_oolong(input_model = smallklda, input_corpus = corpus(abstracts$text), n_top_topics = 3))
    ## It should accept n_top_topics = 2
    expect_error(create_oolong(input_model = smallklda, input_corpus = corpus(abstracts$text), n_top_topics = 2), NA)
    expect_error(create_oolong(input_model = smallklda, input_corpus = corpus(abstracts$text), n_top_topics = 1))
})


test_that("dealing with ties", {
    expect_error(create_oolong(input_model = readRDS("../testdata/lda_ties.RDS"), input_corpus = rep("", 5270), n_top_topics = 2), NA)
})

test_that("low k lda query", {
    library(quanteda)
    library(topicmodels)
    dfm1 <- readRDS("../testdata/low_k_dfm1.RDS")
    dtm1 <- quanteda::convert(dfm1, to = "topicmodels")
    set.seed(122)
    lda1.2 <- LDA(dtm1, method = "Gibbs", k = 3, control = list(alpha = 0.1))
    expect_error(create_oolong(input_model = lda1.2, input_corpus = rep("", 5718)[ntoken(dfm1) > 0], n_top_topics = 2), NA)
})
