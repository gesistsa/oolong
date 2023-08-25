## context("Support for topicmodels")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$wi$answer <- obj1$.__enclos_env__$private$test_content$wi$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$ti$answer <- obj1$.__enclos_env__$private$test_content$ti$intruder
    return(obj1)
}

library(quanteda)
library(BTM)
abstracts_corpus <- corpus(abstracts$text)
tokens(abstracts_corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, split_hyphens = TRUE) %>%  tokens_tolower() %>% tokens_remove(stopwords("en")) %>% tokens_wordstem() -> toks_q
as.data.frame.tokens <- function(x) {
    data.frame(
        doc_id = rep(names(x), lengths(x)),
        tokens = unlist(x, use.names = FALSE)
    )
}
abstracts_df <- as.data.frame.tokens(toks_q)

test_that("wi basic", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    expect_error(wi(abstracts_stm), NA)
    expect_error(wi(abstracts_warplda), NA)
    expect_error(wi(abstracts_btm), NA)
    expect_error(wi(abstracts_keyatm), NA)
})

test_that("witi basic", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    expect_error(witi(abstracts_stm, abstracts$text), NA)
    ## We need to talk about how to handle these two.
    expect_error(witi(abstracts_stm), NA)
    expect_error(witi(abstracts_warplda), NA)
    expect_error(witi(abstracts_warplda, abstracts$text))
    expect_error(witi(abstracts_warplda, abstracts$text, input_dfm = abstracts_dfm), NA)
    expect_error(witi(abstracts_btm, abstracts$text))
    expect_error(witi(abstracts_btm, abstracts_corpus))
    expect_error(witi(abstracts_btm, abstracts$text, btm_dataframe = abstracts_df))
    expect_error(witi(abstracts_btm, abstracts_corpus, btm_dataframe = abstracts_df), NA)
    expect_error(witi(abstracts_keyatm, abstracts$text), NA)
})

test_that("ti basic", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    expect_error(ti(abstracts_stm, abstracts$text), NA)
    expect_error(ti(abstracts_warplda, abstracts$text))
    expect_error(ti(abstracts_warplda, abstracts$text, input_dfm = abstracts_dfm), NA)
    expect_error(ti(abstracts_btm, abstracts$text))
    expect_error(ti(abstracts_btm, abstracts_corpus))
    expect_error(ti(abstracts_btm, abstracts$text, btm_dataframe = abstracts_df))
    expect_error(ti(abstracts_btm, abstracts_corpus, btm_dataframe = abstracts_df), NA)
    expect_error(ti(abstracts_keyatm, abstracts$text), NA)
})

test_that("wsi basic", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    expect_error(wsi(abstracts_stm), NA)
    expect_error(wsi(abstracts_warplda), NA)
    expect_error(wsi(abstracts_warplda), NA)
    expect_error(wsi(abstracts_btm), NA)
    expect_error(wsi(abstracts_keyatm), NA)
})


test_that("gs basic", {
    expect_error(gs(abstracts$text), NA)
})

test_that("correct passing of n_top_terms",{
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    for (i in sample(2:10, 5)) {
        z <- witi(abstracts_stm, abstracts$text, n_top_terms = i)
        expect_equal(length(z$.__enclos_env__$private$test_content$wi$candidates[[1]]), i + 1)
        z <- wi(abstracts_stm, n_top_terms = i)
        expect_equal(length(z$.__enclos_env__$private$test_content$wi$candidates[[1]]), i + 1)
    }
})

test_that("correct passing of n_top_topics", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    for (i in sample(2:10, 3)) {
        z <- witi(abstracts_stm, abstracts$text, n_top_topics = i)
        expect_equal(length(z$.__enclos_env__$private$test_content$ti$candidates[[1]]), i + 1)
        z <- ti(abstracts_stm, abstracts$text, n_top_topics = i)
        expect_equal(length(z$.__enclos_env__$private$test_content$ti$candidates[[1]]), i + 1)
    }
})

test_that("correct passing of n_topiclabel_words", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    for (i in sample(2:10, 3)) {
        z <- witi(abstracts_stm, abstracts$text, n_topiclabel_words = i)
        topic_label <- z$.__enclos_env__$private$test_content$ti$topic_labels[[1]][1]
        expect_equal(length(strsplit(topic_label, ", ")[[1]]), i)
        z <- ti(abstracts_stm, abstracts$text, n_topiclabel_words = i)
        topic_label <- z$.__enclos_env__$private$test_content$ti$topic_labels[[1]][1]
        expect_equal(length(strsplit(topic_label, ", ")[[1]]), i)
    }
})

test_that("correct passing of n_topiclabel_words (wsi)", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    for (g in sample(2:10, 3)) {
        az <- wsi(abstracts_stm, n_topiclabel_words = g, wsi_n_top_terms = 50)
        ws_topic_label <- az$.__enclos_env__$private$test_content$wsi$candidates[[1]][1]
        expect_equal(length(strsplit(ws_topic_label, ", ")[[1]]), g)
    }
})

test_that("correct passing of construct", {
    skip_on_cran()
    for (i in sample(c("Ich", "bin", "eine", "Katze"))) {
        z <- gs(abstracts$text, construct = i)
        expect_equal(z$.__enclos_env__$private$construct, i)
    }
})


test_that("correct passing of exact_n", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    for (i in sample(5:90, size = 3)) {
        z <- witi(abstracts_stm, abstracts$text, exact_n = i)
        expect_equal(nrow(z$.__enclos_env__$private$test_content$ti), i)
        z <- ti(abstracts_stm, abstracts$text, exact_n = i)
        expect_equal(nrow(z$.__enclos_env__$private$test_content$ti), i)
        z <- gs(abstracts$text, exact_n = i)
        expect_equal(nrow(z$.__enclos_env__$private$test_content$gs), i)
    }
})

test_that("correct passing of frac", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    for (i in sample(seq(0.01, 0.8, by = 0.02), size = 3)) {
        expect_ans <- floor(length(abstracts$text) * i)
        z <- witi(abstracts_stm, abstracts$text, frac = i)
        expect_equal(nrow(z$.__enclos_env__$private$test_content$ti), expect_ans)
        z <- ti(abstracts_stm, abstracts$text, frac = i)
        expect_equal(nrow(z$.__enclos_env__$private$test_content$ti), expect_ans)
        z <- gs(abstracts$text, frac = i)
        expect_equal(nrow(z$.__enclos_env__$private$test_content$gs), expect_ans)
    }
})

test_that("correct passing of userid", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    for (i in sample(c("Ich", "bin", "eine", "Katze"), 3)) {
        z <- wi(abstracts_stm, userid = i)
        expect_equal(z$userid, i)
        z <- witi(abstracts_stm, abstracts$text, userid = i)
        expect_equal(z$userid, i)
        z <- ti(abstracts_stm, abstracts$text, userid = i)
        expect_equal(z$userid, i)
        z <- gs(abstracts$text, userid = i)
        expect_equal(z$userid, i)
        z <- wsi(abstracts_stm, userid = i)
        expect_equal(z$userid, i)
    }
})

test_that("correct passing of n_correct_ws", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    for (i in sample(2:5, 3)) {
        z <- wsi(abstracts_stm, n_correct_ws = i, wsi_n_top_terms = 50)
        expect_equal(length(z$.__enclos_env__$private$test_content$wsi$candidates[[1]]), i + 1)
    }
})

test_that("correct passing of frexweight and use_frex_words", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    set.seed(123)
    z <- wi(abstracts_stm) ## prob words
    set.seed(123)
    z2 <- wi(abstracts_stm, frexweight = 0.8) ## no effect, use_frexwords is FALSE
    expect_true(identical(z$.__enclos_env__$private$test_content$wi$candidates,
                          z2$.__enclos_env__$private$test_content$wi$candidates))
    set.seed(123)
    z3 <- wi(abstracts_stm, frexweight = 0.8, use_frex_words = TRUE)
    expect_false(identical(z2$.__enclos_env__$private$test_content$wi$candidates,
                           z3$.__enclos_env__$private$test_content$wi$candidates))
    set.seed(123)
    z4 <- wi(abstracts_stm, frexweight = 0.5, use_frex_words = TRUE)
    expect_false(identical(z3$.__enclos_env__$private$test_content$wi$candidates,
                           z4$.__enclos_env__$private$test_content$wi$candidates))
})

test_that("legacy `difficulty` #74", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    set.seed(123)
    z <- wi(abstracts_stm) ## prob words
    set.seed(123)
    z2 <- wi(abstracts_stm, difficulty = 0.5) ## no effect, use_frexwords is FALSE
    expect_true(identical(z$.__enclos_env__$private$test_content$wi$candidates,
                          z2$.__enclos_env__$private$test_content$wi$candidates))
    set.seed(123)
    z3 <- wi(abstracts_stm, difficulty = 0.8, use_frex_words = TRUE)
    expect_false(identical(z2$.__enclos_env__$private$test_content$wi$candidates,
                           z3$.__enclos_env__$private$test_content$wi$candidates))
    ##equvalent
    set.seed(123)
    z4 <- wi(abstracts_stm, frexweight = 0.8, use_frex_words = TRUE)
    expect_true(identical(z3$.__enclos_env__$private$test_content$wi$candidates,
                          z4$.__enclos_env__$private$test_content$wi$candidates))
})

test_that("correct passing of lambda", {
    skip_on_cran()
    skip_if_not(exists("abstracts_warplda"))
    set.seed(123)
    z <- wi(abstracts_warplda) ## lambda = 1
    set.seed(123)
    z2 <- wi(abstracts_warplda, lambda = 1) ## lambda = 1
    expect_true(identical(z$.__enclos_env__$private$test_content$wi$candidates,
                          z2$.__enclos_env__$private$test_content$wi$candidates))
    set.seed(123)
    z3 <- wi(abstracts_warplda, lambda = .5)
    expect_false(identical(z$.__enclos_env__$private$test_content$wi$candidates,
                           z3$.__enclos_env__$private$test_content$wi$candidates))
})

test_that("legacy `difficulty` #74", {
    skip_on_cran()
    skip_if_not(exists("abstracts_stm"))
    skip_if_not(exists("abstracts_warplda"))
    set.seed(123)
    z <- wi(abstracts_stm) ## prob words
    set.seed(123)
    z2 <- wi(abstracts_stm, difficulty = 0.5) ## no effect, use_frexwords is FALSE
    expect_true(identical(z$.__enclos_env__$private$test_content$wi$candidates,
                          z2$.__enclos_env__$private$test_content$wi$candidates))
    set.seed(123)
    z3 <- wi(abstracts_stm, difficulty = 0.8, use_frex_words = TRUE)
    expect_false(identical(z2$.__enclos_env__$private$test_content$wi$candidates,
                           z3$.__enclos_env__$private$test_content$wi$candidates))
    ##equivalent
    set.seed(123)
    z4 <- wi(abstracts_stm, frexweight = 0.8, use_frex_words = TRUE)
    expect_true(identical(z3$.__enclos_env__$private$test_content$wi$candidates,
                          z4$.__enclos_env__$private$test_content$wi$candidates))
    set.seed(123)
    z <- wi(abstracts_warplda) ## lambda = 1
    set.seed(123)
    z2 <- wi(abstracts_warplda, difficulty = 1) ## lambda = 1
    set.seed(123)
    z3 <- wi(abstracts_warplda, lambda = 1) ## lambda = 1

    expect_true(identical(z$.__enclos_env__$private$test_content$wi$candidates,
                          z2$.__enclos_env__$private$test_content$wi$candidates))
    expect_true(identical(z2$.__enclos_env__$private$test_content$wi$candidates,
                          z3$.__enclos_env__$private$test_content$wi$candidates))
    set.seed(123)
    z4 <- wi(abstracts_warplda, lambda = 0.1)
    expect_false(identical(z3$.__enclos_env__$private$test_content$wi$candidates,
                           z4$.__enclos_env__$private$test_content$wi$candidates))
})
