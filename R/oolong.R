### CODING STYLE
### function names: use full name, except ren for render
### data structures: use singular, except list-column.

.insert <- function(good_terms, intruder, position) {
    length_test_items <- length(c(good_terms, intruder))
    res <- rep(NA, length_test_items)
    res[position] <- intruder
    res[setdiff(1:length_test_items, position)] <- sample(good_terms)
    return(tibble::tibble(position = position, candidates = list(res)))
}


.generate_candidates <- function(i, terms, n_top_terms = 5, bottom_terms_percentile = 0.6, all_terms) {
    good_terms <- head(terms[i,], n_top_terms)
    term_pos <- match(all_terms, terms[i,])
    candidates <- tibble::tibble(all_terms, term_pos)
    non_na_candidates <- candidates[!is.na(candidates$term_pos),]
    intruder <- sample(non_na_candidates$all_terms[non_na_candidates$term_pos > quantile(non_na_candidates$term_pos, bottom_terms_percentile, na.rm = TRUE)], 1)
    position <- sample(1:(n_top_terms + 1), 1)
    .insert(good_terms, intruder, position) -> res
    res$t <- i
    res$intruder <- res$candidates[[1]][res$position]
    res$answer <- NA
    return(res)
}

.cal_oolong_correct <- function(oolong_test) {
    if (all(is.na(oolong_test$answer))) {
        return(0)
    } else {
        base <- sum(!is.na(oolong_test$answer))
        n_correct <- sum(oolong_test$answer[!is.na(oolong_test$answer)] == oolong_test$intruder[!is.na(oolong_test$answer)])
        return(round((n_correct / base) * 100, 2))
    }
}

.UI_WORD_INTRUSION_TEST <- miniUI::miniPage(
                                 miniUI::gadgetTitleBar("oolong"),
                                 miniUI::miniContentPanel(
                                             shiny::textOutput("current_topic"),
                                             shiny::uiOutput("intruder_choice"),
                                             shiny::actionButton("confirm", "confirm"),
                                             shiny::actionButton("nextq", "skip")
                                         )
                             )

.UI_TOPIC_INTRUSION_TEST <- miniUI::miniPage(
                                        miniUI::gadgetTitleBar("oolong"),
                                        miniUI::miniContentPanel(
                                                    shiny::textOutput("current_topic"),
                                                    shiny::uiOutput("text_content"),
                                                    shiny::uiOutput("intruder_choice"),
                                                    shiny::actionButton("confirm", "confirm"),
                                                    shiny::actionButton("nextq", "skip")
                                                )
                                    )

.ren_word_intrusion_test <- function(output, test_content, res) {
    .ren_choices <- function(test_content, res) {
        shiny::renderUI({
    radioButtons("intruder", label = "Which of the following is an intruder word?", choices = test_content$candidates[[res$current_row]], selected = res$intruder[res$current_row])
        })
    }
    .ren_topic_bar <- function(test_content, res) {
        shiny::renderText({
            paste("Topic ", res$current_row, "of", nrow(test_content), ifelse(is.na(res$intruder[res$current_row]), "", " [coded]"))
        })
    }
    output$intruder_choice <- .ren_choices(test_content, res)
    output$current_topic <- .ren_topic_bar(test_content, res)
    return(output)
}

.ren_topic_intrusion_test <- function(output, test_content, res) {
    .ren_choices <- function(test_content, res) {
        shiny::renderUI({
            radioButtons("intruder", label = "Which of the following is an intruder topic?", choiceNames = test_content$topic_labels[[res$current_row]], choiceValues = test_content$candidates[[res$current_row]], selected = res$intruder[res$current_row])
        })
    }
    .ren_topic_bar <- function(test_content, res) {
        shiny::renderText({
            paste("Case ", res$current_row, "of", nrow(test_content), ifelse(is.na(res$intruder[res$current_row]), "", " [coded]"))
        })
    }
    .ren_text_content <- function(test_content, res) {
        shiny::renderUI({
            shiny::pre(test_content$text[res$current_row])
        })
    }
    output$intruder_choice <- .ren_choices(test_content, res)
    output$current_topic <- .ren_topic_bar(test_content, res)
    output$text_content <- .ren_text_content(test_content, res)
    return(output)
}

### It must take a UI and render function. For new test_content type, please prepare a new pair of .UI_XXX_test (in cap) and .ren_xxx_test
.code_oolong <- function(test_content, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_intrusion_test) {
    server <- function(input, output, session) {
        res <- shiny::reactiveValues(intruder = test_content$answer, current_row = 1)
        output <- .ren(output, test_content, res)
        shiny::observeEvent(input$confirm, {
            res$intruder[res$current_row] <- input$intruder
            res$current_row <- res$current_row + 1
            if (res$current_row > nrow(test_content)) {
                res$current_row <- 1
            }
            output <- .ren(output, test_content, res)
        })
        shiny::observeEvent(input$nextq, {
            res$current_row <- res$current_row + 1
            if (res$current_row > nrow(test_content)) {
                res$current_row <- 1
            }
            output <- .ren(output, test_content, res)
        })
        shiny::observeEvent(input$done, (
            shiny::stopApp(res$intruder)
        ))

    }
    shiny::runGadget(ui, server)
}


.generate_word_intrusion_test <- function(model, n_top_terms = 5, bottom_terms_percentile = 0.6, difficulty = 0.8) {
    if ("WarpLDA" %in% class(model)) {
        K <- model$.__enclos_env__$private$n_topics
        V <- length(model$.__enclos_env__$private$vocabulary)
        terms <- t(model$get_top_words(n = V, lambda = difficulty))
        all_terms <- unique(as.vector(terms[,seq_len(n_top_terms)]))
    } else if ("STM" %in% class(model)) {
        K <- model$settings$dim$K
        V <- model$settings$dim$V
        terms <- stm::labelTopics(model, n = model$settings$dim$V, frexweight = difficulty)$frex
        all_terms <- unique(as.vector(terms[,seq_len(n_top_terms)]))
    }
    test_content <- purrr::map_dfr(seq_len(K), .generate_candidates, terms = terms, all_terms = all_terms, bottom_terms_percentile = bottom_terms_percentile)
    ###res <- list()
    ##res$oolong_test <- oolong_test
    ##res$hash <- digest::digest(oolong_test, "sha512")
    ##class(res) <- c(class(res), "oolong_test")
    return(test_content)
}

.do_oolong_test <- function(test_content) {
    test_content$answer <- .code_oolong(test_content)
    return(test_content)
}

Oolong_test <- R6::R6Class(
    "oolong_test",
    public = list(
        initialize = function(model, n_top_terms = 5, bottom_terms_percentile = 0.6, difficulty = 0.8) {
            private$test_content <- .generate_word_intrusion_test(model, n_top_terms = n_top_terms, bottom_terms_percentile = bottom_terms_percentile, difficulty = difficulty)
            private$hash <- digest::digest(private$test_content, algo = "sha1")
        },
        print = function() {
            cat(paste0("An oolong test object with k = ", nrow(private$test_content), ", ", sum(!is.na(private$test_content$answer)), " coded. (", round(.cal_oolong_correct(private$test_content), 3),"%  precision)\n Use the method $do_word_intrusion_test() to start word intrusion test.\n"))
        },
        do_word_intrusion_test = function() {
            private$test_content <- .do_oolong_test(private$test_content)
        }
    ),
    private = list(
        hash = NULL,
        test_content = NULL
    )
)

#' Generate a oolong test for a topic model
#'
#' Currently, this function generates a oolong test object that contains a word intrusion test. Future version will provide additional tests such as topic intrusion test.
#'
#' @param model a STM or WrapLDA object
#' @param n_top_terms integer, number of top topic words to be included in the candidates
#' @param bottm_terms_percentile double, a term is considered to be an intruder when its theta less than the percentile of this theta, must be within the range of 0 to 1.
#' @param difficulty double, to adjust the difficulty of the test. Higher value indicates higher difficulty, must be within the range of 0 to 1.
#' @export
create_oolong <- function(model, n_top_terms = 5, bottom_terms_percentile = 0.6, difficulty = 0.8) {
    return(Oolong_test$new(model, n_top_terms, bottom_terms_percentile, difficulty))
}


#' Newsgroup text dataset
#'
#' This is a subset of the famous "newsgroup 20" dataset.
"newsgroup5"


.sample_corpus <- function(corpus, exact_n = 30, frac = NULL) {
    if (!is.null(frac)) {
        stopifnot(frac >= 0 & frac <= 1)
        exact_n <- floor(length(corpus) * frac)
    }
    sample(seq_len(length(corpus)), exact_n)
}

.get_intruder_by_position <- function(candidates, position) {
    candidates[position]
}

.generate_topic_frame <- function(i, target_text, target_theta, model_terms, k = k, n_top_topics = 3, n_top_words = 8) {
    text <- target_text[i]
    theta_rank <- rank(target_theta[i,])
    theta_pos <- which(theta_rank > (k - n_top_topics))
    intruder_pos <- sample(setdiff(seq_len(k), theta_pos), 1)
    position <- sample(seq_len(n_top_topics + 1), 1)
    topic_frame <- .insert(theta_pos, intruder_pos, position)
    topic_frame$text <- text
    topic_frame$topic_labels <- list(apply(model_terms[topic_frame$candidates[[1]],], 1, paste0, collapse = ", "))
    topic_frame$thetas <- list(target_theta[i, topic_frame$candidates[[1]]])
    topic_frame$answer <- NA
    topic_frame$intruder <- purrr::map2_int(topic_frame$candidates, topic_frame$position, .get_intruder_by_position)
    return(topic_frame)
}

.generate_topic_intrusion_test <- function(model, corpus, exact_n = 30, frac = NULL, n_top_topics = 3, n_top_words = 8, difficulty = 0.8) {
    sample_vec <- .sample_corpus(corpus, exact_n)
    model_terms <- stm::labelTopics(model, n = n_top_words, frexweight = difficulty)$frex
    target_theta <- model$theta[sample_vec, ]
    k <- ncol(target_theta)
    target_text <- corpus[sample_vec]
    test_content <- purrr::map_dfr(seq_len(exact_n), .generate_topic_frame, target_text = target_text, target_theta = target_theta, model_terms = model_terms, k = k, n_top_topics = n_top_topics, n_top_words = n_top_words)
    return(test_content)
}
