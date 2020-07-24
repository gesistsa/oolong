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

.cal_model_precision <- function(oolong_test) {
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
        shiny::uiOutput("current_topic"),
        shiny::uiOutput("intruder_choice"),
        shiny::actionButton("confirm", "confirm"),
        shiny::actionButton("nextq", "skip"),
        shiny::actionButton("ff", "jump to uncoded item")
    )
)

.UI_TOPIC_INTRUSION_TEST <- miniUI::miniPage(
    miniUI::gadgetTitleBar("oolong"),
    miniUI::miniContentPanel(
        shiny::uiOutput("current_topic"),
        shiny::uiOutput("text_content"),
        shiny::uiOutput("intruder_choice"),
        shiny::actionButton("confirm", "confirm"),
        shiny::actionButton("nextq", "skip"),
        shiny::actionButton("ff", "jump to uncoded item")
    )
)

.ren_word_intrusion_test <- function(output, test_content, res) {
    .ren_choices <- function(test_content, res) {
        shiny::renderUI({
            shiny::radioButtons("intruder", label = "Which of the following is an intruder word?", choices = test_content$candidates[[res$current_row]], selected = res$intruder[res$current_row])
        })
    }
    .ren_topic_bar <- function(test_content, res) {
        shiny::renderUI({
            shiny::strong(
                paste("Topic ", res$current_row, "of", nrow(test_content), ifelse(is.na(res$intruder[res$current_row]), "", " [coded]")))
        })
    }
    output$intruder_choice <- .ren_choices(test_content, res)
    output$current_topic <- .ren_topic_bar(test_content, res)
    return(output)
}

.ren_topic_intrusion_test <- function(output, test_content, res) {
    .ren_choices <- function(test_content, res) {
        shiny::renderUI({
            shiny::radioButtons("intruder", label = "Which of the following is an intruder topic?", choiceNames = test_content$topic_labels[[res$current_row]], choiceValues = test_content$candidates[[res$current_row]], selected = res$intruder[res$current_row])
        })
    }
    .ren_topic_bar <- function(test_content, res) {
        shiny::renderUI({
            shiny::strong(
                paste("Case ", res$current_row, "of", nrow(test_content), ifelse(is.na(res$intruder[res$current_row]), "", " [coded]")))
        })
    }
    .ren_text_content <- function(test_content, res) {
        shiny::renderUI({
            shiny::tagList(
                shiny::hr(),
                shiny::p(test_content$text[res$current_row]),
                shiny::hr()
            )
        })
    }
    output$intruder_choice <- .ren_choices(test_content, res)
    output$current_topic <- .ren_topic_bar(test_content, res)
    output$text_content <- .ren_text_content(test_content, res)
    return(output)
}

### It must take a UI and render function. For new test_content type, please prepare a new pair of .UI_XXX_test (in cap) and .ren_xxx_test

.gen_shinyapp <- function(test_content, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_intrusion_test) {
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
        shiny::observeEvent(input$ff, {
            res_with_na <- which(is.na(res$intruder))
            if (length(res_with_na) == 0) {
                res$current_row <- res$current_row
            } else {
                res$current_row <- min(res_with_na)
            }
            output <- .ren(output, test_content, res)
        })
        shiny::observeEvent(input$done, (
            shiny::stopApp(res$intruder)
        ))
        
    }
    return(shiny::shinyApp(ui, server))
}

.code_oolong <- function(test_content, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_intrusion_test) {
    shiny::runGadget(.gen_shinyapp(test_content = test_content, ui = ui, .ren = .ren))
}

### S3 generic

.extract_ingredients <- function(input_model_s3, ...) {
    UseMethod(".extract_ingredients", input_model_s3)
}

## every format should create a s3 method '.extract_ingredients' (e.g. .extract_ingredients.input_model_s3_warplda) to extract K (# of topics), V (# of terms), terms (a term matrix, K x V) and all_terms, model_terms and theta.
## refer to oolong_stm.R for an example

.generate_word_intrusion_test <- function(ingredients, bottom_terms_percentile = 0.6, n_top_terms) {
    test_content <- purrr::map_dfr(seq_len(ingredients$K), .generate_candidates, terms = ingredients$terms, all_terms = ingredients$all_terms, bottom_terms_percentile = bottom_terms_percentile, n_top_terms = n_top_terms)
    return(test_content)
}

.sample_corpus <- function(input_corpus, exact_n = 30) {
    sample(seq_len(length(input_corpus)), exact_n)
}

.get_intruder_by_position <- function(candidates, position) {
    candidates[position]
}

.generate_topic_frame <- function(i, target_text, target_theta, model_terms, k = k, n_top_topics = 3) {
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

.generate_topic_intrusion_test <- function(input_corpus, ingredients, exact_n = NULL, frac = NULL, n_top_topics = 3, n_topiclabel_words = 8) {
    if ("corpus" %in% class(input_corpus)) {
        input_corpus <- quanteda::texts(input_corpus)
    }
    if (n_top_topics + 1 >= ingredients$K) {
        stop("n_top_topics + 1 must be smaller than K.")
    }
    if (!is.null(frac) & is.null(exact_n)) {
        stopifnot(frac >= 0 & frac <= 1)
        exact_n <- floor(length(input_corpus) * frac)
    }
    if (exact_n <=1 ) {
        stop("exact_n or frac are too small for your sample size. Please increase either the exact_n or frac.")
    }
    if (exact_n > length(input_corpus)) {
        warning("exact_n is larger than the size of input_corpus. Switch to frac = 1")
        exact_n <- length(input_corpus)
    }
    sample_vec <- .sample_corpus(input_corpus, exact_n)
    target_theta <- ingredients$theta[sample_vec,]
    k <- ncol(target_theta)
    target_text <- input_corpus[sample_vec]
    test_content <- purrr::map_dfr(seq_len(exact_n), .generate_topic_frame, target_text = target_text, target_theta = target_theta, model_terms = ingredients$model_terms, k = k, n_top_topics = n_top_topics)
    return(test_content)
}

.do_oolong_test <- function(test_content, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_intrusion_test) {
    test_content$answer <- .code_oolong(test_content, ui = ui, .ren = .ren)
    return(test_content)
}

.generate_test_content <- function(input_model, input_corpus = NULL, n_top_terms = 5, bottom_terms_percentile = 0.6, exact_n = NULL, frac = 0.01, n_top_topics = 3, n_topiclabel_words = 8, difficulty = 1, use_frex_words = FALSE, input_dfm = NULL, btm_dataframe = NULL) {
    ingredients <- .extract_ingredients(.convert_input_model_s3(input_model), n_top_terms = n_top_terms, difficulty = difficulty, need_topic = !is.null(input_corpus), n_topiclabel_words = n_topiclabel_words, input_dfm = input_dfm, use_frex_words = use_frex_words, input_corpus = input_corpus, btm_dataframe = btm_dataframe)
    test_content <- list()
    test_content$word <- .generate_word_intrusion_test(ingredients, bottom_terms_percentile = bottom_terms_percentile, n_top_terms = n_top_terms)
    if (is.null(ingredients$theta)) {
        test_content$topic <- NULL
    } else {
        test_content$topic <- .generate_topic_intrusion_test(input_corpus = input_corpus, ingredients = ingredients, exact_n = exact_n, frac = frac, n_top_topics = n_top_topics, n_topiclabel_words = n_topiclabel_words)
    }
    return(test_content)
}

.check_test_content_complete <- function(test_content) {
    all(purrr::map_lgl(test_content, ~all(!is.na(.$answer))))
}

.cal_lr_diff <- function(i, test_content) {
    intruder_idx <- which(test_content$candidates[[i]] == test_content$intruder[i])
    answer_idx <- which(test_content$candidates[[i]] == test_content$answer[i])
    log(test_content$thetas[[i]][intruder_idx]) - log(test_content$thetas[[i]][answer_idx])
}

.cal_tlo <- function(test_content, mean_value = TRUE) {
    res <- purrr::map_dbl(seq_len(nrow(test_content[!is.na(test_content$answer),])), .cal_lr_diff, test_content = test_content[!is.na(test_content$answer),])
    if (mean_value) {
        return(mean(res))
    }
    return(res)
}



Oolong_test_tm <-
    R6::R6Class(
        "oolong_test_tm",
        inherit = Oolong_test,
        public = list(
            initialize = function(input_model, input_corpus = NULL, n_top_terms = 5, bottom_terms_percentile = 0.6, exact_n = 15, frac = NULL, n_top_topics = 3, n_topiclabel_words = 8, difficulty = 1, use_frex_words = FALSE, input_dfm = NULL, btm_dataframe = NULL) {
                private$test_content <- .generate_test_content(input_model, input_corpus, n_top_terms, bottom_terms_percentile, exact_n, frac, n_top_topics, n_topiclabel_words, difficulty, use_frex_words = use_frex_words, input_dfm = input_dfm, btm_dataframe = btm_dataframe)
                private$hash <- digest::digest(private$test_content, algo = "sha1")
            },
            print = function() {
                .cp(TRUE, "An oolong test object with k = ", nrow(private$test_content$word), ", ", sum(!is.na(private$test_content$word$answer)), " coded.")
                .cp(private$finalized, round(.cal_model_precision(private$test_content$word), 3),"%  precision")
                .cp(!private$finalized, "Use the method $do_word_intrusion_test() to do word intrusion test.")
                .cp(!is.null(private$test_content$topic), "With ", nrow(private$test_content$topic) , " cases of topic intrusion test. ", sum(!is.na(private$test_content$topic$answer)), " coded.")
                .cp(!is.null(private$test_content$topic) & !private$finalized, "Use the method $do_topic_intrusion_test() to do topic intrusion test.")
                .cp(private$finalized & !is.null(private$test_content$topic), "TLO: ", round(.cal_tlo(private$test_content$topic, mean_value = TRUE), 3))
                .cp(!private$finalized, "Use the method $lock() to finalize this object and see the results.")
            },
            do_word_intrusion_test = function() {
                private$check_finalized()
                private$test_content$word <- .do_oolong_test(private$test_content$word)
            },
            do_topic_intrusion_test = function() {
                private$check_finalized()
                .cstop(is.null(private$test_content$topic), "No topic intrusion test cases. Create the oolong test with the corpus to generate topic intrusion test cases.")
                private$test_content$topic <- .do_oolong_test(private$test_content$topic, ui = .UI_TOPIC_INTRUSION_TEST, .ren = .ren_topic_intrusion_test)
            }
        ),
        private = list(
            hash = NULL,
            test_content = list(),
            finalized = FALSE
        )
        )


