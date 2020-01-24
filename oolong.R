require(stm)
require(purrr)
require(tibble)
require(shiny)
require(miniUI)
require(text2vec)
require(digest)


.insert <- function(good_terms, intruder, position) {
    length_test_items <- length(c(good_terms, intruder))
    res <- rep(NA, length_test_items)
    res[position] <- intruder
    res[setdiff(1:length_test_items, position)] <- sample(good_terms)
    return(tibble(position = position, candidates = list(res)))
}


.gen_candidates <- function(i, terms, n_top_terms = 5, bottom_terms_percentile = 0.6, all_terms) {
    good_terms <- head(terms[i,], n_top_terms)
    term_pos <- match(all_terms, terms[i,])
    candidates <- tibble(all_terms, term_pos)
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
        return(n_correct / base)
    }
}


.code_oolong <- function(oolong_res) {
    ui <- miniPage(
        gadgetTitleBar("Oolong"),
        miniContentPanel(
            textOutput("current_topic"),
            uiOutput("intruder_choice"),
            actionButton("confirm", "confirm"),
            actionButton("nextq", "next")
        )
    )
    .ren_choices <- function(oolong, res) {
        renderUI({
    radioButtons("intruder", label = "Which of the following is an intruder word?", choices = oolong$candidates[[res$current_row]], selected = res$intruders[res$current_row])
        })
    }
    .ren_topic_bar <- function(oolong, res) {
        renderText({
            paste("Topic ", res$current_row, "of", nrow(oolong), ifelse(is.na(res$intruders[res$current_row]), "", " [coded]"))
        })
    }
    .ren <- function(output, oolong, res) {
        output$intruder_choice <- .ren_choices(oolong, res)
        output$current_topic <- .ren_topic_bar(oolong, res)
        return(output)
    }
    server <- function(input, output, session) {
        res <- reactiveValues(intruders = oolong_res$answer, current_row = 1)
        output <- .ren(output, oolong_res, res)
        observeEvent(input$confirm, {
            res$intruders[res$current_row] <- input$intruder
            res$current_row <- res$current_row + 1
            if (res$current_row > nrow(oolong_res)) {
                res$current_row <- 1
            }
            output <- .ren(output, oolong_res, res)
        })
        observeEvent(input$nextq, {
            res$current_row <- res$current_row + 1
            if (res$current_row > nrow(oolong_res)) {
                res$current_row <- 1
            }
            output <- .ren(output, oolong_res, res)
        })
        observeEvent(input$done, (
            stopApp(res$intruders)
        ))

    }
    runGadget(ui, server)
}

#' @export
print.oolong_test <- function(oolong_test) {
    
    cat(paste0("An oolong test object with k = ", nrow(oolong_test$oolong_test), ", ", sum(!is.na(oolong_test$oolong_test$answer)), " coded. (", round(.cal_oolong_correct(oolong_test$oolong_test), 3)," accuracy)\n"))
}

#' @export
generate_oolong_test <- function(model, n_top_terms = 5, bottom_terms_percentile = 0.6) {
    if ("WarpLDA" %in% class(model)) {
        K <- model$.__enclos_env__$private$n_topics
        V <- length(model$.__enclos_env__$private$vocabulary)
        terms <- t(model$get_top_words(n = V))
        all_terms <- unique(as.vector(terms[,seq_len(n_top_terms)]))
    } else if ("STM" %in% class(model)) {
        K <- model$settings$dim$K
        V <- model$settings$dim$V
        terms <- labelTopics(model, n = model$settings$dim$V)$frex
        all_terms <- unique(as.vector(terms[,seq_len(n_top_terms)]))
    }
    oolong_test <- purrr::map_dfr(seq_len(K), .gen_candidates, terms = terms, all_terms = all_terms, bottom_terms_percentile = bottom_terms_percentile)
    res <- list()
    res$oolong_test <- oolong_test
    res$hash <- digest::digest(oolong_test, "sha512")
    class(res) <- c(class(res), "oolong_test")
    return(res)
}

#' @export
do_oolong_test <- function(oolong_test) {
    oolong_test$oolong_test$answer <- .code_oolong(oolong_test$oolong_test)
    return(oolong_test)
}

