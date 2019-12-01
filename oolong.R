require(stm)

stm_models <- readRDS("~/dev/jcmc/stm_models.RDS")


###stm_models[[2]]



insert <- function(good_terms, intruder, position) {
    length_test_items <- length(c(good_terms, intruder))
    res <- rep(NA, length_test_items)
    res[position] <- intruder
    res[setdiff(1:length_test_items, position)] <- sample(good_terms)
    return(tibble(position = position, candidates = list(res)))
}


gen_candidates <- function(i, terms, n_top_terms = 5, bottom_terms_percentile = 0.6, all_terms) {
    good_terms <- head(terms$frex[i,], n_top_terms)
    term_pos <- match(all_terms, terms$frex[i,])
    intruder <- sample(all_terms[term_pos > quantile(term_pos, bottom_terms_percentile)], 1)
    position <- sample(1:(n_top_terms + 1), 1)
    insert(good_terms, intruder, position) -> res
    res$t <- i
    res$intruder <- res$candidates[[1]][res$position]
    return(res)
}

require(purrr)
require(tibble)

gen_oolong <- function(input_stm, n_top_terms = 5, bottom_terms_percentile = 0.6)

input_stm <- stm_models[[2]]
terms <- labelTopics(input_stm, n = input_stm$settings$dim$V)
all_terms <- unique(as.vector(terms$frex[,1:n_top_terms]))





oolong_res <- map_dfr(1:input_stm$settings$dim$K, gen_candidates, terms = terms, all_terms = all_terms)

require(shiny)
require(miniUI)

code_oolong <- function(oolong_res) {
    ui <- miniPage(
        gadgetTitleBar("Oolong"),
        miniContentPanel(
            textOutput("current_topic"),
            uiOutput("intruder_choice"),
            actionButton("confirm", "confirm")
        )
    )
    server <- function(input, output, session) {
        res <- reactiveValues(intruders = rep(NA, nrow(oolong_res)), current_row = 1)
        output$intruder_choice <- renderUI({
            radioButtons("intruder", label = "Which of the following is the intruder word?", choices = oolong_res$candidates[[res$current_row]])
        })
        output$current_topic <- renderText({
            paste("Topic ", res$current_row, "of", nrow(oolong_res))
        })
        observeEvent(input$confirm, {
            res$intruders[res$current_row] <- input$intruder
            res$current_row <- res$current_row + 1
            if (res$current_row > nrow(oolong_res)) {
                res$current_row <- 1
            }
            output$intruder_choice <- renderUI({
                radioButtons("intruder", label = "Which of the following is the intruder word?", choices = oolong_res$candidates[[res$current_row]])
            })
            output$current_topic <- renderText({
                paste("Topic ", res$current_row, "of", nrow(oolong_res))
            })
        })
        observeEvent(input$done, (
            stopApp(res$intruders)
        ))

    }
    runGadget(ui, server)
}

code_oolong(oolong_res[1:2,])


## output <- rep(NA, nrow(res))

## for(i in 1:nrow(res)) {
##     output[i] <- display_data(res$candidates[[i]])
## }

## testing <- function() {
##     ui <- miniPage(
##         gadgetTitleBar("test"),
##         miniContentPanel(
##             textInput('test', label = "input something"),
##             textOutput("data"),
##             actionButton("confirm", "confirm")
##         )
##     )
##     server <- function(input, output, session) {
##         pointer <- reactiveValues(pointer = 0)
##         observeEvent(input$done, (
##             stopApp(pointer$pointer)
##         ))
##         observeEvent(input$confirm, {
##             pointer$pointer <- pointer$pointer + 1
##         })

##     }
##     runGadget(ui, server)
## }
