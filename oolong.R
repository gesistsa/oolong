require(stm)

stm_models <- readRDS("~/dev/jcmc/stm_models.RDS")


stm_models[[2]]



insert <- function(good_terms, intruder, position) {
    length_test_items <- length(c(good_terms, intruder))
    res <- rep(NA, length_test_items)
    res[position] <- intruder
    res[setdiff(1:length_test_items, position)] <- sample(good_terms)
    return(tibble(position = position, candidates = list(res)))
}


gen_candidates <- function(i, terms, n_top_terms = 5, bottom_terms = 100) {
    good_terms <- head(terms$frex[i,], n_top_terms)
    intruder <- sample(tail(terms$frex[i,], bottom_terms), 1)
    position <- sample(1:(n_top_terms + 1), 1)
    insert(good_terms, intruder, position) -> res
    res$i <- i
    res$intruder <- res$candidates[[1]][res$position]
    return(res)
}

require(purrr)
require(tibble)

input_stm <- stm_models[[2]]
terms <- labelTopics(input_stm, n = input_stm$settings$dim$V)
res <- map_dfr(1:input_stm$settings$dim$K, gen_candidates, terms = terms)

require(shiny)
require(miniUI)

display_data <- function(x) {
    ui <- miniPage(
        gadgetTitleBar("Oolong"),
        miniContentPanel(
            uiOutput("intruder_choice"),
            actionButton("confirm", "confirm")
        )
    )
    server <- function(input, output, session) {
        res <- reactiveValues(intruders = rep(NA, nrow(x)), current_row = 1)
        observeEvent(input$done, (
            stopApp(res$intruders)
        ))
        output$intruder_choice <- renderUI({
            radioButtons("intruder", label = "Which of the following is the intruder word?", choices = x$candidates[[res$current_row]])
        })
    }
    runGadget(ui, server)
}


output <- rep(NA, nrow(res))

for(i in 1:nrow(res)) {
    output[i] <- display_data(res$candidates[[i]])
}

testing <- function() {
    ui <- miniPage(
        gadgetTitleBar("test"),
        miniContentPanel(
            textInput('test', label = "input something"),
            textOutput("data"),
            actionButton("confirm", "confirm")
        )
    )
    server <- function(input, output, session) {
        pointer <- reactiveValues(pointer = 0)
        observeEvent(input$done, (
            stopApp(pointer$pointer)
        ))
        observeEvent(input$confirm, {
            pointer$pointer <- pointer$pointer + 1
        })

    }
    runGadget(ui, server)
}
