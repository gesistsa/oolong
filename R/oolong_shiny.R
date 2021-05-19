
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

### It must take a UI and render function. For new test_content type, please prepare a new pair of
## 1. .UI_XXX_test (in cap, located in oolong_shinyapplayout.R)
## 2. .ren_xxx_test

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

.ren_word_intrusion_test <- function(output, test_content, res, prompt = "Which of the following is an intruder word?") {
    .ren_choices <- function(test_content, res) {
        shiny::renderUI({
            shiny::radioButtons("intruder", label = prompt, choices = test_content$candidates[[res$current_row]], selected = res$intruder[res$current_row])
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

.ren_word_set_intrusion_test <- function(output, test_content, res, prompt = "Which of the following is an intruder word set?") {
    .ren_word_intrusion_test(output = output, test_content = test_content, res = res, prompt = prompt)
}

.code_oolong <- function(test_content, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_intrusion_test) {
    shiny::runGadget(.gen_shinyapp(test_content = test_content, ui = ui, .ren = .ren))
}
