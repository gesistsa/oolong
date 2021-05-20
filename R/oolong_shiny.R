.code_oolong <- function(test_content, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_intrusion_test) {
    shiny::runGadget(.gen_shinyapp(test_content = test_content, ui = ui, .ren = .ren, hash = NULL))
}

.do_oolong_test <- function(test_content, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_intrusion_test) {
    test_content$answer <- .code_oolong(test_content, ui = ui, .ren = .ren)
    return(test_content)

}

.gen_shinyapp <- function(test_content, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_intrusion_test, hash = NULL) {
    server <- function(input, output, session) {
        res <- shiny::reactiveValues(intruder = test_content$answer, current_row = 1)
        output <- .ren(output, test_content, res, hash = hash)
        shiny::observeEvent(input$confirm, {
            res$intruder[res$current_row] <- input$intruder
            res$current_row <- res$current_row + 1
            if (res$current_row > nrow(test_content)) {
                res$current_row <- 1
            }
            output <- .ren(output, test_content, res, hash = hash)
        })
        shiny::observeEvent(input$nextq, {
            res$current_row <- res$current_row + 1
            if (res$current_row > nrow(test_content)) {
                res$current_row <- 1
            }
            output <- .ren(output, test_content, res, hash = hash)
        })
        shiny::observeEvent(input$ff, {
            res_with_na <- which(is.na(res$intruder))
            if (length(res_with_na) == 0) {
                res$current_row <- res$current_row
            } else {
                res$current_row <- min(res_with_na)
            }
            output <- .ren(output, test_content, res, hash = hash)
        })
        shiny::observeEvent(input$done, (
            shiny::stopApp(res$intruder)
        ))
        output$download <- downloadHandler(
            filename = function() {
                paste0('oolong_', Sys.time(), " ", input$userid, '.RDS')
            },
            content = function(file) {
                res <- list()
                test_content$answer <- res$intruder
                res$test_content <- test_content
                res$hash <- hash
                res$hash <- .safe_hash(res$test_content)
                res$userid <- input$userid
                saveRDS(res, file)
            }
        )
    }
    return(shiny::shinyApp(ui, server))
}

### .gen_shinyapp must take a UI and render function. For new test_content type, please prepare a new pair of
## 1. .UI_XXX_test
## 2. .ren_xxx_test

## WSI also use .UI_WORD_INTRUSION_TEST
.UI_WORD_INTRUSION_TEST <- miniUI::miniPage(
    miniUI::gadgetTitleBar("oolong"),
    miniUI::miniContentPanel(
        shiny::uiOutput("current_topic"),
        shiny::uiOutput("intruder_choice"),
        shiny::actionButton("confirm", "confirm"),
        shiny::actionButton("nextq", "skip"),
        shiny::actionButton("ff", "jump to uncoded item"),
        shiny::uiOutput("userid_entry"),
        shiny::uiOutput("download_button")
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
        shiny::actionButton("ff", "jump to uncoded item"),
        shiny::uiOutput("userid_entry"),
        shiny::uiOutput("download_button")
        )
    )

.UI_GOLD_STANDARD_TEST <-
    miniUI::miniPage(
        miniUI::gadgetTitleBar("oolong"),
        miniUI::miniContentPanel(
            shiny::uiOutput("current_topic"),
            shiny::uiOutput("text_content"),
            shiny::uiOutput("score_slider"),
            shiny::actionButton("confirm", "confirm"),
            shiny::actionButton("nextq", "skip"),
            shiny::actionButton("ff", "jump to uncoded item"),
            shiny::uiOutput("userid_entry"),
            shiny::uiOutput("download_button")
            )
        )


.ren_word_intrusion_test <- function(output, test_content, res, prompt = "Which of the following is an intruder word?", hash = NULL) {
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
    if (!is.null(hash)) {
        output$download_button <- shiny::renderUI({
            if (!any(is.na(res$intruder))) {
                shiny::downloadButton("download", "download")
            }
        })
        output$userid_entry <- shiny::renderUI({
            if (!any(is.na(res$intruder))) {
                shiny::textInput("userid", "Your name:", NULL)
            }
        })
    }
    return(output)
}

.ren_topic_intrusion_test <- function(output, test_content, res, hash = NULL) {
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
    if (!is.null(hash)) {
        output$download_button <- shiny::renderUI({
            if (!any(is.na(res$intruder))) {
                shiny::downloadButton("download", "download")
            }
        })
        output$userid_entry <- shiny::renderUI({
            if (!any(is.na(res$intruder))) {
                shiny::textInput("userid", "Your name:", NULL)
            }
        })
    }
    return(output)
}

.ren_word_set_intrusion_test <- function(output, test_content, res, prompt = "Which of the following is an intruder word set?", hash = NULL) {
    .ren_word_intrusion_test(output = output, test_content = test_content, res = res, prompt = prompt, hash = hash)
}

.ren_gold_standard_test <- function(output, test_content, res, construct = "positive", hash = NULL) {
    .ren_choices <- function(test_content, res, construct) {
        shiny::renderUI({
            shiny::sliderInput("intruder", label = paste("How ", construct, "is this text? (1 = Very not ", construct, "; 5 = Very ", construct, ")"), min = 1, max = 5, value = ifelse(is.na(res$intruder[res$current_row]), 3, res$intruder[res$current_row]), ticks = FALSE)
        })
    }
    .ren_topic_bar <- function(test_content, res) {
        shiny::renderUI({
            shiny::strong(paste("Case ", res$current_row, "of", nrow(test_content), ifelse(is.na(res$intruder[res$current_row]), "", " [coded]")))
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
    output$score_slider <- .ren_choices(test_content, res, construct)
    output$current_topic <- .ren_topic_bar(test_content, res)
    output$text_content <- .ren_text_content(test_content, res)
    if (!is.null(hash)) {
        output$download_button <- shiny::renderUI({
            if (!any(is.na(res$intruder))) {
                shiny::downloadButton("download", "download")
            }
        })
        output$userid_entry <- shiny::renderUI({
            if (!any(is.na(res$intruder))) {
                shiny::textInput("userid", "Your name:", NULL)
            }
        })
    }
    return(output)
}

.mobilize <- function(oolong) {
    if (oolong$.__enclos_env__$private$finalized) {
        stop("oolong is locked.")
    }
    if (!.check_new(oolong)) {
        stop("oolong is partially coded.")
    }
    res <- list()
    res$test_content <- oolong$.__enclos_env__$private$test_content
    res$hash <- oolong$.__enclos_env__$private$hash
    res$type <- names(res$test_content)
    res$construct <- oolong$.__enclos_env__$private$construct
    return(res)
}


#' @export
deploy <- function(oolong) {
    if (length(oolong$.__enclos_env__$private$test_content) != 1) {
        stop("Deployment of oolong with more than two test items (e.g. witi) is not supported")
    }
    mob_oolong <- .mobilize(oolong)
    ### could use switch
    if (mob_oolong$type == "word") {
        return(.gen_shinyapp(mob_oolong$test_content$word, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_intrusion_test, hash = mob_oolong$hash))
    } else if (mob_oolong$type == "topic") {
        return(.gen_shinyapp(mob_oolong$test_content$topic, ui = .UI_TOPIC_INTRUSION_TEST, .ren = .ren_topic_intrusion_test, hash = mob_oolong$hash))
    } else if (mob_oolong$type == "wsi") {
        return(.gen_shinyapp(mob_oolong$test_content$wsi, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_set_intrusion_test, hash = mob_oolong$hash))
    } else if (mob_oolong$type == "gold_standard") {
        .ren <- function(output, test_content, res, hash = NULL) {
            return(.ren_gold_standard_test(output, test_content, res, construct = mob_oolong$construct, hash = NULL))
        }
        return(.gen_shinyapp(mob_oolong$test_content$gold_standard, ui = .UI_GOLD_STANDARD_TEST, .ren = .ren, hash = mob_oolong$hash))
    } 

}

undeploy <- function(oolong, tm_word_rds) {
    res <- readRDS(tm_word_rds)
    cloned_oolong <- clone_oolong(oolong)
    hash <- unique(test_content$hash)
    userid <- unique(test_content$userid)
    if (hash != cloned_oolong$.__enclos_env__$private$hash) {
        stop("The oolong test result does not match the original oolong object.", call. = FALSE)
    }
    cloned_oolong$.__enclos_env__$private$test_content$word <- test_content[,1:5]
    cloned_oolong$userid <- userid
    cloned_oolong$lock()
    return(cloned_oolong)
}
