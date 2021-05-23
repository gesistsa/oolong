.code_oolong <- function(test_content, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_intrusion_test) {
    shiny::runGadget(.gen_shinyapp(test_content = test_content, ui = ui, .ren = .ren, hash = NULL))
}

.do_oolong_test <- function(test_content, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_intrusion_test) {
    test_content$answer <- .code_oolong(test_content, ui = ui, .ren = .ren)
    return(test_content)

}

.gen_shinyserver <- function(test_content, .ren = .ren_word_intrusion_test, hash = NULL) {
    function(input, output, session) {
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
        output$download <- shiny::downloadHandler(
            filename = function() {
                paste0('oolong_', Sys.time(), " ", input$userid, '.RDS')
            },
            content = function(file) {
                output <- list()
                test_content$answer <- res$intruder
                output$test_content <- test_content
                output$hash <- hash
                output$test_content_hash <- .safe_hash(output$test_content)
                output$userid <- input$userid
                saveRDS(output, file)
            }
        )
    }
}

.gen_shinyapp <- function(test_content, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_intrusion_test, hash = NULL) {
    server <- .gen_shinyserver(test_content = test_content, .ren = .ren, hash = hash)
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

.mobilize_defend <- function(oolong) {
    if (oolong$.__enclos_env__$private$finalized) {
        stop("oolong is locked.")
    }
    if (!.check_new(oolong)) {
        stop("oolong is partially coded.")
    }
    if (length(oolong$.__enclos_env__$private$test_content) != 1) {
        stop("Deployment of oolong object with more than two test items (e.g. witi) is not supported")
    }
}

.mobilize <- function(oolong) {
    .mobilize_defend(oolong)
    res <- list()
    res$test_content <- oolong$.__enclos_env__$private$test_content
    res$hash <- oolong$.__enclos_env__$private$hash
    res$type <- names(res$test_content)
    res$construct <- oolong$.__enclos_env__$private$construct
    return(res)
}

#' Deploy an oolong test
#' 
#' In most of the time, you should not use this function. You should write the deployable version of your app into a directory using \code{export_oolong} instead. Please refer to the deployment guide for more details.
#' @param oolong an oolong object to be deployed. Please note that the "witi" type, i.e. oolong object with both word and topic intrusion tests, cannot be deployed. Also the object must not be locked and ever coded.
#' @return Nothing, it launches a deployable version of the coding interface
#' @examples
#' # Please try this example in interactive R sessions only.
#' if (interactive()) {
#'    data(abstracts_stm)
#'    x <- wi(abstracts_stm)
#'    deploy_oolong(x)
#' }
#' @author Chung-hong Chan
#' @export
deploy_oolong <- function(oolong) {
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
            return(.ren_gold_standard_test(output, test_content, res, construct = mob_oolong$construct, hash = hash))
        }
        return(.gen_shinyapp(mob_oolong$test_content$gold_standard, ui = .UI_GOLD_STANDARD_TEST, .ren = .ren, hash = mob_oolong$hash))
    }
}

#' Export a deployable Shiny app from an oolong object into a directory
#' 
#' This function exports your oolong test into a launched Shiny app that is ideal for online deployment. Deploying the Shiny app online allows coders to conduct the test online with their browser, rather than having to install R on their own computer. In constrast to the testing interfaces launched with methods such as \code{$do_word_intrusion_test()}, the deployable version provides data download after the coder finished coding. Downloaded data can then revert back to a locked oolong object using \code{revert_oolong}. Further version might provide solutions to permanent storage. The deployable Shiny app will be in a directory. The Shiny app is both launchable with shiny::runApp() and deployable with rsconnect::deployApp(). See the deployment guide for more details.
#' @param oolong an oolong object to be exported. Please note that the "witi" type, i.e. oolong object with both word and topic intrusion tests, cannot be exported. Also the object must not be locked and ever coded.
#' @param dir character string, the directory to be exported. Default to a temporary directory
#' @param verbose logical, whether to display information after exporting
#' @param use_full_path logical, whether to expand dir into full path
#' @return directory exported, invisible
#' @examples
#' # Please try this example in interactive R sessions only.
#' if (interactive()) {
#'    data(abstracts_stm)
#'    x <- wi(abstracts_stm)
#'    export_oolong(x)
#' }
#' @author Chung-hong Chan
#' @export
export_oolong <- function(oolong, dir = base::tempdir(), verbose = TRUE, use_full_path = TRUE) {
    .mobilize_defend(oolong)
    if (!dir.exists(dir)) {
        dir.create(dir)
    }
    if (use_full_path) {
        dir <- base::path.expand(dir)
    }
    file.copy(system.file("app", "app.R", package = "oolong"), dir, overwrite = TRUE)
    saveRDS(oolong, file = paste0(dir, "/oolong.RDS"))
    .cp(verbose, "The Shiny has been written to the directory: ", dir)
    .cp(verbose, "You can test the app with: shiny::runApp(\"", dir, "\")")
    invisible(dir)
}
