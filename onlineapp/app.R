library("shiny")
library("RMySQL")
library("oolong")

Oolong_test_tm <-
  R6::R6Class(
    "oolong_test_tm",
    inherit = Oolong_test,
    public = list(
      initialize = function(input_model, input_corpus = NULL, n_top_terms = 5, bottom_terms_percentile = 0.6, exact_n = 15, frac = NULL, n_top_topics = 3, n_topiclabel_words = 8, difficulty = 1, use_frex_words = FALSE, input_dfm = NULL) {
        private$test_content <- .generate_test_content(input_model, input_corpus, n_top_terms, bottom_terms_percentile, exact_n, frac, n_top_topics, n_topiclabel_words, difficulty, use_frex_words = use_frex_words, input_dfm = input_dfm)
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
      },
      return_data=function(){return(private$test_content$word)},
      get_test=function(){return(private$test_content)}
    ),
    private = list(
      hash = NULL,
      test_content = list(),
      finalized = FALSE
    )
  )

load("oolong_object.rdata")
test_content<-o1$get_test()
test_content<-test_content$word
user <- 'hong'
password <- 'test'

# does NOT COLLECT THE ANSWERS!!!
build_df<-function(o1,updated,user){
  res0<-o1$return_data()
  cands<-as.data.frame(do.call(rbind,res0$candidates))
  names(cands)<-paste0("cand_",names(cands))
  res0<-cbind(res0,cands)
  res0$candidates<-NULL
  res0$answer<-updated$intruder
  res0$user<-user
  res0$session<-Sys.time()
return(res0)}

lapply(dbListConnections(MySQL()),dbDisconnect)


## name strange?
pasteToDb<-function(results,db_user,db_password){

  db_name <- 'teststorage'
  db_host <- '134.155.108.237' # our server access
  db_port <- 3306
  
  us <-  dbConnect(RMySQL::MySQL(), user = user, password = password,
                   dbname = db_name, host = db_host, port = db_port)
  dbWriteTable(us,"temp",results,overwrite=T)
  dbExecute(us,"INSERT into results
            SELECT position,
            t,
            intruder,
            answer,
            cand_V1,
            cand_V2,
            cand_V3,
            cand_V4,
            cand_V5,
            user,
            session
            FROM temp;")
  dbExecute(us,"drop table temp;")
  dbDisconnect(dbListConnections(MySQL())[[1]])
}

# open a database connection
  # generate a new table for the user
  # add a name!


UI_WORD_INTRUSION_TEST <- miniUI::miniPage(
  miniUI::gadgetTitleBar("oolong"),
  miniUI::miniContentPanel(
    shiny::uiOutput("current_topic"),
    shiny::uiOutput("intruder_choice"),
    shiny::actionButton("confirm", "confirm"),
    shiny::actionButton("nextq", "skip")
  )
)




# test content muss bereits direkt nach der erzeugung abgefangen werdne

                                # res scheint eine auswahl aus tc zu sein
                                # wird durch tlo_cal erzeugt

ren_word_intrusion_test <- function(output, test_content, res) {
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


gen_shinyapp_exo <- function(test_content, ui = .UI_WORD_INTRUSION_TEST, .ren = .ren_word_intrusion_test) {
  server <- function(input, output, session) {
    res <- shiny::reactiveValues(intruder = test_content$answer, current_row = 1)
    # hier passwort und nutzer abfragen
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
    pasteToDb(build_df(o1,updated=res,user=user),db_user = user,db_password=password) # added
      ))
       
   
            shiny::observeEvent(input$done, (
     shiny::stopApp(res$intruder)
    ))


  }
  return(shiny::shinyApp(ui, server))
}

## müssen die positionen von test content 
gen_shinyapp_exo(test_content,ui=UI_WORD_INTRUSION_TEST,.ren = ren_word_intrusion_test)

