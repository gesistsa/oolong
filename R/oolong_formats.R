.is_topic_model <- function(x) {
    if (any(class(x) %in% c("WarpLDA", "STM", "BTM", "keyATM_output"))) {
        return(TRUE)
    }
    if (is.null(attr(class(x), "package"))) {
        return(FALSE)
    }
    if ("topicmodels" == attr(class(x), "package")) {
        return(TRUE)
    }
    return(FALSE)
}

### Future expansion of formats should go here.
.convert_input_model_s3 <- function(input_model) {
    if (!.is_topic_model(input_model)) {
        stop("input_model is not supported.")
    }
    output <- list()
    output$model <- input_model
    if ("WarpLDA" %in% class(input_model)) {
        class(output) <- append(class(output), "input_model_s3_warplda")
    } else if ("STM" %in% class(input_model)) {
        class(output) <- append(class(output), "input_model_s3_stm")
    } else if ("BTM" %in% class(input_model)) {
        class(output) <- append(class(output), "input_model_s3_btm")
    } else if ("keyATM_output" %in% class(input_model)) {
        class(output) <- append(class(output), "input_model_s3_keyatm")
    } else if ("topicmodels" == attr(class(input_model), "package")) {
        class(output) <- append(class(output), "input_model_s3_topicmodels")
    }
    return(output)
}
