
.is_topic_model <- function(x) {
    if (any(class(x) %in% c("WarpLDA", "STM", "BTM", "keyATM_output","textmodel_lda", "textmodel_nb"))) {
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

##oolong for formats

.add_class <- function(output, added_class) {
    class(output) <- append(class(output), added_class)
    return(output)
}

.convert_input_model_s3 <- function(input_model) {
    if (!.is_topic_model(input_model)) {
        stop("input_model is not supported.")
    }
    output <- list()
    output$model <- input_model
    if ("WarpLDA" %in% class(input_model)) {
        return(.add_class(output, "input_model_s3_warplda"))
    }
    if ("textmodel_lda" %in% class(input_model)) {
        return(.add_class(output, "input_model_s3_seededlda"))
    }
    if ("textmodel_nb" %in% class(input_model)) {
        return(.add_class(output, "input_model_s3_nb"))
    }
    if ("STM" %in% class(input_model)) {
        return(.add_class(output, "input_model_s3_stm"))
    }
    if ("BTM" %in% class(input_model)) {
        return(.add_class(output, "input_model_s3_btm"))
    }
    if ("keyATM_output" %in% class(input_model)) {
        return(.add_class(output, "input_model_s3_keyatm"))
    }
    if ("topicmodels" == attr(class(input_model), "package")) {
        return(.add_class(output, "input_model_s3_topicmodels"))
    }
}
