.minmax <- function(x) {
    (x - min(x)) / (max(x) - min(x)) 
}

.corr_plot <- function(answers, target_value) {
    plot_data <- tibble::tibble(target_value = .minmax(target_value), avg_answer = .minmax(answers$avg_answer))
    ggplot2::ggplot(plot_data, ggplot2::aes(x = avg_answer, y = target_value)) + ggplot2::geom_point() + ggplot2::geom_smooth(method = lm) + ggplot2::xlab("Human judgement") + ggplot2::ylab("Target value") -> correlation_plot
    return(correlation_plot)
}

.ba_plot <- function(answers, target_value) {
    plot_data <- tibble::tibble(target_value = .minmax(target_value), avg_answer = .minmax(answers$avg_answer))
    plot_data$diffxy <- plot_data$target_value - plot_data$avg_answer
    plot_data$meanxy <- (plot_data$target_value + plot_data$avg_answer) / 2
    mean_diff_xy <- mean(plot_data$diffxy)
    sd_diff_xy <- sd(plot_data$diffxy)
    ggplot2::ggplot(plot_data, ggplot2::aes(x = meanxy, y = diffxy)) + ggplot2::geom_point() + ggplot2::geom_hline(yintercept = mean_diff_xy) + ggplot2::geom_hline(yintercept = mean_diff_xy - 1.96 * sd_diff_xy, linetype = 2) + ggplot2::geom_hline(yintercept = mean_diff_xy + 1.96 * sd_diff_xy, linetype = 2) + ggplot2::xlab("Mean of two values") + ggplot2::ylab("Difference between two values")
}

.length_plot <- function(oolong, target_value, ...) {
    gold <- oolong$turn_gold()
    nwords <- quanteda::ntoken(gold, ...)
    plot_data <- tibble::tibble(target_value = .minmax(target_value), word_length = nwords)
    ggplot2::ggplot(plot_data, ggplot2::aes(x = word_length, y = target_value)) + ggplot2::geom_point() + ggplot2::geom_smooth(method = lm) + ggplot2::xlab("Number of words") + ggplot2::ylab("Target value")
}

.cook_plot <- function(answers, target_value) {
    lmobj <- lm(answers$avg_answer ~ target_value)
    cutoff <- 4 / (length(target_value) - 2 - 1)
    ggplot2::ggplot(tibble::tibble(index = seq_along(target_value), cookd = cooks.distance(lmobj)), ggplot2::aes(x = index, y = cookd)) + ggplot2::geom_point() + ggplot2::geom_hline(yintercept = cutoff, linetype = 2) + ggplot2::xlab("Index") + ggplot2::ylab("Cook's distance")
}

.summarize_oolong_gs <- function(..., target_value = NULL, plot = TRUE) {
    obj_list <- list(...)
    answers <- purrr::map_dfc(obj_list, ~.$.__enclos_env__$private$test_content$gs$answer)
    colnames(answers) <- paste0("answer", seq_len(ncol(answers)))
    avg_answer <- apply(answers, 1, mean)
    answers$avg_answer <- avg_answer
    if (length(obj_list) > 1) {
        kripp <- irr::kripp.alpha(t(as.matrix(answers[,grepl("^answer", colnames(answers))])), method = "ordinal")
    } else {
        kripp <- NA
    }
    res <- list()
    class(res) <- append(class(res), "oolong_summary")
    res$type <- "gs"
    res$kripp_alpha <- kripp
    res$n_models <- length(obj_list)
    res$obj_list <- obj_list
    if (is.null(target_value)) {
        warning("target_value is NULL, only the reliability of the answers from the coder(s) are studied.")
        return(res)
    }
    p1 <- .corr_plot(answers, target_value)
    p2 <- .ba_plot(answers, target_value)
    p3 <- .length_plot(obj_list[[1]], target_value)
    p4 <- .cook_plot(answers, target_value)
    res$diag_plot <- cowplot::plot_grid(p1, p2, p3, p4)
    res$cor <- cor.test(.minmax(target_value), .minmax(answers$avg_answer))
    text_length <- quanteda::ntoken(obj_list[[1]]$turn_gold(), remove_punct = TRUE)
    res$cor_length <- cor.test(.minmax(target_value), text_length)
    res$type <- "gs"
    class(res) <- append(class(res), "oolong_summary")
    return(res)
}
