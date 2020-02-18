devtools::load_all()

.minmax <- function(x) {
    (x - min(x)) / (max(x) - min(x)) 
}

.corr_plot <- function(answers, target_value) {
    tibble::tibble(target_value = .minmax(target_value), avg_answer = .minmax(answers$avg_answer)) %>% ggplot2::ggplot(ggplot2::aes(x = avg_answer, y = target_value)) + ggplot2::geom_point() + ggplot2::geom_smooth(method = lm) -> correlation_plot
    return(correlation_plot)
}

.ba_plot <- function(answers, target_value) {
    plot_data <- tibble::tibble(target_value = .minmax(target_value), avg_answer = .minmax(answers$avg_answer))
    plot_data$diffxy <- plot_data$target_value - plot_data$avg_answer
    plot_data$meanxy <- (plot_data$target_value + plot_data$avg_answer) / 2
    mean_diff_xy <- mean(plot_data$diffxy)
    sd_diff_xy <- sd(plot_data$diffxy)
    ggplot2::ggplot(plot_data, ggplot2::aes(x = meanxy, y = diffxy)) + ggplot2::geom_point() + ggplot2::geom_hline(yintercept = mean_diff_xy) + ggplot2::geom_hline(yintercept = mean_diff_xy - 1.96 * sd_diff_xy, linetype = 2) + ggplot2::geom_hline(yintercept = mean_diff_xy + 1.96 * sd_diff_xy, linetype = 2)
}

.length_plot <- function(oolong, target_value, ...) {
    gold <- oolong$turn_gold()
    nwords <- ntoken(gold, ...)
    plot_data <- tibble::tibble(target_value = .minmax(target_value), word_length = nwords)
    ggplot2::ggplot(plot_data, ggplot2::aes(x = word_length, y = target_value)) + ggplot2::geom_point() + ggplot2::geom_smooth(method = lm)
}

.cook_plot <- function(answers, target_value) {
    lmobj <- lm(answers$avg_answer ~ target_value)
    cutoff <- 4 / (length(target_value) - 2 - 1)
    ggplot2::ggplot(tibble::tibble(index = seq_along(target_value), cookd = cooks.distance(lmobj)), ggplot2::aes(x = index, y = cookd)) + ggplot2::geom_point() + ggplot2::geom_hline(yintercept = cutoff, linetype = 2)
}

summarize_oolong_gold_standard <- function(..., target_value = NULL) {
    if (is.null(target_value)) {
        warning("target_value is NULL, only the reliability of the answers from the coder(s) are studied.")
    }
    obj_list <- list(...)
    answers <- purrr::map_dfc(obj_list, ~.$.__enclos_env__$private$test_content$gold_standard$answer)
    colnames(answers) <- paste0("answer", seq_len(ncol(answers)))
    avg_answer <- apply(answers, 1, mean)
    answers$avg_answer <- avg_answer
    p1 <- .corr_plot(answers, target_value)
    p2 <- .ba_plot(answers, target_value)
    p3 <- .length_plot(obj_list[[1]], target_value)
    p4 <- .cook_plot(answers, target_value)
    plot_grid(p1, p2, p3, p4)
    ##return(answers)
}
require(cowplot)
trump <- create_oolong(input_corpus = trump2k)
trump2 <- clone_oolong(trump)

.monkey_dictionary <- function(oolong, n = 20) {
    oolong$.__enclos_env__$private$test_content$gold_standard$answer <- sample(1:5, size = n, replace = TRUE)
    return(oolong)
}

trump <- .monkey_dictionary(trump)
trump2 <- .monkey_dictionary(trump2)


trump$lock()
trump2$lock()

gold_standard <- trump$turn_gold()

dfm(gold_standard, remove_punct = TRUE) %>% dfm_lookup(afinn) %>% quanteda::convert(to = "data.frame") %>%
    mutate(matching_word_valence = (neg5 * -5) + (neg4 * -4) + (neg3 * -3) + (neg2 * -2) + (neg1 * -1)
           + (zero * 0) + (pos1 * 1) + (pos2 * 2) + (pos3 * 3) + (pos4 * 4) + (pos5 * 5),
           base = ntoken(gold_standard, remove_punct = TRUE), afinn_score = matching_word_valence / base) %>%
    pull(afinn_score) -> target_value

summarize_oolong_gold_standard(trump, trump2, target_value = target_value)

require(ggplot2)

tibble(target_value, avg_answer = answers$avg_answer) %>% mutate(minmax_target = (target_value - min(target_value)) / (max(target_value) - min(target_value)), minmax_answer = (avg_answer - min(avg_answer)) / (max(avg_answer) - min(avg_answer))) %>% ggplot(aes(x = minmax_target, y = minmax_answer)) + geom_point() + geom_smooth(method = lm)

### Correlation
tibble(target_value, avg_answer = answers$avg_answer) %>% mutate(minmax_target = (target_value - min(target_value)) / (max(target_value) - min(target_value)), minmax_answer = (avg_answer - min(avg_answer)) / (max(avg_answer) - min(avg_answer))) %>% mutate(avg_xy = (minmax_target + minmax_answer)/2 , diff_xy = minmax_target - minmax_answer) -> res

mean_diff_xy <- mean(res$diff_xy)
sd_diff_xy <- sd(res$diff_xy)

### bland altman
res %>% ggplot(aes(x = avg_xy, y = diff_xy)) + geom_point() + geom_hline(yintercept = mean_diff_xy) + geom_hline(yintercept = mean_diff_xy - 1.96 * sd_diff_xy, linetype = 2) + geom_hline(yintercept = mean_diff_xy + 1.96 * sd_diff_xy, linetype = 2)

res %>% mutate(nwords = ntoken(gold_standard, remove_punct = TRUE)) %>% ggplot(aes(x = nwords, y = minmax_target)) + geom_point() + geom_smooth(method = lm)

require(car)

res %>% lm(minmax_target ~ minmax_answer, data = .) %>% influencePlot
