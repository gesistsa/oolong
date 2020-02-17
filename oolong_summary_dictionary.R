devtools::load_all()

summarize_oolong_diction <- function(...) {
    obj_list <- list(...)
    answers <- purrr::map_dfc(obj_list, ~.$.__enclos_env__$private$test_content$gold_standard$answer)
    colnames(answers) <- paste0("answer", seq_len(ncol(answers)))
    avg_answer <- apply(answers, 1, mean)
    answers$avg_answer <- avg_answer
    return(answers)
}

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

answers <- summarize_oolong_diction(trump, trump2)

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
