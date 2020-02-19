
#' @export
print.oolong_summary <- function(oolong_summary) {
    if (oolong_summary$type == "tm") {
        .print_oolong_summary_tm(oolong_summary)
    }
    if (oolong_summary$type == "gs") {
        .print_oolong_summary_gs(oolong_summary)
    }
}

#' @export
plot.oolong_summary <- function(oolong_summary) {
    if (oolong_summary$type == "gs") {
        .plot_oolong_summary_gs(oolong_summary)
    } else {
        stop("Don't know how to plot this oolong_summary.")
    }
    
}

.print_oolong_summary_tm <- function(oolong_summary) {
    .cp(TRUE, "Mean model precision: ", mean(oolong_summary$rater_precision))
    .cp(oolong_summary$n_models > 1, "Quantiles of model precision: ", paste(quantile(oolong_summary$rater_precision), collapse = ", "))
    .cp(oolong_summary$n_models > 1, "P-value of the model precision (H0: Model precision is not better than random guess): ", oolong_summary$rater_precision_p_value)
    .cp(oolong_summary$n_models > 1, "Krippendorff's alpha: ", oolong_summary$kripp_alpha)
    .cp(TRUE, "K Precision: ", paste(round(oolong_summary$k_precision, 1), collapse = ", "))
    .cp(!is.na(oolong_summary$tlo[1]), "Mean TLO: ", round(mean(oolong_summary$tlo), 2))
    .cp(!is.na(oolong_summary$tlo[1]), "Median TLO: ", round(median(oolong_summary$tlo), 2))
    .cp(!is.na(oolong_summary$tlo[1]), "Quantiles of TLO: ", paste(quantile(oolong_summary$tlo), collapse = ", "))
    if (!is.na(oolong_summary$tlo[1])) {
        .cp(TRUE, "P-Value of the median TLO (H0: Median TLO is not better than random guess): ", oolong_summary$tlo_p_value)
    }
}

.print_oolong_summary_gs <- function(oolong_summary) {
    .cp(!is.null(oolong_summary$kripp), "Krippendorff's Alpha: ", oolong_summary$kripp$value)
    .cp(!is.null(oolong_summary$cor), "Correlation: ", round(oolong_summary$cor$estimate, 3), " (p = ", round(oolong_summary$cor$p.value,3), ")")
    .cp(!is.null(oolong_summary$cor_length), "Effect of content length: ", round(oolong_summary$cor_length$estimate, 3), " (p = ", round(oolong_summary$cor_length$p.value,3), ")")
}

#' @export
summarise_oolong <- function(...) {
    summarize_oolong(...)
}


#' @export
summarize_oolong <- function(..., target_value = NULL) {
    obj_list <- list(...)
    if (.is.oolong_tm(obj_list[[1]])) {
        return(.summarize_oolong_tm(...))
    } else {
        return(.summarize_oolong_gs(..., target_value = target_value))
    }
}

### test whether the oolong obj is created for a topic model
.is.oolong_tm <- function(oolong) {
    if ("oolong_test_tm" %in% class(oolong)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


.plot_oolong_summary_gs <- function(oolong_summary) {
    oolong_summary$diag_plot
}
