#' Print and plot oolong summary
#'
#' These functions print or plot a useful summary of the results from \code{\link{summarize_oolong}}. For details, please see the overview vignette: \code{vignette("overview", package = "oolong")}
#'
#' @section Summary:
#' 
#' Print function displays the following information:
#' \describe{
#'   \item{Mean model precision}{(tm) Higher value indicates better topic interpretability}
#'   \item{Quantiles of model precision}{(tm) Higher value indicates better topic interpretability}
#'   \item{P-value of the model precision}{(tm) Model precision's p-value calculated by one-sample binomial test and Fisher's Omnibus method.}
#'   \item{Krippendorff's alpha}{(gs/tm) Krippendorff's Alpha, if more than one oolong object is analyzed.}
#'   \item{K Precision}{(tm) Model precision for each topic.}
#'   \item{Mean TLO}{(tm) Mean topic log odds, higher value indicates better interpretability}
#'   \item{Median TLO}{(tm) Median topic log odds, higher value indicates better interpretability}
#'   \item{Quantiles of TLO}{(tm) Quantiles of topic log odds}
#'   \item{P-Value of the median TLO}{(tm) Median topic log odds's p-value calculated by permutation test.}
#'   \item{Correlation (average answer)}{(gs) Pearson's correlation between average answer and target value}
#'   \item{Corrlation (content length)}{(gs) Pearson's correlation between content length and target value}
#' }
#' 
#' @section Diagnostic plot:
#' 
#' Plot function displays a diagnostic plot with the following subplots (gs only).
#' \describe{
#'   \item{Top left}{Correlation between answer from coders and target value to check for correlation between two values. Both axes are minmax transformed.}
#'   \item{Top right}{Bland-altman plot of answer from coders and target value to check for agreement between two values.}
#'   \item{Bottom left}{Correlation between target value and content length to check for the influence of content length.}
#'   \item{Bottom right}{Cook's distance to check for influential observations.}
#' }
#' 
#' @param x an oolong_summary
#' @param ... other parameters
#' @method print oolong_summary
#' @author Chung-hong Chan
#' @return None
#' @export
print.oolong_summary <- function(x, ...) {
    if (x$type == "tm") {
        .print_oolong_summary_tm(x)
    }
    if (x$type == "gs") {
        .print_oolong_summary_gs(x)
    }
}

#' @method plot oolong_summary
#' @rdname print.oolong_summary
#' @export
plot.oolong_summary <- function(x, ...) {
    if (x$type == "gs") {
        .plot_oolong_summary_gs(x)
    } else {
        stop("Don't know how to plot this oolong_summary.")
    }
    
}

.print_oolong_summary_tm <- function(oolong_summary) {
    .cp(TRUE, "Mean model precision: ", mean(oolong_summary$rater_precision))
    .cp(oolong_summary$n_models > 1, "Quantiles of model precision: ", paste(quantile(oolong_summary$rater_precision), collapse = ", "))
    .cp(oolong_summary$n_models > 1, "P-value of the model precision\n (H0: Model precision is not better than random guess): ", round(oolong_summary$rater_precision_p_value, 4))
    .cp(oolong_summary$n_models > 1, "Krippendorff's alpha: ", round(oolong_summary$kripp_alpha, 3))
    .cp(TRUE, "K Precision:\n", paste(round(oolong_summary$k_precision, 1), collapse = ", "))
    .cp(!is.na(oolong_summary$tlo[1]), "Mean TLO: ", round(mean(oolong_summary$tlo), 2))
    .cp(!is.na(oolong_summary$tlo[1]), "Median TLO: ", round(median(oolong_summary$tlo), 2))
    .cp(!is.na(oolong_summary$tlo[1]), "Quantiles of TLO: ", paste(round(quantile(oolong_summary$tlo), 2), collapse = ", "))
    if (!is.na(oolong_summary$tlo[1])) {
        .cp(TRUE, "P-Value of the median TLO \n(H0: Median TLO is not better than random guess): ", round(oolong_summary$tlo_p_value, 4))
    }
}

.print_oolong_summary_gs <- function(oolong_summary) {
    .cp(oolong_summary$n_models > 1, "Krippendorff's Alpha: ", round(oolong_summary$kripp_alpha$value, 3))
    .cp(!is.null(oolong_summary$cor), "Correlation: ", round(oolong_summary$cor$estimate, 3), " (p = ", round(oolong_summary$cor$p.value, 4), ")")
    .cp(!is.null(oolong_summary$cor_length), "Effect of content length: ", round(oolong_summary$cor_length$estimate, 3), " (p = ", round(oolong_summary$cor_length$p.value, 4), ")")
}

#' @rdname summarize_oolong
#' @export
summarise_oolong <- function(..., target_value = NULL) {
    summarize_oolong(..., target_value = NULL)
}

#' Summarize oolong objects
#'
#' This function summarizes one or more oolong objects. All oolong objects must be locked.
#' 
#' @param ... (tm/gs) one or more oolong objects to be summarized.
#' @param target_value (gs) a vector of numeric values, the value you want to validate against the human-coded gold standard. One example of this target value is sentiment score extracted automatically from text.
#' @return An oolong summary.
#' Depends on purpose, an oolong summary object has the following values:
#' \describe{
#'   \item{\code{$type}}{(gs/tm) type of analysis, either 'gs' or 'tm'}
#'   \item{\code{$kripp_aplha}}{(gs/tm) Krippendorff's Alpha, if more than one oolong object is analyzed.}
#'   \item{\code{$rater_precision}}{(tm) Model precision}
#'   \item{\code{$res$rater_precision_p_value}}{(tm) Model precision's p-value calculated by one-sample binomial test and Fisher's Omnibus method.}
#'   \item{\code{$k_precision}}{(tm) precision for each topic}
#'   \item{\code{$tlo}}{(tm) vector of topic log odds}
#'   \item{\code{$tlo_pvalue}}{(tm) Median topic log odds's p-value calculated by permutation test.}
#'   \item{\code{$cor}}{(gs) Pearson's correlation between average answer and target value}
#'   \item{\code{$cor_length}}{(gs) Pearson's correlation between content length and target value}
#'   \item{\code{$diag_plot}}{(gs) diagnostic plot.}
#' }
#' A useful summary of an object can be obtained either by \code{\link{print.oolong_summary}} or \code{\link{plot.oolong_summary}}. For details, please see the overview vignette: \code{vignette("overview", package = "oolong")}
#' @examples
#' # Please try this example in interactive R sessions only.
#' if (interactive()) {
#'    data(abstracts_stm)
#'    oolong_test1 <- create_oolong(abstracts_stm)
#'    oolong_test2 <- clone_oolong(oolong_test1)
#'    oolong_test1$do_word_intrusion_test()
#'    oolong_test2$do_word_intrusion_test()
#'    oolong_test1$lock()
#'    oolong_test2$lock()
#'    summarize_oolong(oolong_test1, oolong_test2)
#' }
#' @author Chung-hong Chan
#' @references
#'   Chang, J., Gerrish, S., Wang, C., Boyd-Graber, J. L., & Blei, D. M. (2009). Reading tea leaves: How humans interpret topic models. In Advances in neural information processing systems (pp. 288-296).
#' 
#'   Song et al. (2020) In validations we trust? The impact of imperfect human annotations as a gold standard on the quality of validation of automated content analysis. Political Communication.
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
