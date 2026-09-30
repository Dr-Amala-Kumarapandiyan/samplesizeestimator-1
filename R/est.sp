#' Sample size for estimating specificity
#' @description
#' In diagnostic studies, the test yields a binary outcome and accuracy is evaluated by sensitivity
#' and specificity. This function calculates sample size for estimating specificity when the diagnostic
#' test yields a binary outcome.
#'
#' @param p Prevalence of disease
#' @param sp anticipated specificity of the test ranges from 0 to 1
#' @param prec Precision required on either side of the true specificity
#' @param alp level of significance or accepted level of probability of type I error
#' @author R. Amala, Scientist-C, ICMR-VCRC, Puducherry  & G. Kumarapandiyan, Asst. Prof., Madras Christian College, Chennai
#' @return a list of total sample size based on specificity along with reporting
#' @import stringi stats
#' @export
#' @examples
#' est.sp(p = 0.10, sp = 0.90, prec = 0.05, alp = 0.05)
#' @references 1. Hajian-Tilaki, K. (2014). Sample size estimation in diagnostic test studies of biomedical informatics. Journal of biomedical informatics, 48, 193-204. 2.
est.sp <- function(p, sp, prec, alp) {
  n <- ceiling(((qnorm(1 - alp / 2))^2 * sp * (1 - sp)) / (prec^2 * (1 - p)))
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is not installed.")
  }
  if (!is.numeric(p) || length(p) != 1 || p <= 0 || p >= 1) {
    stop("'p' must be a single numeric value between 0 and 1.")
  }

  if (!is.numeric(sp) || length(sp) != 1 || sp <= 0 || sp >= 1) {
    stop("'sp' must be a single numeric value between 0 and 1.")
  }

  if (!is.numeric(prec) || length(prec) != 1 || prec <= 0) {
    stop("'prec' must be a single positive numeric value.")
  }

  if (!is.numeric(alp) || length(alp) != 1 || alp <= 0 || alp >= 1) {
    stop("'alp' must be a single numeric value between 0 and 1.")
  }

  s<-stringi::stri_paste("Description: \n The study would require a total sample size of ",
                         n,
                         " with anticipated specificity of ",
                         sp*100,
                         "% and prevalence of disease as ",
                         p,
                         " with marginal error does not exceed from ",
                         prec * 100,
                         "% with ", (1 - alp) * 100, "% confidence level.\n")
  out <- list(Sample_Size = n,stringi::stri_pad(stringi::stri_wrap(s), side='both'))
  return(out)
}
est.sp1(p=0.3, sp=0.01, prec=0.70, alp=-0.05)

