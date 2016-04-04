#' Regression Analysis Summary Table
#'
#' @param m An object of class lm.
#' @param caption A character string denoting the title of the regression table.
#' @param digits Number of digits after the decimal place.
#' @param level Confidence Level desired.
#'
#' @importFrom broom tidy
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom pander pander
#' @importFrom lm.beta lm.beta
#'
#' @export

regression_analysis_summary_table_withCI <- function(m, caption = "Regression Analysis Summary", digits = 2, level = .95) {

  # load packages
  # library(broom)
  # library(magrittr)
  # library(lm.beta)
  # library(pander)

  # unstandardized estimates
  estimates <- m %>% tidy %$% estimate %>% formatC(digits = digits, format = "f")

  # SE of B
  # se <- m %>% tidy %$% std.error %>% formatC(digits = digits, format = "f")

  # CI
  ci <- confint(m1, level = level)
  alpha <- 1 - level

  # standardized estimates
  betas <- m %>% lm.beta %$% standardized.coefficients %>% formatC(digits = digits, format = "f")
  betas[1] <- NA

  # t-values
  tstat <- m %>% tidy %$% statistic %>% formatC(digits = digits, format = "f")

  # p-values
  pval <- m %>% tidy %$% p.value %>% pvalueAPA

  # combine vectors
  values_table <- data.frame(estimates, ci, betas, tstat, pval)
  # str(values_table)

  # set column names
  colnames(values_table) <- c("$b$", "$`r alpha / 2`\\% CI$", "$`r 1-alpha \ 2`\\% CI$", "$\\beta$", "$t$", "$p$")

  # set row names
  rownames(values_table) <- values_table %>% rownames %>% termCleaner()

  # note values
  rsquared <- m1 %>% summary %$% r.squared %>% formatC(digits = 2, format = "f")
  n <- m1 %>% summary %$% fstatistic[3]
  p <- anova(m1)$'Pr(>F)'[1] %>% pvalueAPA

  # note - put equals sign if not p not "<.001"
  ifelse(p <=.001,
         note <- paste0("Note. $R^2$ = ", rsquared, " (N = ", n, ", p ", p, ")"),
         note <- paste0("Note. $R^2$ = ", rsquared, " (N = ", n, ", p = ", p, ")"))

  # pander table
  values_table %>% pander(
    caption = paste0(caption, "\n\n", note, collapse = "\n"),
    round = digits,
    split.tables = Inf,
    style = "rmarkdown",
    emphasize.rownames = FALSE,
    table.alignment.rownames = "left",
    missing = "",
    justify = "lrrrrrr"
  )
}

