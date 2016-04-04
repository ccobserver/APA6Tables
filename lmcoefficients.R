#' APA Regression Table
#'
#' APA Regression tables are created from data frames using lmCoefficients
#' @param m A model of class `regression`.
#' @param digits Number of digits after the decimal place.
#' @param caption A caption for the regression table.
#' @return APA Regression Table
#' @export
#' @importFrom broom tidy
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom magrittr inset
#' @importFrom magrittr extract
#' @importFrom magrittr set_rownames
#' @importFrom magrittr set_colnames
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom pander pander
#' @importFrom lm.beta lm.beta
#' @importFrom stats coef
#' @importFrom stats confint


# d <- psych::galton
# m <- lm(child ~ parent,data = d)

lmCoefficients <- function(m, caption = "Regression Coefficients", digits = 2) {
  beta <- lm.beta(m) %>% coef %>% inset(1,NA)
  term <- termCleaner(tidy(m) %$% term)
  ci <- confint(m) %>%
    formatC(digits = digits,format = "f") %>%
    apply(1,paste,collapse = ",&nbsp;") %>%
    paste0("[",.,"]") %>%
    as.vector()
  m %>% tidy %>%
    mutate(p.value = pvalueAPA(p.value),
           beta = beta,
           ci = ci) %>%
    select(beta,estimate,std.error,statistic,p.value,ci) %>%
    set_colnames(c("$\\beta$",
                   "$b$",
                   "$s_b$",
                   "$t$",
                   "$p$",
                   "$95\\%~CI$")) %>%
    set_rownames(term) %>%
    pander(caption = caption,
           round = digits,
           split.tables = Inf,
           style = "rmarkdown",
           emphasize.rownames = FALSE,
           keep.trailing.zeros = TRUE,
           table.alignment.rownames = "left",
           missing = "",
           justify = "lcccccc")

}
