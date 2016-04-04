#' APA MANOVA Table
#'
#' APA MANOVA tables are created from data frames using aovTable
#' @param m A model of class `maov`.
#' @param caption A caption for the MANOVA table.
#' @param test Type of hypothesis test used.
#' @return APA MANOVA table
#' @export
#' @importFrom broom tidy
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom lm.beta lm.beta
#' @importFrom lsr etaSquared
#' @importFrom magrittr set_colnames
#' @importFrom magrittr %>%
#' @importFrom pander pander
#' @importFrom tidyr gather

manovaTable <- function(m, caption = "MANOVA Table", test = "Pillai"){
  etaSquared <- heplots::etasq(m,test = test) %>% as.data.frame()
  rownames(etaSquared) <- NULL
  testName <- switch(test,
                     Pillai = "Pillai's Trace",
                     Wilks = "Wilks' $\\Lambda$",
                     `Hotelling-Lawley` = "Hotelling-Lawley's Trace",
                     Roy = "Roy's Largest Root")
  m %>% manova %>%
    tidy(test = test) %>%
    mutate(p.value = pvalueAPA(p.value),
           term = gsub(x = term,
                       pattern = ":",
                       replacement = " $\\\\times$ ") ) %>%
    cbind(etaSquared) %>%
    set_colnames(c("Factor",
                   "df",
                   testName,
                   "$F$",
                   "$df_1$",
                   "$df_2$",
                   "$p$",
                   "$\\eta^2$")) %>%
    pander(caption = caption,
           round = 2,
           split.tables = Inf,
           style = "rmarkdown",
           emphasize.rownames = FALSE,
           keep.trailing.zeros = TRUE,
           table.alignment.rownames = "left",
           missing = "",
           justify = "lccccccc")
}


manovaUnivariateTable <- function(m, caption = "Univariate Analyses"){
  beta <- lm.beta(m)$standardized.coefficients %>% as.data.frame()
  beta <- cbind(term = rownames(beta),beta)
  rownames(beta) <- NULL
  beta <- beta %>% gather(key = "DV","beta",-term)
  beta[beta$term == "(Intercept)","beta"] <- NA
  m %>% tidy %>%
    mutate(response = ifelse(response == Lag(response),
                             NA,
                             response),
           p.value = pvalueAPA(p.value),
           term = gsub(x = term,
                       pattern = ":",
                       replacement = "LatexMultiply"))  %>%
    mutate(term = gsub(x = term,
                       pattern = "[[:punct:]]",
                       replacement = "")) %>%
    mutate(term = gsub(x = term,
                       pattern = "LatexMultiply",
                       replacement = " $\\\\times$ ")) %>%
    cbind(beta[,"beta",drop = F]) %>%
    select(response,term,estimate,std.error,beta,statistic,p.value) %>%
    set_colnames(c("Outcome",
                   "Predictor",
                   "$b$",
                   "$s_b$",
                   "$\\beta$",
                   "$t$",
                   "$p$")) %>%
    pander(caption = caption,
           round = 2,
           split.tables = Inf,
           style = "rmarkdown",
           emphasize.rownames = FALSE,
           keep.trailing.zeros = TRUE,
           table.alignment.rownames = "left",
           missing = "",
           justify = "llccccc")
}
