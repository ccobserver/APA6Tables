#' Sequential (i.e., Hierarchical) Regression Table - Beta and R-Squared
#'
#' @param m An object of class lm.
#' @param digits Number of digits provided.
#' @param caption A character string denoting the title of the regression table.
#' @export

simple_regression_table <- function(m, digits = 2, caption = "Standardized Regression Coefficients and Change in R-Squared") {
  library(broom)
  library(dplyr)
  library(magrittr)
  library(lm.beta)
  library(pander)

  # Standardized coefficients
  beta <- m %>% lm.beta() %>% coef

  # Change in R2
  # Code adapted from rockchalk::getDeltaRsquare
  modeldrop1 <- drop1(m)
  RSS <- modeldrop1[1, "RSS"]
  deltaSS <- modeldrop1[, "Sum of Sq"]
  SST = sum((m$model[, 1] - mean(m$model[, 1])) ^ 2)
  deltaRsquare <- deltaSS/SST
  names(deltaRsquare) <- row.names(modeldrop1)
  omit <- is.na(deltaRsquare)
  deltaRsquare <- deltaRsquare[-omit]

  # Table
  cbind(beta[-1],deltaRsquare) %>%
    set_colnames(c("$\\beta$","$\\Delta R^2$")) %>%
    set_rownames(deltaRsquare %>%
                   names %>%
                   termCleaner()) %>%
    pander(caption = caption,
           round = digits,
           split.tables = Inf,
           style = "rmarkdown",
           emphasize.rownames = FALSE,
           keep.trailing.zeros = TRUE,
           table.alignment.rownames = "left",
           missing = "",
           justify = "lrr")
}
