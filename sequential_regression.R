#' Sequential (i.e., Hierarchical) Regression Table
#'
#' @param X An object of class lm
#' @param ... Objects of class lm
#' @param caption A character string denoting the title of the regression table
#' @param alpha A single number denoting the significance level
#' @param standardized A boolean value indicating whether the coefficients are standardized or unstandardized
#' @param vNames A character vector of predictor names
#' @param digits Number of digits after the decimal place.
#' @export

sequential_regression_table <- function(X, ..., caption = "Sequential Regression", alpha = 0.05, standardized = FALSE, vNames = NULL, digits = 2) {

  # require(broom)
  # require(tidyr)
  # require(plyr)
  # require(dplyr)
  # require(lm.beta)
  # require(magrittr)

  # Put models in a list
  m <- list(X, ...)
  # Put coefficients in a data.frame
  b <- ldply(m, tidy)
  # Function for extracting standardized coefficients
  coefbeta <- function(mm)
    as.data.frame(coef(lm.beta(mm)))
  # Extract standardized coefficients
  beta <- llply(m, coefbeta) %>% ldply(data.frame)
  # Put standardized codfficients in table
  b$beta <- beta$coef.lm.beta.mm..
  # Number the models.
  b$model <- paste("$\\text{Model", ifelse(b$term == "(Intercept)", 1, 0) %>% cumsum,"}$")
  b$term <- factor(b$term,levels = b$term %>% unique)

  # p-values
  p <- b %>% dplyr::select(term, model, p.value) %>%
    spread(model, p.value) %>% dplyr::select(-term)


  # Create wide-format matrix
  if (standardized) {
    coefb <- b %>%
      dplyr::select(term, model, beta) %>%
      spread(model, beta)

    # Remove intercept row
    coefb <- coefb[-1,]
    p <- p[-1,]

    # Coefficient title row
    cr <- matrix(rep(NA, ncol(coefb) - 1), nrow = 1)
    rownames(cr) <- "$\\mathbf{\\text{Coefficients }} \\boldsymbol{\\beta}$"
  } else {
    coefb <- b %>%
      dplyr::select(term, model, estimate) %>%
      spread(model, estimate)
    # Coefficient title row
    cr <- matrix(rep(NA, ncol(coefb) - 1), nrow = 1)
    rownames(cr) <- "$\\mathbf{\\text{Coefficients }} \\boldsymbol{b}$"
  }

  # Variable names
  if (length(vNames) == 0) {
    rownames(coefb) <- termCleaner(coefb$term)
  } else {
    if (standardized) {
      rownames(coefb) <- vNames
    } else {
      rownames(coefb) <- c("Intercept",vNames)
    }
  }

  # Remove the term column
  coefb$term <- NULL

  # Format the table
  bt <- apply(coefb,
              2,
              formatC,
              digits = digits,
              format = "f") %>%
    gsub(x = ., replacement = "", pattern = "NA") %>%
    gsub(x = ., replacement = NA, pattern = " ")

  # Add bolding to significant coefficients
  bt <- ifelse(is.na(bt), NA, paste0("$", ifelse(p <= 0.05,"\\mathbf{",""),bt,ifelse(p <= alpha,"}",""), "$"))

  # Create summary statistics
  mg <- ldply(m, glance) %>% dplyr::select(sigma, r.squared) %>% mutate(dR2 = r.squared - lag(r.squared)) %>% apply(2, formatC, digits = digits, format = "f") %>% gsub(x = .,replacement = "", pattern = "NA") %>% gsub(x = .,replacement = NA, pattern = " ")

  # Differences in models
  ma <- anova(X, ...)

  # Merge summary statistics and differences
  mg <- cbind(mg,ma$F %>% formatC(digits, format = "f") %>% gsub(x = .,replacement = "", pattern = "NA") %>% gsub(x = .,replacement = NA, pattern = " "), ma$`Pr(>F)` %>% pvalueAPA) %>% set_colnames(c("sigma", "r.squared", "dR2", "Fstat", "pvalue"))
  # Make into LaTeX equations
  mg <- ifelse(is.na(mg), NA, paste0("$", mg, "$"))
  mg <- as.data.frame(mg)

  # Remove R change if models not nested
  mg$dR2[is.na(mg$pvalue)] <- NA

  # Remove leading zeroes
  mg$dR2 <- mg$dR2 %>% sub(pattern = "0.",replacement = ".", x = .)
  mg$r.squared <- mg$r.squared %>% sub(pattern = "0.",replacement = ".", x = .)

  # Add column names
  colnames(mg) <- c("$s_e$", "$R^2$", "$\\Delta R^2$", "$F$", "$p$")

  # Model title row
  br <- matrix(rep(NA, ncol(bt)), nrow = 1)
  rownames(br) <- "$\\mathbf{\\text{Model Statistics}}$"
  # caption <- paste0("Table 1 <br><br>", caption, collapse = "")
  # Bind in pander table
  rbind(cr,bt, br, t(mg)) %>%
    pander(caption = paste0(caption,"\n\nNote: Significant coefficients are bolded.", collapse = "\n"),
           missing = "",
           emphasize.rownames = F,
           digits = 2,
           split.tables = Inf,
           justify = c(paste0(c("l",
                                rep("r", ncol(bt))),
                              collapse = "")))
}




