#' Interaction term replace
#'
#' Replaces ":" in interaction terms with the LaTeX multiply symbol
#' @param term A vector of effect names
#' @export
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%

termCleaner <- function(term) {
  x <- term %>% gsub(pattern = "(\\w)(\\w*)",
                replacement = "\\U\\1\\E\\2",
                perl = TRUE) %>%
    gsub(pattern = ":",
         replacement = "$\\\\times$")  %>%
    gsub(pattern = "I\\(",
         replacement = "") %>%
    gsub(pattern = "\\(",
         replacement = "") %>%
    gsub(pattern = "\\)",
         replacement = "") %>%
    gsub(pattern = "\\(Intercept\\)",
         replacement = "Intercept")
    paste0(x,ifelse(regexpr("\\^",x) > 0, "^",""))
}

