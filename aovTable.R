#' APA ANOVA Table
#'
#' APA ANOVA tables are created from data frames using aovTable
#' @param m A model of class `aov`.
#' @param caption A caption for the ANOVA table.
#' @param partial_etasq Include partial eta-squared values.
#' @param eta Include eta-squared values.
#' @param widthProportion Adjusts the width of the table (1.00 equals the width of the page; less than 1.00 makes it smaller than the width of the page).
#' @return APA ANOVA table
#' @export
#' @importFrom broom tidy
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom magrittr %>%
#' @importFrom pander pander
#' @importFrom pander panderOptions



aovTable <- function(m,
                     caption = "ANOVA Table",
                     partial_etasq = TRUE,
                     eta = FALSE,
                     widthProportion = 80
                     ){
  #require(broom) # For tidying results
  #require(lsr) # For effect sizes
  #require(pander) # For table output
  #require(dplyr) # For aggregating data

  # Tidy model
  tm <- tidy(anova((m)))
  # Create totals
  tr <- tm %>%
    select(df:sumsq) %>%
    summarize(df = sum(df),
              sumsq = sum(sumsq)) %>%
    mutate(statistic = NA,
           p.value = NA,
           term = "Total",
           meansq = sumsq/df)
  tm <- rbind(tm,tr)
  # Calculate and format effect sizes
  es <- lsr::etaSquared(m)
  es <- as.data.frame(es)  %>%
    mutate(term = rownames(es))
  tm <- tm %>% left_join(es, "term")
  # Format table
  ftm <- tm %>% select(-term,-df) %>%
    apply(FUN = formatC,
          MARGIN = 2,
          digits = 2,
          format = "f")
  ftm[ftm == " NA"] <- ""
  ftm <- tm %>% select(df) %>% cbind(ftm)
  ftm$p.value <- pvalueAPA(tm$p.value)
  # Table headers
  rownames(ftm) <-  InteractionReplace(tm$term)
  rownames(ftm)[nrow(tm) - 1] <- "Error"
  cnames <- c("df","SS","MS","F","p")
  if (eta)
    cnames[length(cnames) + 1] <- "\\eta^2" else
      ftm <- ftm %>% select(-eta.sq)
  if (partial_etasq)
    cnames[length(cnames) + 1] <- "\\eta_p^2" else
      ftm <- ftm %>% select(-eta.sq.part)
  colnames(ftm) <- paste0("$",cnames,"$")
  justifyString = paste0("l",
                         paste0(rep("r",ncol(ftm)),
                                collapse = ""),
                         collapse = "")
  # Output table
  return(pander(ftm,
         missing = "",
         round = 2,
         keep.trailing.zeros = TRUE,
         emphasize.rownames = FALSE,
         justify = justifyString,
         caption = caption,
         split.cells = c("50%", "10%", "10%", "10%", "10%", "10%")
         ))
}
