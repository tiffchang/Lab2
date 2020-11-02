#' Statistics function
#'
#' @param df a dataframe
#' @param s a calculate method (mean, median, sd)
#'
#' @return a table contains mean or median or sd of DRG codes
#' @import dplyr
#' @import readr
#' @export
#'
#' @examples
#' data <- load("Lab2/Lab2/data/DRG_data.csv")
#'
#' stat_function(data, "mean")
#'
stat_function <- function(df, s){
  names(df) <- gsub('\\.', ' ', names(df))  ##remove "." and replace it with space
  data2<-df %>%
    mutate(`DRG Number` = substr(df $ `DRG Definition`, 0, 3)) %>%  ##only select the number part of DRG codes
    group_by(`DRG Number`) %>%  ##group by DRG Number
    summarise(mean = mean(`Average Medicare Payments`),  ##do the statistical calculation
              sd = sd(`Average Medicare Payments`),
              median = median(`Average Medicare Payments`))
  data2 %>% select(`DRG Number`, s)
}
