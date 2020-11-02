#' DRG Boxplot
#'
#' @param df a dataframe
#' @param vary a payment name you want to draw a boxplo
#'
#' @return A boxplot with payments in different DRG code
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#' data <- load("Lab2/Lab2/data/DRG_data.csv")
#'
#' draw_boxplot(data, "Average Total Payments")
#'
draw_boxplot <- function(df, vary){
  options(scipen = 999)  ##remove scientific notation
  df <- df %>%
    mutate(DRG.Num = substr(df$DRG.Definition,0,3))  ##only select the number part of DRG codes
  names(df) <- gsub('\\.', ' ', names(df))  ##remove "." and replace it with space
  plot<- ggplot(df, aes(x = `DRG Num`, y = get(vary)))+
    scale_y_continuous(trans = "log10")+  ##change y scale
    xlab("DRG codes")+  ##change xlab name
    ylab("Payment $(log base 10)")+   ##change ylab name
    ggtitle(paste0("Box plot of ", as.name(vary)))+  ##add title for the plot
    theme(axis.text.x = element_text(size = 6,angle = 90,hjust = 1))+  ##adjust x axis text
    geom_boxplot()  ##draw a boxplot
  plot
}
