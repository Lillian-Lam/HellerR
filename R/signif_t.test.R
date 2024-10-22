#' Gives a list of all the pairs that are statistically significant in a t.test.
#'
#' @param df Dataframe.
#' @param x  A string. The name of the column that represents the independnent variable.
#' @param y A string. The name of the column that represents the indepednent variable.
#' @param group_order A vector. The exact order that the groups/bars are wanted.
#' @return A list pf all significant p_values.

signif_t.test <- function(df,x, y, group_order, signif_level=F, symbol="*"){
  group_names<- unique(df[[x]])
  all_pairs<-all_group_pairs(group_names)
  only_signif <- list()
  for (i in 1:length(all_pairs)){
    #Adds to only_signif list if the p_value is greater than equal to 0.05.
    if(t.test(df[which(df[[x]]==all_pairs[[i]][[1]]),][[y]],
              df[which(df[[x]]==all_pairs[[i]][[2]]),][[y]])$p.value <= 0.05){
    #Shows the level of significance via symbols
      if (signif_level) {
        p_value <- t.test(df[which(df[[x]] == all_pairs[[i]][[1]]), ][[y]], df[which(df[[x]] == all_pairs[[i]][[2]]), ][[y]])$p.value
        pair_string <- paste(all_pairs[[i]][1], all_pairs[[i]][2], sep = " vs ")
        formatted_string <- paste(pair_string, ":", p_value_to_symbol(p_value, symbol))
        only_signif <- append(only_signif, formatted_string)
      }
      else {
        only_signif <- append(only_signif, all_pairs[i])
      }
    }
  }
  return(only_signif)
}
