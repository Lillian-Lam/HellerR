#' Makes all possible group-pairing in a specific order. Made to place in order of shortest to longest bar in ggsignif.
#'
#' @param group_names A list or vector. The names of the groups to make all pairs.
#' @return All possible group pairings.
#' @examples
#' all_group_pairs(c(1,2,3,4)) Will return: list(c("1" "2"), c("2" "3"), c("3" "4"), c("1" "3"), c("2" "4"), c("1" "4"))
all_group_pairs<- function(group_names){
# creates a level of all the bar names in a bar plot
  level<- c(levels(factor(group_names)))
#empty list that will fill of all possible pairs of t-test comparisions
  comparison<- list()
#
  for (gap in 1:(length(level)-1)) {
    for (i in 1:(length(level)-gap)){
      comparison<-append(comparison, list(c(level[i], level[i+gap])))
    }
  }

return(comparison)
}

