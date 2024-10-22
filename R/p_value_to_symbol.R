
p_value_to_symbol <- function(p, symbol) {
  if (p < 0.001) {
    return(paste0(symbol, symbol, symbol))
  }
  else if (p < 0.01) {
    return(paste0(symbol, symbol))
  }
  else if (p < 0.05) {
    return(paste0(symbol))
  }
  else {
    return(NA)
  }
}
