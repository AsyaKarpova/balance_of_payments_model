

fill_recursive = function(first_values = 0, add_term = rep(0, 10), coefs = 1) {
  len_filter = length(coefs)
  init = rev(tail(first_values, len_filter))
  vector = stats::filter(x = add_term, filter = coefs, method = "recursive",
                         init = init)
  vector = c(first_values, vector)
  return(vector)
}

?stats::filter


vector = stats::filter(x = rep(0, 10), filter = c(2, 1), method = "recursive", 
                       init = c(1, 4))
vector


fill_recursive(first_values = c(8, 7), add_term = rep(1, 10), coefs = c(0.5, 0.3))
