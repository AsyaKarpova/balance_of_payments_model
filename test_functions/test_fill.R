### THERE JUST THE PARTS OF THE CODE I SAVE FOR THE FUTURE IMPRUVEMENTS 




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







all_vars = import('data/vars_for_model.csv') %>% mutate(date = yearmonth(date))
long_table = all_vars %>% 
  pivot_longer(-date, names_to = 'series', values_to = 'value') %>% drop_na()

data =
  long_table %>%
  group_by(series) %>%
  mutate(`_1` = dplyr::lag(value, n = 1, default = NA)) 
dl = data %>% pivot_longer(c(value, `_1`), values_to = 'value', names_to = 'lag')
dl %>% head()
dl1 = dl %>% mutate(lag = ifelse(lag == 'value', '', lag))
dl1 %>% pivot_wider(names_from = c(series, lag), names_sep = '')
