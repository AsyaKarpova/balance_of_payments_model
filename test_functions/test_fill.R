### THERE JUST THE PARTS OF THE CODE I SAVE FOR THE FUTURE IMPRUVEMENTS




fill_recursive = function(first_values = 0, add_term = rep(0, 10), coefs = 1,
                          multiplier = rep(1, length(add_term))) {
  add_term = unlist(add_term)
  multiplier = unlist(multiplier)
  nsteps = length(add_term)
  vector = c(first_values, rep(NA, nsteps))
  for (i in 1:nsteps) {
    recur = sum(rev(coefs) * vector[(i + length(first_values) - length(coefs)):(i + length(first_values) - 1)])
    vector[i + length(first_values)] = multiplier[i] * (add_term[i] + recur)
  }
  return(vector)
}

vector = stats::filter(x = rep(0, 10), filter = c(2, 1), method = "recursive",
                       init = c(1, 4))
vector


fill_recursive(first_values = c(8, 7), add_term = rep(1, 10), coefs = c(0.5, 0.3))


fill_recursive(first_values = 4, multiplier = rep(1, 10), add_term = rep(1, 10), coefs = 0.5)





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
