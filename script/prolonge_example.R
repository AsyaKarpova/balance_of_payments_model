library(tidyverse)
library(rlang)

prolonge_data = function(data, var_name, var_pred){
  data_prolonge = data %>% mutate(!!(var_name) := ifelse(is.na(!!sym(var_name)) == TRUE,
                                            var_pred, !!sym(var_name)))
  return(data_prolonge)
}

data = tibble(x = c(1,1,1,1,NA), y = c(2,3,4,5,6))
var_old = data$x
var_pred = c(1,1,1,1,4)
prolonge_data(data, 'x', var_pred)

