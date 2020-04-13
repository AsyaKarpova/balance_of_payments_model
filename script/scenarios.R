library(forecast)
library(rio)
library(RcppRoll) # for rolling window operations
library(MLmetrics)
library(GenSA)
library(stats)
library(Metrics)
library(tidyverse)
library(tsibble)
library(zoo)
library(fable)
library(lubridate)


data = import('data/data_month.xlsx') %>% mutate(date = yearmonth(date))
par_model = import('script/gensa_par.Rds')
source('script/functions.R')

### exogenous vars for scenarios

exog = data %>% select(date, brent, gas_lng, gas_europe, usd_eur,
                           v_prod_oil, v_prod_op, v_prod_gas, 
                           n_y, n_c, n_j, n_g, n_ds,
                           rate_repo, rate_10tr, r_price_cur_purch,
                           r_dum_cur_purch, em_index, dum01:dum12, vcor)
                          
long_exog = exog %>% 
  pivot_longer(-date, names_to = 'series', values_to = 'value') %>% drop_na()

exog_tsb = long_exog %>% as_tsibble(index = date, key = series)

#long_exog %>% group_by(series) %>% top_n(date, n=1)

# models

series_models = exog_tsb %>% 
  model(
    snaive = SNAIVE(value ~ lag('year'))
    #ets = ETS(value), 
    #arima = ARIMA(value)
  )

exog_forecasts = series_models %>% 
  forecast(h = "2 year")

exog_forecasts %>%
  filter(series %in% c('brent', 'gas_europe', 'gas_lng')) %>%
  autoplot(exog_tsb, level = NULL) +
  xlab("Year") + ylab("price")

exog_forecasts %>%
  filter(series %in% c('n_j', 'n_c', 'n_ds', 'n_g', 'n_y')) %>%
  autoplot(exog_tsb, level = NULL) +
  xlab("Year") + ylab("price")

exog_forecasts_naive = exog_forecasts %>% filter(.model == 'snaive') %>% as_tsibble() %>%
         select(-`.distribution`, -`.model`)


# history and prediction
exog_full = full_join(exog_forecasts_naive, exog_tsb) #%>% filter(year(date) < 2021) %>% spread(series, value)

# all variables in long format
all_vars_tsb = data %>% 
  pivot_longer(-date, names_to = 'series', values_to = 'value') %>% 
  drop_na() %>% 
  as_tsibble(index = date, key = series)

all_vars = full_join(all_vars_tsb, exog_full) %>% 
  filter(year(date) < 2021) %>% spread(series, value)



all_vars[is.na(all_vars$dum_1114),]['dum_1114'] = 0
all_vars[is.na(all_vars$dum_2012),]['dum_2012'] = 0
all_vars = all_vars%>% mutate(r_price_cur_purch = ifelse(r_dum_cur_purch == 0, 0, r_price_cur_purch))
all_vars = create_tibble(all_vars)

### parameters from gensa
par_oil = par_model$par_oil
par_rub_usd = par_model$par_rub_usd
par_cur_purch = par_model$par_cur_purch

## cur purchase
X_cur = tibble(r_price_cur_purch = all_vars$r_price_cur_purch, 
               brent = all_vars$brent, 
               brent_1 = all_vars$brent_1, 
               brent_2 = all_vars$brent_2,
               r_dum_cur_purch = all_vars$r_dum_cur_purch)


R_cur = all_vars %>% 
  select(r_cur_purch) %>%
  rename('r_real' = 'r_cur_purch')

pred_cur_purch = make_pred_cur_purch(par_cur_purch, X_cur)
r_hat_cur_purch = pred_cur_purch$r_hat_cur_purch 


all_vars = mutate(all_vars, r_hat_cur_purch = r_hat_cur_purch)
all_vars = all_vars %>% mutate(r_cur_purch = ifelse(is.na(r_cur_purch) == TRUE, r_hat_cur_purch, r_cur_purch))
autoplot(ts.union(real_data = ts(all_vars$r_cur_purch, start = c(2006, 1), freq = 12),  
                  model = ts(r_hat_cur_purch, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Currency purchase')


a
# rub_usd 

X_rub_usd = tibble(const = 1, 
                   dif_brent_ratio = all_vars$dif_brent_ratio,
                   dif_brent_ratio_1114 = all_vars$dum_1114 * dif_brent_ratio,
                   dif_brent_ratio_cor = (all_vars$vcor*dif_brent_ratio)/all_vars$rub_usd_1, 
                   dif_r_1114 = all_vars$dif_r * all_vars$dum_1114, 
                   r_cur_purch = all_vars$r_cur_purch,
                   r_cur_purch_1 = lag(all_vars$r_cur_purch, 1),
                   dif_usd_eur_ratio = all_vars$dif_usd_eur_ratio,
                   em_index_ratio = all_vars$dif_em_index_ratio,
                   em_index_ratio_1114 = em_index_ratio * all_vars$dum_1114,
                   dif_usd_rub_ratio = all_vars$dif_usd_rub_ratio)

View(X_rub_usd)
R_rub_usd = as_tibble(all_vars) %>% 
  select(rub_usd) %>%
  rename('r_real' = 'rub_usd') %>% as.data.frame()
end = nrow(X_rub_usd)

end_2018 = nrow(X_rub_usd)


R_rub_usd_quarter = roll_mean(R_rub_usd$r_real, n = 3, by = 3)


mask = matrix(0, 8, end_2018)
mask[1,] = c(rep(1,3), rep(0, 23),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[2,] = c(rep(1,3), rep(0, 2),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[3,] = c(rep(1,3), rep(0, 5),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[4,] = c(rep(1,3), rep(0, 8),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[5,] = c(rep(1,3),rep(0, 11),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[6,] = c(rep(1,3),rep(0, 14),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[7,] = c(rep(1,3),rep(0, 17),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]
mask[8,] = c(rep(1,3),rep(0, 20),1,rep(c(rep(0,23), 1), times=7))[1:end_2018]

make_pred_rub_usd = function(par, X, R, end = nrow(X), mask){
  hat_rub_usd_ratio = rep(NaN, end)
  hat_rub_usd_ratio[2:3] = X$dif_usd_rub_ratio[2:3] 
  
  add_term = as.matrix(X[4:end,1:10]) %*% c(par[1:10])
  
  hat_rub_usd_ratio[3:end] = fill_recursive(first_values = hat_rub_usd_ratio[3], add_term = add_term,
                                            coefs = par[11]) # from 2006m04 to 2018m12
  
  hat_rub_usd = matrix(NaN, 8, end)
  hat_rub_usd[1:8, 1:3] = matrix(R[1:3,1], ncol = 3, nrow = 8, byrow = TRUE) 
  print(hat_rub_usd)
  vector = 1 + hat_rub_usd_ratio
  real_values = matrix(R[1:end,1], ncol = end, nrow = 8, byrow = TRUE)
  print(nrow(hat_rub_usd))
  for (i in 1:nrow(hat_rub_usd)) {
    for (j in 4:ncol(hat_rub_usd)) {
      if (mask[i,j] == 0){
        hat_rub_usd[i,j] = hat_rub_usd[i, j-1] * vector[j]}
      else{
        hat_rub_usd[i,j] = real_values[i,j]
        
      }
    }
  }
  
  hat_rub_usd_final = colMeans(hat_rub_usd)
  hat_rub_usd_quarter = roll_mean(hat_rub_usd_final, n = 3, by = 3)
  
  return(list(hat_rub_usd_final = hat_rub_usd_final, 
              hat_rub_usd_quarter = hat_rub_usd_quarter,
              hat_rub_usd = hat_rub_usd))
}


pred_rub_usd = make_pred_rub_usd(par_rub_usd, X_rub_usd,R_rub_usd,mask=mask)
hat_rub_usd_final = pred_rub_usd$hat_rub_usd_final

autoplot(ts.union(real_data = ts(all_vars$rub_usd, start = c(2006, 1), freq = 12),  
                  model = ts(hat_rub_usd_final, start = c(2006, 1), freq = 12))) + ylab('exchange rate') + xlab('') + ggtitle('Exchange rate (rub/usd)')

