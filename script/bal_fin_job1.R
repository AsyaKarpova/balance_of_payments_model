# library(forecast)
library(rio)
library(RcppRoll) # for rolling window operations
library(MLmetrics)

library(GenSA) # GenSA optimizator
library(nloptr) # stogo optimizator

library(stats)
library(Metrics)
library(tidyverse)
library(tsibble)
library(fable)
library(lubridate)
library(skimr)
library(dtw)


all_vars = import('data/data_month1.xlsx') %>%
  mutate(date = yearmonth(date)) %>% as_tsibble()


all_vars = all_vars %>% mutate(em_index_ratio = em_index / lag(em_index, 1))
all_vars = all_vars %>% filter(year(date) > 2011, year(date) < 2020)
source('script/functions.R')
#all_vars = all_vars %>%na.omit()


all_vars_quater = import('data/data_quarter_new.xlsx')
all_vars_quater = mutate_at(all_vars_quater, vars(-date), ~ . / 1000)

all_vars_quater = mutate(all_vars_quater, date = yearquarter(date)) %>%
  as_tsibble() %>% filter(year(date) > 2011)

## март 2013 - большие значений aq_obl и aq_assets


cors = cor(all_vars$aq_assets, select(as_tibble(all_vars), -date, -dum01:-dum12), use = 'complete.obs') %>%
  as.data.frame(row.names = 'val') %>% abs()


cors_obl = cor(all_vars$aq_obl, select(as_tibble(all_vars), -date, -dum01:-dum12), use = 'complete.obs') %>%
  as.data.frame(row.names = 'val') %>% abs()

is_high = (cors > 0.3) %>% as.data.frame
high_cor_assets = which(unlist(transpose(is_high)))
names(high_cor_assets)

is_high_obl = (cors_obl > 0.3) %>% as.data.frame
high_cor_obl = which(unlist(transpose(is_high_obl)))

all_vars = create_tibble(all_vars)
all_vars$aq_assets
all_vars$r_cur_purch
X_aq_assets = tibble(const = 1,
                     r_cur_account = all_vars$r_cur_account,
                     rate_repo = all_vars$rate_repo/all_vars$rate_10tr,
                     rub_usd = all_vars$rub_usd,
                     cur_purch = all_vars$r_cur_purch,
                     rub_usd_var = all_vars$vcor*rub_usd/lag(rub_usd, 1),
                     em_index_ratio = all_vars$em_index_ratio,
                     dif_usd_eur_ratio = all_vars$dif_usd_eur_ratio,
                     dum_1114 = all_vars$dum_1114)



all_vars = mutate(all_vars, rub_usd_var = all_vars$vcor*all_vars$rub_usd/lag(rub_usd, 1))
names(all_vars)
all_vars %>% select(aq_assets,aq_obl) %>%
  pivot_longer(cols = c("aq_assets", "aq_obl")) %>%
  as_tsibble(index=date, key=name) %>%
  autoplot(value)

R_assets = all_vars %>%
  select(aq_assets) %>%
  rename('r_real' = 'aq_assets')

R_assets_quarter = all_vars_quater %>%
  select(aq_assets) %>%
  rename('r_real' = 'aq_assets') %>%
  na.omit()


X = X_aq_assets
R = R_assets
par = rep(0.6,21)
par_0 = par
nrow(X)
make_pred_assets = function(par, X){
  dummies = rep(c(par[10:11], 1, par[12:20]), nrow(X)/12)
  coef = as.matrix(X) %*% par[1:9]
  add_term = as.vector(coef * dummies)
  hat_aq_assets = rep(NA, length(add_term))
  hat_aq_assets = fill_recursive(add_term = tail(add_term, -1), coefs = par[21])
  hat_aq_assets_quarter = roll_sum(hat_aq_assets, n = 3, by = 3) # рассчет квартальной выручки
  return(list(hat_aq_assets = hat_aq_assets,
              hat_aq_assets_quarter = hat_aq_assets_quarter))
}


error_opt_assets = function(par, X, R, R_quarter){
  frcst = make_pred_assets(par, X)
  pse0(frcst$hat_aq_assets[2:60], R$r_real[2:60])
  error = pse_abs(pred = frcst$hat_aq_assets[2:60], pred_quarter = frcst$hat_aq_assets_quarter[2:20],
                  real = R$r_real[2:60], real_quarter = R_quarter$r_real[2:20])
error
  return(error)
}

error_opt_assets(par_0, X = X_aq_assets, R = R_assets, R_quarter = R_assets_quarter)


result_sa_aq_assets = GenSA(lower = rep(-0.5, length(par_0)),
                            upper = rep(1.9, length(par_0)),
                            fn = error_opt_assets,
                            X = X_aq_assets, R = R_assets, R_quarter = R_assets_quarter,
                            control = list(verbose = TRUE, max.time = 60))

par_aq_assets = result_sa_aq_assets$par
par_aq_assets = import('par_aq_assets.Rds')
pred_res_assets = make_pred_assets(par_aq_assets, X_aq_assets)

tsib2plot = mutate(all_vars, model = pred_res_assets$hat_aq_assets)
tsib2plot %>% select(model, aq_assets) %>%
  pivot_longer(cols = c("model", "aq_assets")) %>%
  as_tsibble(index=date, key=name) %>%
  autoplot(value)





mape(all_vars$aq_assets[2:60], pred_res_assets$hat_aq_assets[2:60])







cors_obl = cor(all_vars$aq_obl, select(as_tibble(all_vars), -date, -dum01:-dum12), use = 'complete.obs') %>%
  as.data.frame(row.names = 'val') %>% abs()

is_high_obl = (cors_obl > 0.3) %>% as.data.frame
high_cor_obl = which(unlist(transpose(is_high_obl)))
names(high_cor_obl)
all_vars = create_tibble(all_vars)

X_aq_obl = tibble(const = 1,
                     r_cur_account = all_vars$r_cur_account,
                     rate_repo = all_vars$rate_repo/all_vars$rate_10tr,
                     rub_usd = all_vars$rub_usd,
                     cur_purch = all_vars$r_cur_purch,
                     rub_usd_var = all_vars$vcor*rub_usd/lag(rub_usd, 1),
                     em_index_ratio = all_vars$em_index_ratio,
                     dif_usd_eur_ratio = all_vars$dif_usd_eur_ratio,
                     dum_1114 = all_vars$dum_1114)

ncol(X_aq_obl)
R_obl = all_vars %>%
  select(aq_obl) %>%
  rename('r_real' = 'aq_obl')

R_obl_quarter = all_vars_quater %>%
  select(aq_obl) %>%
  rename('r_real' = 'aq_obl') %>%
  na.omit()

ncol(X)
par = rep(0.6, 20)
X = X_aq_obl
ncol(X_aq_obl)
make_pred_obl = function(par, X){
  dummies = rep(c(par[10:11], 1, par[12:20]), nrow(X)/12)
  coef = as.matrix(X) %*% par[1:9]
  hat_aq_obl = as.vector(coef * dummies)
  #hat_aq_assets = fill_recursive(add_term = add_term, coefs = par[16])
  hat_aq_obl_quarter = roll_sum(hat_aq_obl, n = 3, by = 3) # рассчет квартальной выручки
  return(list(hat_aq_obl = hat_aq_obl,
              hat_aq_obl_quarter = hat_aq_obl_quarter))
}
X = X_aq_obl
R = R_obl
R_quarter = R_obl_quarter
as.matrix(X_aq_obl) %*% par[1:9]
error_opt_obl = function(par, X, R, R_quarter){
  frcst = make_pred_obl(par, X)
  pse0(frcst$hat_aq_obl_quarter[2:32], R_quarter$r_real[2:32])
  error = pse_abs(pred = frcst$hat_aq_obl[2:96], pred_quarter = frcst$hat_aq_obl_quarter[2:32],
                  real = R$r_real[2:96], real_quarter = R_quarter$r_real[2:32])
error

  return(error)
}



ncol(X_aq_obl)
par_0 = par
error_opt_obl(par_0, X = X_aq_obl, R = R_obl, R_quarter = R_obl_quarter)


result_sa_obl = GenSA(lower = rep(-0.5, length(par_0)),
                      upper = rep(1.9, length(par_0)),
                      fn = error_opt_obl,
                      X = X_aq_obl, R = R_obl, R_quarter = R_obl_quarter,
                      control = list(verbose = TRUE, max.time = 300))


par_aq_obl = result_sa_obl$par
par_aq_obl = import('par_aq_obl.Rds')
pred_res_obl = make_pred_obl(par_aq_obl, X_aq_obl)

tsib2plot2 = mutate(all_vars, model = pred_res_obl$hat_aq_obl)
autoplot(tsib2plot2, vars(aq_obl, model))
tsib2plot2 %>% select(model, aq_obl) %>%
  pivot_longer(cols = c("model", "aq_obl")) %>%
  as_tsibble(index=date, key=name) %>%
  autoplot(value)

all_vars %>% select(rub_usd, ) %>%
  pivot_longer(cols = c("model", "aq_obl")) %>%
  as_tsibble(index=date, key=name) %>%
  autoplot(value)


r_bal_fin_hat = pred_res_assets$hat_aq_assets - pred_res_obl$hat_aq_obl
tsib2plot3 = mutate(all_vars, model = r_bal_fin_hat)
autoplot(tsib2plot3, vars(r_bal_fin, model))
tsib2plot %>% select(model, r_bal_fin) %>%
  pivot_longer(cols = c("model", "r_bal_fin")) %>%
  as_tsibble(index=date, key=name) %>%
  autoplot(value)

mse(all_vars$r_bal_fin, r_bal_fin_hat)
