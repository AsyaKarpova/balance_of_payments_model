library(forecast)
library(rio)
library(RcppRoll) # for rolling window operations
library(MLmetrics)
library(GenSA)
library(stats)
library(Metrics)
library(tidyverse)
library(tsibble)
library(rlang)
library(fable)
library(lubridate)


data = import('data/data_month.xlsx') %>%
  mutate(date = yearmonth(date))
all_vars_quater = import('data/data_quarter.xlsx')
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
exog_tsb

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
all_vars_tsb %>% filter(year(date) == 2019) %>% as_tibble() %>% group_by(series)  %>% summarise(value_2019 = sum(value)) %>% View()

all_vars = full_join(all_vars_tsb, exog_full) %>%
  filter(year(date) < 2021) %>% spread(series, value)

all_vars[is.na(all_vars$dum_1114),]['dum_1114'] = 1
all_vars[is.na(all_vars$dum_2012),]['dum_2012'] = 0
#all_vars = all_vars %>% mutate(r_price_cur_purch = ifelse(r_dum_cur_purch == 0, 0, r_price_cur_purch))

#### сценарий!!!!
#https://cbr.ru/Collection/Collection/File/27833/forecast_200424.pdf
# BRENT - 27, 35, 45
all_vars1 = all_vars %>% mutate(brent = if_else(year(date) == 2020, 27, brent))

#export(all_vars, 'data/all_vars_pred.csv')
### parameters from gensa
par_oil = par_model$par_oil
par_gas = par_model$par_gas
par_op = par_model$par_op
par_othg = par_model$par_othg
par_imp_gds = par_model$par_imp_gds
par_imp_serv = par_model$par_imp_serv
par_exp_serv = par_model$par_exp_serv
par_bal_wage = par_model$par_bal_wage
par_rent_sinc = par_model$par_rent_sinc
par_inv = par_model$par_inv
par_errors = par_model$par_errors
par_difr_res = par_model$par_difr_res
par_cur_purch = par_model$par_cur_purch
par_rub_usd = par_model$par_rub_usd


a = predict_bp(all_vars, par_model)
a%>%tail()%>% View()
X_cur = tibble(r_price_cur_purch = all_vars$r_price_cur_purch,
               brent = all_vars$brent,
               brent_1 = all_vars$brent_1,
               brent_2 = all_vars$brent_2,
               r_dum_cur_purch = all_vars$r_dum_cur_purch)

all_vars$brent
R_cur = all_vars %>%
  select(r_cur_purch) %>%
  rename('r_real' = 'r_cur_purch')

pred_cur_purch = make_pred_cur_purch(par_cur_purch, X_cur)
r_hat_cur_purch = pred_cur_purch$r_hat_cur_purch


all_vars = mutate(all_vars, r_hat_cur_purch = r_hat_cur_purch)
all_vars = all_vars %>% mutate(r_cur_purch = ifelse(is.na(r_cur_purch) == TRUE, r_hat_cur_purch, r_cur_purch))
autoplot(ts.union(real_data = ts(all_vars$r_cur_purch, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_cur_purch, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Currency purchase')





# rub_usd

X_rub_usd = tibble(const = 1,
                   dif_brent_ratio = all_vars$dif_brent_ratio,
                   dif_brent_ratio_1114 = all_vars$dum_1114 * dif_brent_ratio,
                   dif_brent_ratio_cor = (all_vars$vcor*dif_brent_ratio)/mean(all_vars$rub_usd_1, na.rm = TRUE),
                   dif_r_1114 = all_vars$dif_r * all_vars$dum_1114,
                   r_cur_purch = all_vars$r_cur_purch,
                   r_cur_purch_1 = lag(all_vars$r_cur_purch, 1),
                   dif_usd_eur_ratio = all_vars$dif_usd_eur_ratio,
                   em_index_ratio = all_vars$dif_em_index_ratio,
                   em_index_ratio_1114 = em_index_ratio * all_vars$dum_1114,
                   dif_usd_rub_ratio = all_vars$dif_usd_rub_ratio,
                   date = all_vars$date) %>% as_tsibble()

R_rub_usd = as_tibble(all_vars) %>%
  select(rub_usd) %>%
  rename('r_real' = 'rub_usd') %>% as.data.frame()

R_rub_usd_quarter = roll_mean(R_rub_usd$r_real, n = 3, by = 3)

mask = create_mask(nrow(all_vars))

pred_rub_usd = make_pred_rub_usd(par_rub_usd, X_rub_usd,R_rub_usd, mask=mask)
hat_rub_usd_final = pred_rub_usd$hat_rub_usd_final


all_vars = mutate(all_vars, hat_rub_usd_final = hat_rub_usd_final)
all_vars = all_vars %>% mutate(rub_usd = ifelse(is.na(rub_usd) == TRUE, hat_rub_usd_final, rub_usd))

autoplot(ts.union(real_data = ts(all_vars$rub_usd, start = c(2006, 1), freq = 12),
                  model = ts(hat_rub_usd_final, start = c(2006, 1), freq = 12))) + ylab('exchange rate') + xlab('') + ggtitle('Exchange rate (rub/usd)')


all_vars = mutate(all_vars,
                  rub_usd_1 = lag(rub_usd, 1), rub_usd_2 = lag(rub_usd, 2),
                  rub_usd_eur_1 = rub_usd_1 * usd_eur_1,
                  dif_usd_rub_ratio = (rub_usd - rub_usd_1)/rub_usd_1,
                  dif_usd_rub_ratio_1 = (rub_usd_1 - rub_usd_2)/rub_usd_2,
                  dif_usd_eur_ratio = dif_usd_eur/usd_eur_1,
                  dif_usd_rub = rub_usd - rub_usd_1)

X_oil = all_vars %>%
  select(const, brent, brent_1, rub_usd_1)

R_oil = all_vars %>%
  select(p_exp_oil, v_exp_oil, r_exp_oil)

R_oil_quarter = all_vars_quater %>%
  select(p_exp_oil, v_exp_oil, r_exp_oil) %>% na.omit()

pred_oil = make_pred_oil(par_oil, X_oil, R_oil)

autoplot(ts.union(real_data = ts(all_vars$p_exp_oil, start = c(2006, 1), freq = 12),
                  model = ts(pred_oil$p_hat_oil, start = c(2006, 1), freq = 12))) + ylab('p_exp_oil') + xlab('') +  ggtitle('Average price of oil exported')
#ggsave('oil_p.png')

autoplot(ts.union(real_data = ts(all_vars$v_exp_oil, start = c(2006, 1), freq = 12),
                  model = ts(pred_oil$v_hat_oil, start = c(2006, 1), freq = 12))) + ylab('v_exp_oil') + xlab('') + ggtitle('Average volume of oil exported')
#ggsave('oil_v.png')
autoplot(ts.union(real_data = ts(all_vars$r_exp_oil, start = c(2006, 1), freq = 12),
                  model = ts(pred_oil$r_hat_oil, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Average revenue from export of oil')


#ggsave('oil_r.png')

r_hat_oil = pred_oil$r_hat_oil
r_hat_oil_quarter = pred_oil$r_hat_oil_quarter

# oil product model

X_op = all_vars %>%
  select(const,
         brent, brent_1, brent_2, brent_3,
         rub_usd_eur_1, v_prod_op_1)

R_op = all_vars %>%
  select(p_exp_op, v_exp_op, r_exp_op)

R_op_quarter = all_vars_quater %>%
  select(p_exp_op, v_exp_op, r_exp_op) %>% na.omit()

pred_op = make_pred_op(par_op, X_op, R_op)
autoplot(ts.union(real_data = ts(all_vars$p_exp_op, start = c(2006, 1), freq = 12),
                  model = ts(pred_op$p_hat_op, start = c(2006, 1), freq = 12))) + ylab('p_exp_op')  + xlab('') + ggtitle('Average price of oil products exported')
##ggsave('op_p.png')

autoplot(ts.union(real_data = ts(all_vars$v_exp_op, start = c(2006, 1), freq = 12),
                  model = ts(pred_op$v_hat_op, start = c(2006, 1), freq = 12))) + ylab('v_exp_op') + xlab('') + ggtitle('Average volume of oil products exported')

#ggsave('op_v.png')
autoplot(ts.union(real_data = ts(all_vars$r_exp_op, start = c(2006, 1), freq = 12),
                  model = ts(pred_op$r_hat_op, start = c(2006, 1), freq = 12))) + ylab('r_exp_op') + xlab('') + ggtitle('Average revenue from export of oil products')

#ggsave('op_r.png')

r_hat_op = pred_op$r_hat_op
r_hat_op_quarter = pred_op$r_hat_op_quarter

### optimization for gas model


X_gas = all_vars %>%
  select(const, dum_2012,
         gas_europe,
         gas_lng_3, gas_lng_5,
         brent_1, brent_3, brent_5, brent_6,
         v_prod_gas, usd_eur, dif_usd_rub_ratio_1)

R_gas = all_vars %>%
  select(p_exp_gas, v_exp_gas, r_exp_gas)

R_gas_quarter = all_vars_quater %>%
  select(p_exp_gas, v_exp_gas, r_exp_gas) %>% na.omit()


pred_gas = make_pred_gas(par_gas, X_gas, R_gas)

r_hat_gas = pred_gas$r_hat_gas
r_hat_gas_quarter = pred_gas$r_hat_gas_quarter


autoplot(ts.union(real_data = ts(all_vars$p_exp_gas, start = c(2006, 1), freq = 12),
                  model = ts(pred_gas$p_hat_gas, start = c(2006, 1), freq = 12))) + ylab('p_exp_gas') + xlab('') + ggtitle('Average price of gas exported')
#ggsave('gas_p.png')

autoplot(ts.union(real_data = ts(all_vars$v_exp_gas, start = c(2006, 1), freq = 12),
                  model = ts(pred_gas$v_hat_gas, start = c(2006, 1), freq = 12))) + ylab('v_exp_gas') + xlab('') + ggtitle('Average volume of gas exported')
#ggsave('gas_v.png')

autoplot(ts.union(real_data = ts(all_vars$r_exp_gas, start = c(2006, 1), freq = 12),
                  model = ts(pred_gas$r_hat_gas, start = c(2006, 1), freq = 12))) + ylab('r_exp_gas')  + xlab('') + ggtitle('Average revenue from export of gas')
#ggsave('gas_r.png')


### optimisation for export other goods model

X_othg = tibble(const = 1,
                r_hat_oog = r_hat_gas + r_hat_oil + r_hat_op,
                r_hat_oog_dum = r_hat_oog * all_vars$dum_1114,
                gpd_defl = lag(all_vars$n_y, 1)/ all_vars$rub_usd_1)

R_othg = all_vars %>% select(r_exp_othg, r_exp_goods)

R_othg_quarter = all_vars_quater %>%
  select(r_exp_othg, r_exp_goods) %>% na.omit()

pred_exp = make_pred_exp(par_othg, X_othg, R_othg)

autoplot(ts.union(real_data = ts(all_vars$r_exp_othg, start = c(2006, 1), freq = 12),
                  model = ts(pred_exp$r_hat_othg, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of other goods')
#ggsave('exp_othg.png')


autoplot(ts.union(real_data = ts(all_vars$r_exp_goods, start = c(2006, 1), freq = 12),
                  model = ts(pred_exp$r_hat_gds, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of all goods')

#ggsave('exp_all.png')

r_hat_othg = pred_exp$r_hat_othg
r_hat_gds = pred_exp$r_hat_gds

# Model for goods import
X_imp = tibble(consump_defl = lag(all_vars$n_c, 1)/all_vars$rub_usd_1,
               j_defl = lag(all_vars$n_j, 1)/all_vars$rub_usd_1,
               ds_defl = (all_vars$n_ds_1)/all_vars$rub_usd_1,
               r_hat_gds = r_hat_gds)

R_imp = all_vars %>%
  select(r_imp_goods, r_imp_serv, r_imp_all)

R_imp_quarter = all_vars_quater %>%
  select(r_imp_goods, r_imp_serv, r_imp_all) %>% na.omit()


par_imp = c(par_imp_gds, par_imp_serv)

pred_imp = make_pred_imp(par_imp, X_imp, R_imp)

autoplot(ts.union(real_data = ts(all_vars$r_imp_goods, start = c(2006, 1), freq = 12),
                  model = ts(pred_imp$r_hat_imp_gds, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from import of goods')

#ggsave('imp_goods.png')

autoplot(ts.union(real_data = ts(all_vars$r_imp_serv, start = c(2006, 1), freq = 12),
                  model = ts(pred_imp$r_hat_imp_serv, start = c(2006, 1), freq = 12))) + ggtitle('Average revenue from import of services') + ylab('revenue') + xlab('')

#ggsave('imp_services.png')

autoplot(ts.union(real_data = ts(all_vars$r_imp_all, start = c(2006, 1), freq = 12),
                  model = ts(pred_imp$r_imp_all, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from import')

#ggsave('imp_all.png')

r_hat_imp_gds = pred_imp$r_hat_imp_gds
r_hat_imp_serv = pred_imp$r_hat_imp_serv
r_hat_imp_all = pred_imp$r_imp_all

### Model for export of services

par = par_exp_serv

X_exp_serv = tibble(const = 1,
                    r_hat_gds = r_hat_gds,
                    r_hat_imp_serv = r_hat_imp_serv)

R_exp_serv = all_vars %>%
  select(r_exp_serv)

R_exp_serv_quarter = all_vars_quater %>%
  select(r_exp_serv) %>% na.omit()


pred_exp_serv = make_pred_exp_serv(par_exp_serv, X_exp_serv, R_exp_serv)

autoplot(ts.union(real_data = ts(all_vars$r_exp_serv, start = c(2006, 1), freq = 12),
                  model = ts(pred_exp_serv$r_hat_exp_serv, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of services')

#ggsave('exp_serv.png')

r_hat_exp_serv = pred_exp_serv$r_hat_exp_serv

r_hat_exp_all = r_hat_exp_serv + r_hat_gds
r_hat_exp_all_quarter = c(NA, roll_sum(r_hat_exp_all[4:length(r_hat_exp_all)], n = 3, by = 3))

autoplot(ts.union(real_data = ts(all_vars$r_exp_all, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_exp_all, start = c(2006, 1), freq = 12))) + ylab('revenue') + xlab('') + ggtitle('Revenue from export of goods and services')

#ggsave('exp_gs.png')

r_hat_bal_trade = r_hat_gds - r_hat_imp_gds
r_hat_bal_trade_quarter = roll_sum(r_hat_bal_trade, n = 3, by = 3)


r_hat_bal_serv = r_hat_exp_serv - r_hat_imp_serv
r_hat_bal_serv_quarter = roll_sum(r_hat_bal_serv, n = 3, by = 3)


#### model for (1) balance of rent and secondary income; (2) investement; (3) wages

X_rent_sinc = tibble(const = 1,
                     r_hat_exp_serv = r_hat_exp_serv,
                     r_hat_gds = r_hat_gds)

R_rent_sinc = all_vars %>%
  select(r_bal_rent, r_bal_sinc) %>%
  mutate(r_bal_rent_sinc = r_bal_rent + r_bal_sinc) %>%
  select(r_bal_rent_sinc) %>% rename('r_real' = 'r_bal_rent_sinc')

R_rent_sinc_quarter = all_vars_quater %>%
  select(r_bal_rent, r_bal_sinc) %>%
  mutate(r_bal_rent_sinc = r_bal_rent + r_bal_sinc) %>%
  select(r_bal_rent_sinc) %>%
  rename('r_real' = 'r_bal_rent_sinc')%>%
  na.omit()


pred_rent_sinc = make_pred_balances(par_rent_sinc, X_rent_sinc, R_rent_sinc)

autoplot(ts.union(real_data = ts(all_vars$r_bal_rent + all_vars$r_bal_sinc, start = c(2006, 1), freq = 12),
                  model = ts(pred_rent_sinc$r_hat, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Balance of rent and secondary income')

#ggsave('rent_sink.png')

r_hat_rent_sinc = pred_rent_sinc$r_hat


# (2) model for the balance of investment

X_inv = tibble(const = 1,
               r_hat_bal_trade = r_hat_bal_trade,
               r_hat_bal_serv = r_hat_bal_serv)

R_inv = all_vars %>%
  select(r_bal_inv) %>%
  rename('r_real' = 'r_bal_inv')

R_inv_quarter = all_vars_quater %>%
  select(r_bal_inv) %>%
  rename('r_real' = 'r_bal_inv')

pred_inv = make_pred_balances(par_inv, X_inv, R_inv)

autoplot(ts.union(real_data = ts(all_vars$r_bal_inv, start = c(2006, 1), freq = 12),
                  model = ts(pred_inv$r_hat, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Balance of investment income')

#ggsave('inv_income.png')
r_hat_inv = pred_inv$r_hat

# (3) balance of wages
X_wage = tibble(const = 1,
                r_hat_oil = r_hat_oil,
                r_hat_othg = r_hat_othg,
                r_hat_exp_serv = r_hat_exp_serv,
                r_hat_imp_serv = r_hat_imp_serv)


R_wage = all_vars %>%
  select(r_bal_wage) %>%
  rename('r_real' = 'r_bal_wage')

R_wage_quarter = all_vars_quater %>%
  select(r_bal_wage) %>%
  rename('r_real' = 'r_bal_wage') %>%
  na.omit()

pred_wage = make_pred_balances(par_bal_wage, X_wage, R_wage)
autoplot(ts.union(real_data = ts(all_vars$r_bal_wage, start = c(2006, 1), freq = 12),
                  model = ts(pred_wage$r_hat, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('Balance of wages')

#ggsave('bwages.png')
r_hat_wage = pred_wage$r_hat
### model for erros

X_errors = select(all_vars, const, dif_brent, dum01:dum12)

R_errors = all_vars %>%
  select(r_errors) %>%
  rename('r_real' = 'r_errors')

R_errors_quarter = all_vars_quater %>%
  select(r_errors) %>%
  rename('r_real' = 'r_errors') %>%
  na.omit()

pred_erros = make_pred_errors(par_errors, X_errors)

r_hat_errors = pred_erros$r_hat_errors

autoplot(ts.union(real_data = ts(all_vars$r_errors, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_errors, start = c(2006, 1), freq = 12))) + ylab('value') + xlab('') + ggtitle('net errors and omissions')r_hat_cur_acc = r_hat_inv + r_hat_rent_sinc + r_hat_bal_serv + r_hat_bal_trade + r_hat_wage

r_hat_cur_acc = r_hat_inv + r_hat_rent_sinc + r_hat_bal_serv + r_hat_bal_trade + r_hat_wage

X_difr = tibble(const = 1,
                dif_brent = all_vars$dif_brent,
                dif_brent_cor = (all_vars$dif_brent * all_vars$vcor)/(all_vars$rub_usd_1),
                dif_brent_1 = all_vars$brent_1 - all_vars$brent_2,
                dif_usd_rub = all_vars$dif_usd_rub,
                dif_usd_rub_1 = all_vars$rub_usd_1 - all_vars$rub_usd_2,
                dif_usd_eur = all_vars$dif_usd_eur,
                r_hat_cur_acc = r_hat_cur_acc,
                r_hat_cur_acc_1 = lag(r_hat_cur_acc, 1),
                quarter = 0,
                r_cur_purch = all_vars$r_cur_purch,
                r_cur_purch_1 = lag(all_vars$r_cur_purch, 1))


X_difr_dummy = bind_cols(X_difr, select(all_vars, dum01:dum12))

R_dif_reserves = all_vars %>%
  select(r_dif_reserves) %>%
  rename('r_real' = 'r_dif_reserves')
pred_res = make_pred_dif_res(par_difr_res, X_difr_dummy)
par_difr_res
r_hat_dif_res = pred_res$r_hat_dif_res
r_hat_dif_res_quarter = pred_res$r_hat_dif_res_quarter
r_hat_dif_res_short = pred_res$r_hat_dif_res_short
r_hat_dif_res_short_quarter = pred_res$r_hat_dif_res_short_quarter

autoplot(ts.union(real_data = ts(all_vars$r_dif_reserves, start = c(2006, 1), freq = 12),
                  model = ts(r_hat_dif_res_short, start = c(2006, 1), freq = 12))) + ylab('change') + xlab('') + ggtitle('Difference of reserves')

