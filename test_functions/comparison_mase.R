library(fable)
library(Metrics)

data = tsibble(date = 1:100, value = rnorm(100), index = date)
data_train = data %>% filter(date<=90)
models = data_train %>%
  model(
    ets = ETS(value),
    arima = ARIMA(value),
    rw  = RW(value),
    naive = NAIVE(value)
  )
data_forecasts = models %>%
  forecast(h = 10)

mape = data_forecasts %>%
  accuracy(data) %>%
  arrange(MAPE)

mape %>% View()

mase = mape %>% select(.model, MAE)
mase
mase_l = mase %>% pivot_longer(cols = MAE)
mase_rw = mase_l %>%filter(.model == 'rw') %>% rename(value_rw = value)  %>% select(value_rw)
mase_fable = mase$MAE/mase_rw[[1]]
mase_final = left_join(mase, mase_rw, by=TRUE) %>% mutate(mase = MAE / value_rw) %>%filter(.model != 'rw')
mase_final
series_models

ets_fcst = data_forecasts %>% as_tibble() %>%select(-.distribution) %>%filter(.model == 'ets')
mase(tail(data$value, 10), ets_fcst$value)
help(mase)
