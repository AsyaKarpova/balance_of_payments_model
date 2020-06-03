library(tidyverse)
library(lubridate)
library(rio)
library(feasts)
library(tseries)
library(tsibble)
library(patchwork)



all_data = import('data/data_month1.xlsx') %>%mutate(date = yearmonth(date)) %>%as_tsibble()
all_data_quarter = import('data/data_quarter_new.xlsx')%>%mutate(date = yearquarter(date)) %>%as_tsibble()
quarter_structure = all_data_quarter %>% select(date, r_cur_account, r_bal_fin, r_cap_account, r_errors)
all_data %>% feasts::ACF(rub_usd) %>% autoplot() + xlab('lag, месяц') + ylab('ACF')
all_data %>% feasts::gg_subseries(r_dif_reserves, period=4)
21qwrub_usd = select(all_data, rub_usd) %>% as_tibble() %>%select(-date)
print(kpss.test(ts(rub_usd,start = c(2006,1)), null = c("Level", "Trend"), lshort = TRUE)$statistic)
all_data%>%feasts::features(rub_usd, KPSS)
data_export = all_data_quarter %>%select(date, r_exp_oil, r_exp_op, r_exp_gas, r_exp_othg) %>%filter(year(date)==2019)
data_export
data_share = data_export%>%gather(key='export_type', value='value', -date)%>%group_by(export_type)%>%
  summarize(share = sum(value)) %>%
  mutate(share1 = share/sum(share)) %>%
  mutate(struct = if_else(export_type == 'r_exp_othg', 'Другие товары', 'Топливо')) %>% mutate(Товары = if_else(export_type == 'r_exp_othg', 'Другое',
                                                                                                                if_else(export_type == 'r_exp_oil','Нефть',
                                                                                                                        if_else(export_type == 'r_exp_gas','Газ',
                                                                                                                                'Нефтепродукты')))) %>%
  mutate(Товары = factor(Товары, levels = c('Нефть', 'Нефтепродукты', 'Газ', 'Другое')))

ggplot(data_share, aes(x = struct, y = share1, fill = Товары)) +
  geom_bar(stat = "identity", width = 0.2) +
  coord_flip() +
  ylab("Доля в экспорте") + ggtitle('Структура экспорта РФ, 2019 год')+
  xlab("")

ggplot(data_share, aes(y = share, fill = export_type)) +
  geom_bar(stat = "identity")+theme_minimal() +
  coord_flip()

for_plot = import('data/structure.csv', header=TRUE, dec='.',
                  encoding = "UTF-8")


for_plot2 = mutate_all(for_plot, ~str_replace(., ",", ""))


plot_spread = gather(for_plot2, key = 'date', value = 'Value', `01.01.2000`:`01.10.2019`)
#%>% select(V1, Value, date)
#plot_spread$date = parse_date_time(plot_spread$date, orders = 'dmY')

structure = filter(plot_spread,
                   V1 != 'Счет текущих операций',  V1 != 'Счет операций с капиталом',
                   V1 != 'Финансовый счет',V1 !='', V1 != 'Чистые пропуски и ошибки', V1 != 'Изменение резервных активов')


plot_spread  = plot_spread %>% mutate(date=yearquarter(dmy(date)),
                                  Value=as.numeric(Value))
structure  = structure %>% mutate(date=yearquarter(dmy(date)),
                                 Value=as.numeric(Value))
structure$V1
ggplot(structure, aes(x = date, y = Value/1000)) +
  geom_line(color = 'red') + facet_wrap(.~V1) + xlab('') + ylab('млрд. долл.')

cur_acc = filter(plot_spread, V1 == 'Счет текущих операций') %>%
  ungroup()

fin_acc = filter(plot_spread, V1 == 'Финансовый счет') %>%
  ungroup()

cap_acc = filter(plot_spread, V1 == 'Счет операций с капиталом') %>%
  ungroup()

r_errors = filter(plot_spread, V1 == 'Чистые пропуски и ошибки') %>%
  ungroup()
r_errors

f = ggplot(cur_acc, aes(y = Value/1000, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ylab('млрд. долл.') + ggtitle('Счет текущих операций')
a = ggplot(fin_acc, aes(y = Value/1000, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ylab('млрд. долл.') + ggtitle('Финансовый баланс')
b = ggplot(cap_acc, aes(y = Value/1000, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ylab('млрд. долл.') + ggtitle('Cчет операций с капиталом' )
c = ggplot(r_errors, aes(y = Value/1000, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ylab('млрд. долл.') + ggtitle('Чистые ошибки и пропуски' )

f/a/b/c


left_join(brent, r_errors, by = 'date')

ggplot(brent, aes(y = 100*r_errors, x = date)) + geom_line(colour = 'red', size=0.5) +
  geom_line(data = brent, aes(y = brent, x = date))+ xlab('') + ggtitle('Current account')

names(all_data)
all_data = import('data/data_quarter_new.xlsx')
all_data%>% select(n_y, n_j, n_c, n_ex, n_im, n_ds)
n_y = rep(all_data$n_y/3, each = 3)
n_c = rep(all_data$n_c/3, each = 3)
n_j = rep(all_data$n_j/3, each = 3)
n_ex = rep(all_data$n_ex/3, each = 3)
n_im = rep(all_data$n_im/3, each = 3)
n_ds = rep(all_data$n_ds/3, each = 3)
n_g = rep(all_data$n_g/3, each = 3)
prolonged_n = data.frame(n_y, n_c, n_j, n_ex, n_im, n_ds, n_g)/1000
export(prolonged_n, 'prol_n.csv')

