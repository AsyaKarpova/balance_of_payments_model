library(tidyverse)
library(lubridate)
library(rio)

for_plot = import('data/structure.csv')
all_data = import('data/data_month.xlsx')

plot_spread = gather(for_plot, key = 'date', value = 'Value', `01.01.2000`:`01.04.2019`)%>% select(V1, Value, date)
plot_spread$date = parse_date_time(plot_spread$date, orders = 'dmY')
plot_spread %>% select(V1) %>% unique()

fuels = plot_spread %>% pivot_wider(names_from = `V1`, values_from = Value) %>% mutate(fuels = r_exp_op + r_exp_oil + r_exp_gas)

fuels %>% group_by(date) %>% mutate(share = 1000 * fuels/`Current Account`) %>% select(date, share, `Current Account`, fuels)

structure = filter(plot_spread,
                   V1 != 'Current Account', V1 != 'Financial Account', V1 != 'Capital Account', V1 != 'Balance on Rent',
                   V1 != 'Financial Account', V1 != 'Net Errors and Omissions', V1 != 'Reserves and Related Items')
plot_spread$V1 %>% unique()

ggplot(structure, aes(x = date, y = Value)) +
  geom_line(color = 'red') + facet_wrap(.~V1) + xlab('') +ggtitle('Components of the Current Account')

cur_acc = filter(plot_spread, V1 == 'Current Account') %>%
  ungroup()

fin_acc = filter(plot_spread, V1 == 'Financial Account') %>%
  ungroup()

cap_acc = filter(plot_spread, V1 == 'Capital Account') %>%
  ungroup()

r_errors = filter(plot_spread, V1 == 'Net Errors and Omissions') %>%
  ungroup()
r_errors

ggplot(cur_acc, aes(y = Value, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ggtitle('Current account')
ggplot(fin_acc, aes(y = Value, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ggtitle('Financial account')
ggplot(cap_acc, aes(y = Value, x = date)) + geom_line(colour = 'red', size=0.5) + xlab('') + ggtitle('Capital account')
ggplot(r_errors, aes(y = Value, x = date)) + geom_line(colour = 'red', size=0.5) +
  geom_line(data = brent, aes(y = 1000 * c(0, diff(r_exp_oil)), x = date))+ xlab('') + ggtitle('Current account')
brent = all_data %>% select(brent, date, r_errors, r_exp_oil) %>%na.omit() %>% mutate(brent_diff = c(0, diff(brent)))
bre
cor(r_errors$Value, c(0, diff(brent$r_exp_oil)))
left_join(brent, r_errors, by = 'date')
r_errors
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

