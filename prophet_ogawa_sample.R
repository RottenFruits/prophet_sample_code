#前準備------------------------------------------------------------------------
#ライブラリ読み込み
library(prophet)
library(tidyverse)

#データ読み込み
#ブログのユーザー数のアクセスログ
df <-read.csv("s_ogawa_blog_access.csv", skip = 13, stringsAsFactors = FALSE)

#変数名・型調整
df <- df %>%
  mutate(ds = as.Date(日の指標), y = as.numeric(ユーザー)) %>%
  mutate(y = ifelse(y == 0, NA, y)) %>%
  mutate(year = format(ds, "%Y"),
         month = format(ds, "%m"),
         week = format(ds, "%W"),
         day = format(ds, "%d"),
         wod = format(ds, "%A"),
         month_day = format(ds, "%m/%d")
         ) %>%
  select(-日の指標, -ユーザー)


#月ごとの週番号
week_number <- df %>% 
  distinct(year, month, week) %>%
  group_by(year, month) %>%
  mutate(week_number = order(week))

df <- df %>% left_join(week_number, by = c("year", "month", "week"))


#データ可視化
##全体可視化
ggplot(df, aes(x = ds, y = y)) +
  geom_line()

##年ごと周期確認
ggplot(df, aes(x = month_day, y = y, group = year)) +
  geom_line() +
  facet_grid(year~0)

##月ごと周期確認
ggplot(df, aes(x = day, y = y, group = month)) +
  geom_line() +
  facet_grid(year~month)

##週ごと周期確認
ggplot(df %>% filter(year == 2019), aes(x = wod, y = y, group = month)) +
  geom_line() +
  facet_grid(week_number ~ month)



#全デフォルト------------------------------------------------------------------------
#フィッティング
m <- prophet(df)

#予測用データフレーム作成
future <- make_future_dataframe(m, periods = 30)
tail(future)

#予測
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#予測可視化
plot(m, forecast)

#要素の可視化
prophet_plot_components(m, forecast)

#パラメータ試行錯誤------------------------------------------------------------------------
#フィッティング
df2 <- df
df2$y <- log(df$y)
m <- prophet(df2, 
             yearly.seasonality = FALSE,
             weekly.seasonality = TRUE,
             daily.seasonality = FALSE,
             seasonality.prior.scale = 10,
             changepoint.prior.scale = 5) #tau:changepoint.prior.scale

#予測用データフレーム作成
future <- make_future_dataframe(m, periods = 30)
tail(future)

#予測
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#予測可視化
plot(m, forecast)

#要素の可視化
prophet_plot_components(m, forecast)
