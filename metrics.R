
library(tidyverse)
library(lubridate)

# http://docs.rstudio.com/shinyapps.io/metrics.html#ApplicationMetrics
df_ <- rsconnect::showMetrics(metricSeries = "container_status",
                             metricNames = c("connect_count",
                                             "connect_procs"),
                             appName="word-cloud-maker",
                             server="shinyapps.io",
                             from="3w",
                             interval="1m",
                             account = "werdna63"
)

df <- df_ %>%
  # magrittr::set_colnames(c("connect_count", "dummy", "connect_procs", "timestamp")) %>%
  magrittr::set_colnames(c("dummy", "timestamp", "connect_procs", "connect_count")) %>%
  mutate(across(everything(), as.numeric))

df1 <- df %>%
  mutate(date=as_datetime(timestamp)) %>%
  select(-timestamp) %>%
  arrange(date) %>%
  mutate(
    n_count=cumsum(connect_count),
    n_procs=cumsum(connect_procs),
    new_connect=case_when(
      connect_count>lag(connect_count,1) ~ connect_count-lag(connect_count,1),
      TRUE ~ 0),
    n_connect=cumsum(new_connect) # approximate
  ) %>%
  filter(n_count>0)

df2 <- df1 %>%
  select(n_connect, date) %>%
  gather(key="key", value="value", -date)

p2 <- ggplot(df2) +
  labs(title="Cumulative Connections", x="", y="") +
  geom_line(aes(x=date, y=value, colour=key)) +
  facet_wrap(~key)

print(p2)

df1 |>
  ggplot(aes(x = date,
             y = connect_procs)) +
  geom_line()
