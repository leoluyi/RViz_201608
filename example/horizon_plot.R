library(magrittr)
library(dplyr)
library(ggplot2)
require(quantmod)
require(PerformanceAnalytics)
# devtools::install_github('Ather-Energy/ggTimeSeries')
library(ggTimeSeries)

## data
data(edhec)
ed = data.frame(edhec)
ed$date = as.Date(rownames(ed))
m = melt(ed, id = "date")
m$variable = gsub('\\.', ' ', m$variable)

## data2
data(edhec)
# get 12 month rolling return of edhec indexes
roc <- as.xts(apply(cumprod(edhec + 1),
                    MARGIN = 2,
                    ROC,
                    n = 12,
                    type = "discrete"),
              order.by = index(edhec))
roc.df <- roc %>% broom::tidy() %>% tbl_df %>% na.omit()

## tile plot
# http://stevepowell.blot.im/horizon-plots-with-ggplot-not/
ggplot(roc.df, aes(index, 0, fill = value)) +
  geom_tile(aes(height = max(value) - min(value))) +
  geom_line(aes(x = index, y = value)) +
  facet_grid(series ~ .) +
  scale_fill_gradient2(low = "red", high = "blue") +
  ylab("value") +
  theme(strip.text.y = element_text(angle = 0, hjust = 1))

## not so good
ggplot_horizon(na.omit(roc.df), "V1", "value") +
  facet_grid(variable ~.) +
  scale_fill_continuous(low = 'red', high = 'green') +
  theme(strip.text.y = element_text(angle = 0, hjust = 1))
