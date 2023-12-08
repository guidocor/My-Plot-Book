library(ggplot2)
library(dplyr)
set.seed(123)
# Problem: I want to plot the responses to certain items
# by each condition (a response on other )

# lets simulate some data. 
# here we have two conditions, 10 items and 5 response options
n = 6000
df <- data.frame(
  exp_condition = sample(c("0", "1"), n, replace = TRUE), 
  item = paste0("item_", 1:6),
  response = sample(1:5, n, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.2, 0.1)))

# This part of the code will order the facets by median
#~of each item
medians <- df %>%
  group_by(item) %>%
  summarize(median = median(response)) %>%
  arrange(median) %>%
  pull(item)

# Reordering the factor levels based on medians
df$item <- factor(df$item, levels = medians)

dist_items <-df %>%  ggplot(aes(x = response, color = exp_condition, fill = exp_condition)) + 
  geom_bar(aes(y=after_stat(count)), position=position_dodge(), color = "black") +
  scale_fill_manual(values=c("#9e42f5", "#E69F00"), 
                    name="Experimental Condition",
                    breaks=c("0", "1"),
                    labels=c("Contriol", "Experimental")) +
  ylab("") + xlab("") +
  facet_wrap(~item, ncol = 3) +
  theme(legend.position = "top") +
  scale_x_continuous(name = "X label")+
  ggtitle("Item response by condition")

dist_items