# Imagine that you want to plot the responses of a ordinal questionnaire
# besides the common practice, is not always correct to use ordinal data as
# continous. There are good resources like https://bookdown.org/Rmadillo/likert/always-visualize.html
# which tell you how to proper represent ordinal data. However, I found that if your scale has missing 
# response options in one question... the package likert does not deal with that
# so, I self-made the plot(ex_2_likert, type = "heat") graph

# Creating a fake "ordinal" data with seven questions with 0-4 responses
set.seed(1989)
df = data.frame(
p1 = floor(rnorm(234, mean = 0, sd = 3)),
p2 = floor(rnorm(234, mean = 1, sd = 2)),
p3 = floor(rnorm(234, mean = 2, sd = 1)),
p4 = floor(rnorm(234, mean = 3, sd = 1)),
p5 = floor(rnorm(234, mean = 4, sd = 1)),
p6 = floor(rnorm(234, mean = 5, sd = 2)),
p7 = floor(rnorm(234, mean = 6, sd = 3))
)
change_values <- function(x) 
{
  x[x<0] <- 0
  x[x>4] <- 4
  return(x)
}
df <- apply(df, 2, change_values)
df <- as.data.frame(df)

library(tidyverse)
library(see)

# first we create a tidy data frame 
# where the absolute value of each response is calculated
resp_p = df %>% dplyr::select(p1:p7) %>% 
  pivot_longer(p1:p7) %>% 
  group_by(name, value) %>% 
  summarise(n = n())
# We create a relative frequency of each response by each question calculating the 
# total number of responses for each question 
resp_p_perc <- resp_p %>% group_by(name) %>% summarise(n_total = sum(n))
resp_p_perc
# and then joining both data frames
resp_p <- left_join(resp_p, resp_p_perc) %>% mutate(p = n/n_total) 
# we can make the format labelling in this place
resp_p <- resp_p %>% 
  # creating the formated percentages by
  # multiplying by 100, rounding (round()), applying the two digit format  (format())
  # and finally adding a % simbol with paste0
  mutate(p = paste0(format(round(p*100, 2), digits = 2), " %"), 
 # reorder the factors                                
  name = factor(name, levels = paste0("p", 7:1)),
 # we can change the item for a descriptive label 
   name_label = case_when(
                                    
                                    name == "p1" ~ "1. Isolated", 
                                    name == "p2" ~ "2. Ashamed", 
                                    name == "p3" ~ "3. Inferior",
                                    
                                    name == "p4" ~ "4. Embarrssing", 
                                    name == "p5" ~ "5. Insecure",
                                    
                                    name == "p6" ~ "6. Negative evaluated", 
                                    name == "p7" ~ "7. Inadequate"
                                  ))
resp_p 

# If wewant to order the responses by the median (in order to use the questions 
# order as information...) and then we join it to the data frame

resp_p_med = df %>% dplyr::select(p1:p7) %>% 
  pivot_longer(p1:p7) %>%
  group_by(name) %>%
  summarise(median = median(value)) %>% arrange(desc(median))
resp_p  <- left_join(resp_p, resp_p_med)

# we are ready to plot with geom_tile
resp_p %>% 
  ggplot(aes(x = reorder(name_label, median), # this part makes the order by the median
             y  = as.factor(value), fill = n)) +
  geom_tile() + 
  geom_text(aes(label = p)) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  coord_flip() +
  theme_modern() +
  scale_y_discrete(labels = c(
    "0" = "0\nNever true", # note the use of \n to break the line in the label
    "1" = "1",
    "2" = "2",
    "3" = "3",
    "4" = "4\nAlways true"),
    name = "Response") +
  scale_x_discrete(name = "Question") +
  labs(title = "P scores distribution",
       subtitle = "Higher scores represent more agreement with statement, questions are presented ordered by median scores",
       caption = "Data source: DOI") +
  theme(legend.position = "none") + # we remove the legend
  theme(axis.text.x = element_text(angle = 45, hjust=1))
# uncomment to save the plot 
# ggsave("p_responses.png", units = "cm", width = 25, height = 20)