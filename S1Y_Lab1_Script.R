library(tidyverse)

ratings2 <- read_csv('ratings_seasons.csv')
ratings2 <- mutate(ratings2, channel = case_when(series < 5 ~ "BBC2",
                                               series > 4 & series <8 ~ "BBC1",
                                               series > 7 ~ "C4"))
str(ratings2)
ratings2 <- ratings2 %>%
  mutate(series = as.factor(series),
         episode = as.factor(episode),
         channel = as.factor(channel))


ratings <- read.csv('ratings_seasons.csv')
View(ratings)
str(ratings, give.attr=FALSE)  #The argument give.attr=FALSE surpresses extra info
ratings$viewers_7day
ratings <- mutate(ratings, channel = case_when(series < 5 ~ "BBC2",
                                               series > 4 & series <8 ~ "BBC1",
                                               series > 7 ~ "C4"))
str(ratings)

select(ratings, series, channel)
ratings <- ratings %>%
  mutate(series = as.factor(series),
         episode = as.factor(episode),
         channel = as.factor(channel))
str(ratings)


summarise(ratings,
          mean_ratings = mean(viewers_7day),
          sd_ratings = sd(viewers_7day),
          min_ratings = min(viewers_7day),
          max_ratings = max(viewers_7day))

count(ratings, series)
count(ratings, channel, series)

ratings_grouped <- ratings %>%
  group_by(channel)

ch_ratings <- ratings_grouped %>%
  summarise(count = n(),
            mean_ratings = mean(viewers_7day),
            min_ratings = min(viewers_7day),
            max_ratings = max(viewers_7day)) %>%
  ungroup()

ch_ratings

ch_ratings <-
  ratings %>%             # Start with the original dataset; and then
  group_by(channel) %>%   # group it; and then
  summarise(count = n(),  # summarise it by those groups
            mean_ratings = mean(viewers_7day),
            min_ratings = min(viewers_7day),
            max_ratings = max(viewers_7day)) %>%
  ungroup()

ch_ratings

ch_series_ratings <-
  ratings %>%
  group_by(channel, series) %>%
  summarise(count = n(),
            mean_ratings = mean(viewers_7day),
            min_ratings = min(viewers_7day),
            max_ratings = max(viewers_7day)) %>%
  ungroup()

ch_series_ratings


ggplot(ratings, aes(x = channel)) +
  geom_bar()

ggplot(ratings, aes(x = channel))
+ geom_bar()

ggplot(ratings, aes(x = channel, 
                    fill = channel)) +
  geom_bar() +
  labs(title="Number of episodes of GBBO broadcast on each channel")+ # adds a plot title
  ylab("Number of Episodes")+ # adds an axis label
  theme(legend.position = "none")+ # removes the legend
  scale_x_discrete(
    # change axis label
    name = "Broadcast Channels (in chronological order)", 
    # change to chronological order
    limits = c("BBC2", "BBC1", "C4"), 
    # change labels
    labels = c("BBC 2", "BBC 1", "Channel 4")
  )

ratings <- ratings %>% mutate(ep_id = row_number())

ggplot(ratings, aes(x = ep_id, y = viewers_7day, fill = series)) +
  geom_col() +
  labs(title="7-Day Viewers across All Series/Episodes")+
  ylab("Number of viewers (millions)")+      # adds an axis label
  xlab("Episode Index")+                     # adds an axis label
  scale_fill_discrete(name="Series")         # set the name of the legend  


ggplot(ratings, aes(x = viewers_7day)) +
  geom_histogram()

# adjust width of each bar
ggplot(ratings, aes(x = viewers_7day)) +
  geom_histogram(binwidth = 2)
# adjust number of bars
ggplot(ratings, aes(x = viewers_7day)) +
  geom_histogram(bins = 5)

ggplot(ratings, aes(x = viewers_7day)) +
  geom_histogram(binwidth = 1, 
                 boundary = 0, 
                 fill = "white", 
                 color = "black") +
  scale_x_continuous(name = "Number of viewers 7 days after broadcast (in millions)")

ggplot(ratings, aes(x = viewers_7day, fill = channel)) +
  geom_histogram(binwidth = 1,
                 color = "black")

ggplot(ratings, aes(x = channel, y = viewers_7day)) +
  geom_violin() +
  ggtitle('scale = "area"')

ggplot(ratings, aes(x = channel, y = viewers_7day)) +
  geom_violin(scale = "count") +
  ggtitle('scale = "count"')

ggplot(ratings, aes(x = channel, y = viewers_7day)) +
  geom_boxplot()

ggplot(ratings, aes(x = ep_id, y = viewers_7day)) +
  geom_point()

avg_ratings <- ratings %>% 
  select(series, episode, viewers_7day) %>% 
  group_by(series) %>%
  summarise(avg_viewers_7day = mean(viewers_7day)) %>%
  ungroup()

ggplot(avg_ratings, aes(x = series, y = avg_viewers_7day, group=1)) +
  geom_point() +
  geom_line() +
  ggtitle("Great British Bake Off Average Ratings")

plot_data <- ratings %>% 
  select(series, episode, viewers_7day) %>% 
  group_by(series) %>%
  filter(episode == 1 | episode == max(as.numeric(episode))) %>%
  mutate(episode = recode(episode, "1" = "first", .default = "last")) %>%
  ungroup()

ggplot(plot_data, aes(x = series,
                      y = viewers_7day,
                      color = episode, 
                      group = episode
)) +
  geom_point() +
  geom_line() +
  ggtitle("Great British Bake Off Finales Get More Viewers than Premiers") +
  labs(color = "Episode")


# 5 Group Tasks

source("http://www.openintro.org/stat/data/cdc.R")
str(cdc)

# Q1

ggplot(data = cdc, mapping = aes(x = wtdesire, y = weight)) +
  geom_point() +
  labs(title = "Plot of Weight vs Desired Weight", x = "Desired Weight", y = "Weight")+
  geom_abline()

# Q2

cdc <- cdc %>%
  mutate(wdiff = (wtdesire - weight))

# Q4

ggplot(data = cdc, mapping = aes(x = wdiff)) +
  geom_histogram(binwidth = 10) +
  xlim(-200, 200)

summary(cdc$wdiff)


# Q5

cdc.men <- cdc %>%
  filter(gender == "m")
cdc.women <- cdc %>%
  filter(gender == "f")
summary(cdc.men$wdiff)

summary(cdc.women$wdiff)

ggplot(data = cdc, mapping = aes(x = gender, y = wdiff)) +
  geom_boxplot()

