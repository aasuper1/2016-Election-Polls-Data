setwd("~/Desktop/Data Science Club/Poll Believability")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
data <- read.csv("presidential_polls.csv", head = TRUE, sep = ",")
data_final <- read.csv("Election_Results.csv", head = TRUE, sep = ",")
data_states = subset(data, state != "U.S.")
data_states = subset(data_states, state != "Maine CD-1")
data_states = subset(data_states, state != "Maine CD-2")
data_states = subset(data_states, state != "Nebraska CD-1")
data_states = subset(data_states, state != "Nebraska CD-2")
data_states = subset(data_states, state != "Nebraska CD-3")

grouped_data_grade_pollster <- group_by(data, grade, pollster) %>%
  summarise(
    numOfPolls = n(),
    clinton = mean(adjpoll_clinton),
    trump = mean(adjpoll_trump),
    clinton_minus_trump = mean(adjpoll_clinton - adjpoll_trump)
  )

grouped_data_grade <- group_by(data, grade) %>%
  summarise(
    numOfPolls = n(),
    clinton = mean(adjpoll_clinton),
    trump = mean(adjpoll_trump),
    clinton_minus_trump = mean(adjpoll_clinton - adjpoll_trump)
  )

grouped_data_state <- group_by(data_states, state) %>%
  summarise(
    numOfPolls = n(),
    clinton = mean(adjpoll_clinton),
    trump = mean(adjpoll_trump),
    clinton_minus_trump = mean(adjpoll_clinton - adjpoll_trump)
  )


polls_by_rating_pollster <- ggplot(grouped_data_grade_pollster, aes(x = grade, y = clinton_minus_trump)) +
  geom_point(aes(color = numOfPolls)) +
  stat_smooth(method = "lm")

polls_by_rating <- ggplot(grouped_data_grade, aes(x = grade, y = clinton_minus_trump)) +
  geom_point(aes(color = numOfPolls)) +
  stat_smooth(method = "lm")

grouped_data_state <- rbind(grouped_data_state, data_final)
grouped_data_state <- cbind(grouped_data_state, final = (grouped_data_state$numOfPolls == 0))



polls_by_state <- ggplot(grouped_data_state, aes(x = reorder(state, (final)*clinton_minus_trump - (!final)*clinton_minus_trump), y = as.numeric(clinton_minus_trump))) +
  geom_point(aes(color = factor(final))) + 
  stat_smooth(method = "lm") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
