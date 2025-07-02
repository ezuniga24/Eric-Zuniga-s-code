library(readr)
Statcast_2018 <- read_csv("Desktop/Statcast_2018.csv")
View(Statcast_2018)

library(readr)
Statcast_2019 <- read_csv("Desktop/Statcast_2019.csv")
View(Statcast_2019)

library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(rpart.plot)
library(nnet)

#filtering Shane Bieber and Clayton Kershaw in the data sets and keep only ten events that happened on the field
df_xy <- Statcast_2018 %>%
  filter(player_name %in% c("Bieber, Shane", "Kershaw, Clayton"),
    balls   >= 0,
    strikes >= 0,
    events  %in% c("double_play", "double",  "field_out", "home_run", "triple", "walk",   "strikeout", "single"))

#scatter plot of events that happened based off the ball strike count of the at bat
ggplot(df_xy, aes(x = balls, y = strikes, color = events)) + geom_jitter( width  = 0.2, height = 0.2, alpha  = 0.7, size   = 1.1) +
facet_wrap(~ player_name) + labs( x = "Balls", y = "Strikes", color = "Event", title = "Pitch Counts (Balls and Strikes) for the outcome on the field" ) +
theme_minimal() +
theme(
    axis.text  = element_text(size = 10),
    strip.text = element_text(face = "bold")
  ) +
  scale_color_manual(values = c(
    double_play = "#1b9e77",
    double      = "#d95f02",
    field_out   = "#7570b3",
    home_run    = "#e7298a",
    triple      = "#66a61e",
    walk        = "#e6ab02",
    strikeout   = "#a6761d",
    single      = "#666666",
    force_out   = "#a6cee3"
  ))


#getting the types of pitches that cole hamels threw for either a strike, swinging strike or ball
df_cs_selected <- Statcast_2018 %>%
  filter(player_name %in% c("Hamels, Cole"), description %in% c("called_strike", "ball", "swinging_strike"), !is.na(pitch_type)
  )

#bar plot of the types of pitches that cole hamels threw for for strike, swinging strike or ball
ggplot(df_cs_selected, aes(x = pitch_type, fill = description)) + geom_bar(position = "dodge") + facet_wrap(~ player_name) +
  labs( x     = "Pitch Type", y     = "Count", fill  = "Description", title = "Count of Called Strikes vs. Balls by Pitch Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

#Model for us to use to predict
model <- multinom(events ~ balls + strikes, data = df_xy)

#Clayton Kershaw’s rows
df_k  <- df_xy %>% filter(player_name == "Kershaw, Clayton")

#predict his events in sample
pred_k <- predict(model, newdata = df_k)

#labels to share pred_k’s levels
ref_k <- factor(df_k$events, levels = levels(pred_k))

#confusion matrix
cm_k  <- confusionMatrix(pred_k, ref_k)
print(cm_k)

#getting predicted probabilities for every row
probs <- predict(model, df_xy, type = "probs")
probs_df <- bind_cols(df_xy %>% select(player_name, balls, strikes), as.data.frame(probs))

#for each player, balls, strikes, we sum the probs to get expected counts
expected_counts <- probs_df %>%
  group_by(player_name, balls, strikes) %>%
  summarise(across(
    .cols = all_of(colnames(probs)),
    .fns  = sum,
    .names = "exp_{col}"
  ), .groups = "drop")

#rename for a wide table
exp_wide <- expected_counts %>%
  rename_with(~ sub("^exp_", "", .x)) %>%
  arrange(player_name, balls, strikes)

print(exp_wide)

#rounding the numbers that are not right like 6.15 -e 1
exp_wide_int <- exp_wide %>%
  mutate(across(where(is.numeric),     
    ~ round(.x, 0)         
  ))

print(n = 24, exp_wide_int)

#prepping the data
df <- Statcast_2018 %>%
  filter( player_name == "Hamels, Cole", description  %in% c("called_strike","ball","swinging_strike"), !is.na(pitch_type)) %>%
  select(description, pitch_type, balls, strikes) %>%
  na.omit() %>%
  mutate(across(everything(), factor))

#spliting the data between test and train
data1  <- createDataPartition(df$description, p = 0.7, list = FALSE)
train <- df[data1, ]
test  <- df[-data1, ]

#sample
ctrl <- trainControl(method="cv", number=5, sampling="up", classProbs=TRUE)
model <- train( description ~ ., data = train, method = "rpart", trControl  = ctrl,tuneLength = 10)

#predicting probabilities
probs <- predict(model, newdata = df, type = "prob")

#predict on the test set
test_pred <- predict(model, newdata = test)

#confusion matrix for accuracy 
cm <- confusionMatrix(test_pred, test$description)
print(cm)

#combining the original data frame with the predicted probabilities with pitch type and ball strike
exp_counts <- bind_cols(df, probs) %>%
  group_by(pitch_type, balls, strikes) %>%
  summarise( called_strike    = sum(called_strike), ball = sum(ball), swinging_strike  = sum(swinging_strike), .groups = "drop") %>% mutate(across( c(called_strike, ball, swinging_strike),~ round(.x, 0)
  ))

print(n = 67, exp_counts)

#decision tree
rpart.plot( model$finalModel, main = paste0("Hamels Tree (CP=", round(model$bestTune$cp,3),")")
)

#extracting the events that I would like to highlight
events_list <- c(
  "double_play","double","field_out","home_run",
  "triple","walk","strikeout","single"
)

actual_2019_data <- Statcast_2019 %>%
  filter( player_name %in% c("Kershaw, Clayton"), balls   >= 0, strikes >= 0, events  %in% events_list) %>%
  #counting how many times each event happened at each count
  count(player_name,balls,strikes,events, name = "actual_count") %>%
  #making sure every combo and every event shows up 
  complete(player_name,balls, strikes, events = events_list, fill = list(actual_count = 0)
  ) %>%
  #making sure each event is its own column
  pivot_wider( names_from  = events, values_from = actual_count)

print(actual_2019_data)


#define the three outcomes that we want
outcomes_3 <- c("called_strike","ball","swinging_strike")

#summarizing actual 2019 counts for Camels
actual_2019_hamels <- Statcast_2019 %>%
  filter( player_name == "Hamels, Cole",description  %in% outcomes_3, !is.na(pitch_type)) %>%
  #count how often each pitch_type, balls, strikes, description  occurs
  count(pitch_type,balls,strikes,description, name = "actual_count"
  ) %>%
  #making sure every combination appears 
  complete(pitch_type,balls,strikes,description = outcomes_3, fill = list(actual_count = 0)) %>%
  #pivot so each description is its own column
  pivot_wider(names_from  = description, values_from = actual_count)

print(n = 60, actual_2019_hamels)

#data sets 
#https://www.kaggle.com/datasets/s903124/mlb-statcast-data/data