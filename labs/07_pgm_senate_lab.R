library(tidyverse)

# Read in data -----------------------------------------------------------------
name <- read_csv("data/senate_data/senate109/name.csv",
                 col_names = FALSE)
party <- read_csv("data/senate_data/senate109/party.csv",
                  col_names = FALSE)
votes <- read_csv("data/senate_data/senate109/votes.csv")

# Clean data -------------------------------------------------------------------
senators <- data.frame(senator = name$X1,
                       party = party$X1)

# Replace all -1 with 0
# Note that 0s in the data are also coded as 0
vote_df <- as.data.frame(t(votes)) %>% 
  mutate_all(~ ifelse(. == -1, 0, .))
colnames(vote_df) <- name$X1[-1] # removing Bush

# Logistic regression ----------------------------------------------------------
# Predict the first senator using the other 100
log_reg_sessions <- glm(SESSIONS ~ ., data = vote_df, family = binomial)
summary(log_reg_sessions)

log_reg_obama <- glm(OBAMA ~ ., data = vote_df, family = binomial)
summary(log_reg_obama)
