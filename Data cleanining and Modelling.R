library(tidyverse)

# User
user <- read_csv("User (1).csv")
user[1] <- NULL
summary(user)

# Seems all users are required to register with a gmail. Some emails are not given in the right format. We set these emails as NA
user$Email <- ifelse(str_detect(user$Email, "gmail"), user$Email, NA)

# Session
session <- read_csv("Session (1).csv")
summary(session)
session[1] <- NULL
session$Device <- factor(session$Device)

# Reaction Types
reaction_types <- read_csv("ReactionTypes (1).csv")
reaction_types[1] <- NULL
summary(reaction_types)
reaction_types$Type <- factor(reaction_types$Type)
reaction_types$Sentiment <- factor(reaction_types$Sentiment)

# Content
content <- read_csv("content (1).csv")
content[1] <- NULL
summary(content)
content$Type <- factor(content$Type)
content$Category <- factor(content$Category)

# Reactions
reactions <- read_csv("Reactions (1).csv")
summary(reactions)
reactions[1] <- NULL
reactions$Type <- factor(reactions$Type)

# Location
location <- read_csv("Location (1).csv")
summary(location)
location[1] <- NULL

# Profile
profile <- read_csv("Profile (1).csv")
summary(profile)
profile[1] <- NULL

# Remove special characters from the Interests column and replacing numbers with NA since the numbers don't have any meaning in the column
profile$Interests <- gsub("'", "", as.character(profile$Interests))
profile$Interests <- gsub("\\[", "", as.character(profile$Interests))
profile$Interests <- gsub("\\]", "", as.character(profile$Interests))
profile$Interests <- ifelse(grepl("\\d", profile$Interests), NA, profile$Interests)

content$Category <- gsub("'", "", as.character(content$Category))

# Merge user, location, session and profile data frames to create user profile data frame
user_profile <- list(user, location, session, profile)
user_profile <- user_profile %>%
  reduce(full_join, by = "User ID")


# Merge reaction and reaction types data frames
user_reactions <- inner_join(reactions, reaction_types, by = "Type")

# Rename the Type columns in user_reactions and content to differentiate
user_reactions <- user_reactions %>%
  rename(`Reaction Type` = Type)

content <- content %>%
  rename(`Content Type` = Type)

# Drop the User ID column in user reaction data frame
user_reactions$`User ID` <- NULL

# Drop the URL column in content data frame
content$URL <- NULL

# Merge user_reactions and content data frames
content_reactions <- left_join(user_reactions, content, by = "Content ID")

# Create csv file
write_csv(content_reactions, "Social_Buzz_Contents.csv")


# The top 5 categories with respect to popularity
top_5 <- content_reactions %>%
  group_by(Category) %>%
  summarise(Score = sum(Score)) %>%
  arrange(desc(Score)) %>%
  slice(1:5)

# Create csv file
write_csv(top_5, "top_5.csv")
