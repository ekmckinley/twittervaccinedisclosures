
##### REGRESSION SCRIPT #####

# Use this R script to do the following:
# 1) Check for overdispersion and excess zeros
# 2) Negative Binomial Regression with Log Transformation 
# 3) Correlation Matrix
# 4) Negative binomial: The number of content types within a tweet 

library(tidyverse)
library(dplyr)
library(MASS) # for negative binomial regression
library(foreign)
library(ggplot2)
library(performance)

###############
# 1) Check for overdispersion and excess zeros
###############

#Load in the dataset using the files feature in R Studio

# Check for overdispersion
var_engagement <- var(final_contentcoded_dataset$total_engagement_score,y = NULL, na.rm = FALSE)
mean_engagement <- mean(final_contentcoded_dataset$total_engagement_score)
cat("Variance for total_engagement_score:", var_engagement, "\n") 
cat("Mean for total_engagement_score:", mean_engagement, "\n") 
# Poisson regression
poisson_model <- glm(total_engagement_score ~ civicduty_label + gratitude_label + political_label + relief_label + encouragingvaccination_label + community_label + sideeffects_label + socialbenefits_label + humorous_label + healthbenefits_label + vaccexperience_label + total_benefits + total_experience, data = final_contentcoded_dataset, family = poisson())
summary(poisson_model)
# Overdispersion check
poisson_dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
print(poisson_dispersion)
# Excess Zeros Check
check_zeroinflation(nbmodel_all)


###############
# 2) Negative Binomial Regression with Log Transformation 
###############

# Convert the date column to a date format
final_contentcoded_dataset$created_at_date_only <- as.Date(final_contentcoded_dataset$created_at_date_only, format = "%Y-%m-%d")         
#Apply log transformation on controls (using log1p which is log(x+1))
final_contentcoded_dataset$public_metrics.tweet_count_log <- log1p(final_contentcoded_dataset$public_metrics.tweet_count)
final_contentcoded_dataset$public_metrics.followers_count_log <- log1p(final_contentcoded_dataset$public_metrics.followers_count)
final_contentcoded_dataset$public_metrics.following_count_log <- log1p(final_contentcoded_dataset$public_metrics.following_count)
final_contentcoded_dataset$word_count_nohashtags_log <- log1p(final_contentcoded_dataset$word_count_nohashtags)
final_contentcoded_dataset$hashtag_count_log <- log1p(final_contentcoded_dataset$hashtag_count)
final_contentcoded_dataset$word_count_log <- log1p(final_contentcoded_dataset$word_count)

# Estimate the Negative Binomial model using the transformed variables:
nb_model_log <- glm.nb(total_engagement_score ~ civicduty_label + gratitude_label + political_label + relief_label + encouragingvaccination_label + 
                         community_label + sideeffects_label + socialbenefits_label + humorous_label + healthbenefits_label + vaccexperience_label + 
                         media_present + public_metrics.tweet_count_log + useraccount_ageinweeks + public_metrics.followers_count_log + public_metrics.following_count_log + 
                         word_count_nohashtags_log + hashtag_count_log + created_at_date_only + sentiment_scores,
                       data = final_contentcoded_dataset)
summary(nb_model_log)

# Subset the data to include only IVs and DV
data_subset <- final_contentcoded_dataset[, c("total_engagement_score", "civicduty_label", "gratitude_label", "political_label", 
                                              "relief_label", "encouragingvaccination_label", "community_label", 
                                              "sideeffects_label", "socialbenefits_label", "humorous_label", 
                                              "healthbenefits_label", "vaccexperience_label", "media_present", 
                                              "public_metrics.tweet_count_log", "useraccount_ageinweeks", 
                                              "public_metrics.followers_count_log", "public_metrics.following_count_log", 
                                              "word_count_nohashtags_log", "hashtag_count_log")]
###############
# 3) Correlation Matrix
###############
# Compute the correlation matrix
cor_matrix <- cor(data_subset, use = "complete.obs")
# Round the matrix for readability
cor_matrix <- round(cor_matrix, 2)
# Print the correlation matrix
print(cor_matrix)
#save as csv
getwd()
write.csv(cor_matrix, "correlation_matrix.csv")
## Visualize the plot
library(corrplot)
cor_matrix <- cor(data_subset, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         diag = FALSE, tl.col = "black", tl.srt = 45)


print(cor_matrix)

###############
# 4) Negative binomial: The number of content types within a tweet 
###############
# Column names
binary_cols <- c("civicduty_label", "gratitude_label", "political_label", 
                 "relief_label", "encouragingvaccination_label", "community_label", 
                 "sideeffects_label", "socialbenefits_label", "humorous_label", 
                 "healthbenefits_label", "vaccexperience_label")

# Create the total_content_count column by summing the rows for the selected columns
final_contentcoded_dataset$total_content_count <- rowSums(final_contentcoded_dataset[, binary_cols])
# Count occurrences of each unique value in the total_content_count column
count_values <- table(final_contentcoded_dataset$total_content_count)
unique(final_contentcoded_dataset$total_content_count)
# Print the results
print(count_values)
### NBM
model <- glm.nb(total_engagement_score ~ total_content_count +media_present + public_metrics.tweet_count_log + useraccount_ageinweeks + public_metrics.followers_count_log + public_metrics.following_count_log + 
                  word_count_nohashtags_log + hashtag_count_log + created_at_date_only + sentiment_scores,
                data = final_contentcoded_dataset)
summary(model)

  