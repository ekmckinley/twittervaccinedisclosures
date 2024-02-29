
##### VACCINE DISCLOSURE DATA WRANGLING #####

# Use this R script to do the following:
# 1) Combine data from all Twitter queries
# 2) Remove duplicates
# 3) Run data through NER
# 4) Remove bot accounts

library(reticulate)
library(dplyr)

###############
# 1) Combine data from all Twitter queries
###############

# Upload all of the CSV tweet files (use the files feature of R studio)

# Bind all of the tweet csvs q1-q34 into the all_tweets dataset csv #######
all_tweets <- rbind(q1_tweet, q2_tweet, q3_tweet, q4_tweet, q5_tweet, q6_tweet, q7_tweet, q8_tweet, q9_tweet, q10_tweet, q11_tweet, q12_tweet, q13_tweet, q14_tweet, q15_tweet, q16_tweet, q17_tweet, q18_tweet, q19_tweet, q20_tweet, q21_tweet, q22_tweet, q23_tweet, q24_tweet, q25_tweet, q26_tweet,q27_tweet, q28_tweet, q29_tweet, q30_tweet, q31_tweet, q32_tweet, q33_tweet, q34_tweet)

###############
# 2) Clean data (i.e. remove duplicates)
###############

# Remove duplicates based on a column, e.g. the id or text column
all_tweets <- all_tweets[!duplicated(all_tweets$text), ] 

# Write into a csv and save to your machine
write.csv(all_tweets,"YOUR FILE PATH NAME.csv", row.names = FALSE)

###############
# 3) Run data through NER
###############
py_install("transformers")
py_install("torch") # Necessary for transformers
transformers <- import("transformers")
AutoTokenizer <- transformers$AutoTokenizer
AutoModelForTokenClassification <- transformers$AutoModelForTokenClassification
tokenizer <- AutoTokenizer$from_pretrained("dbmdz/bert-large-cased-finetuned-conll03-english")
model <- AutoModelForTokenClassification$from_pretrained("dbmdz/bert-large-cased-finetuned-conll03-english")
apply_ner <- function(description) {
  tokens <- tokenizer$encode(description, return_tensors = "pt")
  predictions <- model(tokens)$logits
  predictions_indices <- which.max(predictions$array(), arr.ind = TRUE)[,1]
  labels <- tokenizer$convert_ids_to_tokens(predictions_indices - 1)
  return(labels)
}
all_tweets$user_description_ner <- sapply(all_tweets$user_description, apply_ner)

write.csv(all_tweets, "all_tweets_ner.csv", row.names = FALSE)

# Create Two Datasets Based on the Presence of an Organization Entity:
all_tweets$contains_org <- sapply(all_tweets$user_description_ner, function(ner_results) {
  any(grepl("\\[ORG\\]", ner_results)) # Adjust the pattern if necessary
})

# Split dataset based on presence of an organizational entity
tweets_with_org <- all_tweets[all_tweets$contains_org, ]
tweets_without_org <- all_tweets[!all_tweets$contains_org, ]
write.csv(tweets_with_org, "tweets_with_organization.csv", row.names = FALSE)
write.csv(tweets_without_org, "tweets_without_organization.csv", row.names = FALSE)

###############
# 4) Remove bot accounts
###############
# Filter rows where 'bot' appears in 'user_description' or 'username'
tweets_without_org_no_bots <- tweets_without_org %>%
  filter(!grepl("bot", user_description, ignore.case = TRUE) &
           !grepl("bot", username, ignore.case = TRUE))
# Save the new dataframe to a CSV file
write.csv(tweets_without_org_no_bots, "FILE PATH.csv", row.names = FALSE)



