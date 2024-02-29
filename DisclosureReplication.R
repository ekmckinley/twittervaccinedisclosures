#### FOR REPLICATION FILE


###### Raw dataset ######

raw_data <- all_tweet_users_entities
# Remove specified columns
columns_to_remove <- c("author_id", "is_individual", "conversation_id", "in_reply_to_user_id", 
                       "geo.place_id", "geo.coordinates.type", "attachments.poll.ids", 
                       "user_description", "profile_url", "username", "name", "profile_image_url")
raw_data <- raw_data[, !(names(raw_data) %in% columns_to_remove)]
# Save the dataframe as a CSV file
write.csv(raw_data, "/Users/emilymckinley/vaccination_disclosure/NEW TWITTER DATA/Replication/raw_dataset_dontshare.csv", row.names = FALSE)
# Remove the 'id' column
raw_data <- raw_data[, !(names(raw_data) == "id")]
# Save the dataframe as a CSV file
write.csv(raw_data, "/Users/emilymckinley/vaccination_disclosure/NEW TWITTER DATA/Replication/raw_dataset_share.csv", row.names = FALSE)

###### Clean dataset ######

clean_dataset <- final_contentcoded_datasetWITHEMOTION
# Remove specified columns
columns_to_remove2 <- c("followers_divided1000","following_divided1000", "text_nourls", "cleanedtext_sentanlys")
clean_dataset <- clean_dataset[, !(names(clean_dataset) %in% columns_to_remove2)]
# Set the seed for replicability
set.seed(123)
# Assign unique random numbers to each row
clean_dataset$assigned_tweetID <- sample.int(n = nrow(raw_data), size = nrow(clean_dataset), replace = FALSE)
# Save the dataframe as a CSV file
write.csv(clean_dataset, "/Users/emilymckinley/vaccination_disclosure/NEW TWITTER DATA/Replication/clean_dataset_dontshare.csv", row.names = FALSE)
# Remove the 'id' column
clean_dataset <- clean_dataset[, !(names(clean_dataset) == "id")]
# Save the dataframe as a CSV file
write.csv(clean_dataset, "/Users/emilymckinley/vaccination_disclosure/NEW TWITTER DATA/Replication/clean_dataset_share.csv", row.names = FALSE)
