#DONE WITH R 

Posts <- read.csv("/Users/thomas/RStudio/Assignment_1/Posts.csv")
Users<- read.csv("/Users/thomas/RStudio/Assignment_1/Users.csv")
Badges<- read.csv("/Users/thomas/RStudio/Assignment_1/Badges.csv")
Tags<- read.csv("/Users/thomas/RStudio/Assignment_1/Tags.csv")
Votes<- read.csv("/Users/thomas/RStudio/Assignment_1/Votes.csv")

install.packages("microbenchmark")
library(microbenchmark)
install.packages("waldo")
library(waldo)

# Merge Posts and Users on userID
merged_data <- merge(Posts, Users, by.x = "OwnerUserId", by.y = "Id", all.x = TRUE)

# Filter users who have provided their location (non-empty 'Location' field)
users_with_location <- merged_data[merged_data$Location != "" & !is.na(merged_data$Location), ]

# Ensure 'CommentCount' is numeric
users_with_location$CommentCount <- as.numeric(users_with_location$CommentCount)

# Sum up the 'CommentCount' for each user
total_comments <- aggregate(CommentCount ~ OwnerUserId, data = users_with_location, sum)

# Get unique user information
user_info <- unique(users_with_location[, c("OwnerUserId", "DisplayName", "Location")])

# Merge the total comments with user information
final_result <- merge(total_comments, user_info, by = "OwnerUserId")

# Rearrange columns 
final_result <- final_result[, c("OwnerUserId", "DisplayName", "Location", "CommentCount")]

# View the result
print(final_result)




#COMPARISON IN R 

 
#library(compare)

# Read the CSV of Python
python_result <- read.csv("/Users/thomas/total_comments_by_user.csv")

# Ensure that the column types are the same
python_result$OwnerUserId <- as.numeric(python_result$OwnerUserId)
python_result$CommentCount <- as.numeric(python_result$CommentCount)

# Use Compare function (is easier) to compare the R and Python result 
comparison <- compare(final_result, python_result)

# Print the comparison result
print(comparison)
  




