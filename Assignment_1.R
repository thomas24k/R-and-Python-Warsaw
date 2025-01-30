Badges<- read.csv("/Users/thomas/RStudio/Assignment_1/Badges.csv")
Tags<- read.csv("/Users/thomas/RStudio/Assignment_1/Tags.csv")
Votes<- read.csv("/Users/thomas/RStudio/Assignment_1/Votes.csv")

install.packages("microbenchmark")
library(microbenchmark)
install.packages("waldo")
library(waldo)


base_solution_1 <- function() {
  # Step 1: Filter duplicated posts based on LinkTypeId in PostLinks (assuming 3 signifies duplicate)
  duplicated_posts <- subset(PostLinks, LinkTypeId == 3)
  #head(duplicated_posts)
  
  
  # Step 2: Count the number of duplicates per PostId
  # Join with Posts to get OwnerUserId for each duplicated post
  duplicate_counts <- merge(duplicated_posts, Posts, by.x = "PostId", by.y = "Id")
  duplicate_counts <- duplicate_counts[!is.na(duplicate_counts$OwnerUserId), ]  # Exclude rows without OwnerUserId
  #head(duplicate_posts)
  
  # Count duplicates per user
  #FUN = length: apply  length function to PostId column within each group of OwnerUserId.
  #count the number of occurrences of PostId for each OwnerUserId
  user_duplicates <- aggregate(PostId ~ OwnerUserId, data = duplicate_counts, FUN = length)
  #head(user_duplicates)
  names(user_duplicates) <- c("UserId", "DuplicatePostCount") #rename column of user_duplicates
  #head(user_duplicates)
  
  # Step 3: Count total questions, answers, and comments for each user
  # PostTypeId == 1 = question, 2 = answer 
  
  # Count total questions per user
  #FUN = length: apply  length function to Id column within each group of OwnerUserId.
  #count the number of occurrences of PostId for each OwnerUserId
  questions_count <- aggregate(Id ~ OwnerUserId, data = subset(Posts, PostTypeId == 1), FUN = length)
  names(questions_count) <- c("UserId", "TotalQuestions")
  
  # Count total answers per user
  answers_count <- aggregate(Id ~ OwnerUserId, data = subset(Posts, PostTypeId == 2), FUN = length)
  names(answers_count) <- c("UserId", "TotalAnswers")
  
  # Sum up all comments per user (if CommentCount represents the comment count per post)
  #CommentCount column to be aggregated, OwnerUserId  grouping variable
  #= CommentCount values will be summed for each unique OwnerUserId
  comments_count <- aggregate(CommentCount ~ OwnerUserId, data = Posts, FUN = sum, na.rm = TRUE)
  names(comments_count) <- c("UserId", "TotalComments")
  
  
  # Step 4: Merge these counts with user details in Users
  user_stats <- merge(Users, user_duplicates, by.x = "Id", by.y = "UserId", all.x = TRUE)
  user_stats <- merge(user_stats, questions_count, by.x = "Id", by.y = "UserId", all.x = TRUE)
  user_stats <- merge(user_stats, answers_count, by.x = "Id", by.y = "UserId", all.x = TRUE)
  user_stats <- merge(user_stats, comments_count, by.x = "Id", by.y = "UserId", all.x = TRUE)
  
  # Replace NA with 0 for counts where no data exists, maybe unuseful as doesn't required 
  user_stats$DuplicatePostCount[is.na(user_stats$DuplicatePostCount)] <- 0
  user_stats$TotalQuestions[is.na(user_stats$TotalQuestions)] <- 0
  user_stats$TotalAnswers[is.na(user_stats$TotalAnswers)] <- 0
  user_stats$TotalComments[is.na(user_stats$TotalComments)] <- 0
  
  # Step 5: Order by highest DuplicatePostCount and select top 10 users
  top_users <- user_stats[order(-user_stats$DuplicatePostCount), ]
  top_users <- head(top_users, 10)
  
  # Step 6: Select and display the required columns
  #I intended "score" not as the score of the post but the asses of the Users so "reputation"
  result <- top_users[, c("Id", "DisplayName", "TotalQuestions", "TotalAnswers", "TotalComments", "Reputation", "DuplicatePostCount")]
  return(result)
}


#------------------------------------------------------------------------------------

install.packages("dplyr")
library(dplyr)


dplyr_solution_1 <- function() { 
  # Step 1: Filter duplicated posts based on LinkTypeId in PostLinks (assuming 3 signifies duplicate)
  duplicated_posts <- PostLinks %>% #pipe operator to pass datas
    filter(LinkTypeId == 3)
  
  # Step 2: Join with Posts to get OwnerUserId for each duplicated post, then count duplicates per user
  user_duplicates <- duplicated_posts %>% #to call later duplicated_posts
    inner_join(Posts, by = c("PostId" = "Id")) %>% #inner_join just keep equal in both
    filter(!is.na(OwnerUserId)) %>% #remove Owner = na
    group_by(OwnerUserId) %>%
    summarise(DuplicatePostCount = n()) 
  
  # Step 3: Count total questions, answers, and comments for each user
  questions_count <- Posts %>%
    filter(PostTypeId == 1) %>%
    group_by(OwnerUserId) %>%
    summarise(TotalQuestions = n())
  
  answers_count <- Posts %>%
    filter(PostTypeId == 2) %>%
    group_by(OwnerUserId) %>%
    summarise(TotalAnswers = n())
  
  comments_count <- Posts %>%
    group_by(OwnerUserId) %>%
    summarise(TotalComments = sum(CommentCount, na.rm = TRUE))
  
  # Step 4: Merge these counts with user details in Users, and replace NA with 0 for missing values
  user_stats <- Users %>%
    rename(UserId = Id) %>%
    #left_join=matching values in specified columns just when 2 dataframes has equal rows
    left_join(user_duplicates, by = c("UserId" = "OwnerUserId")) %>%
    left_join(questions_count, by = c("UserId" = "OwnerUserId")) %>%
    left_join(answers_count, by = c("UserId" = "OwnerUserId")) %>%
    left_join(comments_count, by = c("UserId" = "OwnerUserId")) %>%
    #mutate to change and ifelse to check if I have NA put 0
    mutate( 
      DuplicatePostCount = ifelse(is.na(DuplicatePostCount), 0, DuplicatePostCount),
      TotalQuestions = ifelse(is.na(TotalQuestions), 0, TotalQuestions),
      TotalAnswers = ifelse(is.na(TotalAnswers), 0, TotalAnswers),
      TotalComments = ifelse(is.na(TotalComments), 0, TotalComments)
    )
  
  # Step 5: Order by highest DuplicatePostCount and select top 10 users
  top_users <- user_stats %>%
    arrange(desc(DuplicatePostCount)) %>% #descending order
    slice_head(n = 10) #sort first 10
  
  # Step 6: Select and display the required columns, renaming Reputation to Score
  result <- top_users %>%
    select(UserId, DisplayName, TotalQuestions, TotalAnswers, TotalComments, Reputation, DuplicatePostCount)
  
  # Display the result
  return(result)
}

#------------------------------------------------------------------------------------


# Run and compare execution times
comparison <- microbenchmark(
  base_solution_1 = base_solution_1(),
  dplyr_solution_1 = dplyr_solution_1(),
  times = 10
)

print(comparison)

# Check if results are equivalent
base_result_1 <- base_solution_1()
dplyr_result_1 <- dplyr_solution_1()

# Confirm equivalency of rows and columns
#identical(base_result_1, as.data.frame(dplyr_result_1))
#identical changed in compare

compare(base_result_1, as.data.frame(dplyr_result_1))


#add table of times
#Unit: milliseconds
#expr      min       lq     mean   median       uq      max neval
#base_solution_1 432.9762 438.0413 463.0817 460.4852 464.9088 553.2472    10
#dplyr_solution_1 163.5167 165.2453 167.6289 167.1491 169.3469 172.3284    10
 

#---------------------------------------------------------------------------------------------------


#Task 2
# Base R

base_solution_2 <- function() {
  # Convert CreationDate to POSIXct and extract hour
  Posts$CreationDate <- as.POSIXct(Posts$CreationDate, format = "%Y-%m-%dT%H:%M:%OS")
  Posts$Hour <- as.numeric(format(Posts$CreationDate, "%H")) #take hours
  
  # Filter posts created between 8 PM (20) and 6 AM (5)
  night_posts <- subset(Posts, (Hour >= 20 | Hour < 6) & !is.na(OwnerUserId))
  
  # Count posts per location by joining with Users data
  merged_data <- merge(night_posts, Users, by.x = "OwnerUserId", by.y = "Id", all.x = TRUE)
  
  # Filter to keep only Location and count per location
  location_counts <- as.data.frame(table(merged_data$Location))
  names(location_counts) <- c("Location", "PostCount")
  
  # Sort by PostCount in descending order
  location_counts <- location_counts[order(-location_counts$PostCount), ]
  
  return(location_counts)
}

#---------------------------------------------------------------------------------------------------
# dplyr solution

library(dplyr)

dplyr_solution_2 <- function() {
  # Convert CreationDate to POSIXct and extract hour
  Posts <- Posts %>%
    mutate(CreationDate = as.POSIXct(CreationDate, format = "%Y-%m-%dT%H:%M:%OS"),
           Hour = as.numeric(format(CreationDate, "%H"))) 
  
  # Filter posts created between 8 PM (20) and 6 AM (5), and join with Users
  night_posts <- Posts %>%
    filter((Hour >= 20 | Hour < 6) & !is.na(OwnerUserId)) %>%
    inner_join(Users, by = c("OwnerUserId" = "Id")) %>%
    group_by(Location) %>%
    summarise(PostCount = n()) %>%
    arrange(desc(PostCount))
  
  return(night_posts)
}

#---------------------------------------------------------------------------------------------------

# Run and compare execution times
comparison <- microbenchmark(
  base_solution_2 = base_solution_2(),
  dplyr_solution_2 = dplyr_solution_2(),
  times = 10
)

print(comparison)

# Check if results are equivalent
base_result_2 <- base_solution_2()
dplyr_result_2 <- dplyr_solution_2()

# Confirm equivalency of rows and columns
identical(base_result_2, as.data.frame(dplyr_result_2))
#identical changed in compare
compare(base_result_2, as.data.frame(dplyr_result_2))

#add table of times
#Unit: milliseconds
#expr      min       lq     mean   median       uq      max neval
#base_solution_2 60.53133 60.88385 70.56453 63.33817 86.07560 88.46312    10
#dplyr_solution_2 37.72599 38.63159 51.81606 42.46774 67.30334 77.01969    10
