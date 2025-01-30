# Assignment no. 3
# Task 3

install.packages("mclust")
install.packages("reshape2") 
install.packages("ggplot2")

library(stringi)
library(mclust)

# 1.) Path to data sets and algorithms results
     
data_path <- '/Users/thomas/Desktop/Erasmus/corsi/MATHEMATICS/PYTHON AND R/EXAM/ASSIGNMENT_3/clustering-data-v1'    # Path to ground truth labels
results_path <- '/Users/thomas/Desktop/Erasmus/corsi/MATHEMATICS/PYTHON AND R/EXAM/ASSIGNMENT_3/clustering-results-v1/original'

# 2.) Input information

sets_collection <- c('fcps', 'graves', 'sipu', 'uci')

algorithms <- c('fastcluster_average',
                'fastcluster_complete',
                'fastcluster_ward',
                'fastcluster_centroid',
                'fastcluster_median',
                'sklearn_kmeans',
                'sklearn_spectral',
                'GenieIc', 'ITM')

# 3.) Evaluate the results

# list of correct labelling
data_files <- list.files(file.path(data_path, sets_collection), 
                        recursive = TRUE,
                        full.names = TRUE, 
                        pattern = 'labels0.gz')

# vector of data names
data_names <- stri_replace_all_fixed(basename(data_files),
                                     ".labels0.gz",
                                     "")
# empty list prepared to store ARI values for each algorithm
# on each data set
r <- vector(mode = 'list', length = length(algorithms))
names(r) <- algorithms # each element will correspond to 
                       # one, particular algorithm

for(method in algorithms){ # main loop: for each method
    
    # storage for results evaluationfor this particular method
    # on each data set
    
    r[[method]] <- numeric(length(data_files))
    names(r[[method]]) <- data_names
    
    for(f in data_files){ # secondary loop: for each data set
        # lets retrieve data identifier
        data <- stri_replace_all_fixed(basename(f),
                                       ".labels0.gz",
                                       "")
        # and the name of the collection the data came from
        collection <- stri_replace_all_fixed(dirname(f),
                                             data_path,
                                             "")
        collection <- stri_replace_all_fixed(collection,
                                             "/", "")
        # We will read correct labeling 
        y <- read.csv(f, header = FALSE)[, 1]
        # and create the path to the result file
        # acording to the naming rules
        result_path <- file.path(results_path, 
                           method, 
                           collection,
                           paste(data, 
                                 '.result', 
                                 max(y), # number of groups is retrieve from correct labeling
                                 '.gz', sep = ''))
        # If it possible we will read the data
        # and calculate the ARI index of 
        # this method on this data set
        tryCatch({
            prediction <- read.csv(result_path)[, 1]
            r[[method]][data] <- adjustedRandIndex(prediction, y)
        }, error = function(e) NA) 
        # if error occurs NA will be given instead
    }
}

R <- data.frame(r) # transform into data frame


#Scumisiemo
#Transform Results into a Data Frame
R <- data.frame(r)  # Convert the list to a data frame
R$Dataset <- rownames(R)  # Add dataset names as a column

# Output the Results
print("ARI Values Computed for Each Algorithm-Dataset Pair:")
print(R)

#Save the Data Frame for Visualization
write.csv(R, "ari_results.csv", row.names = FALSE)


#HEATMAP 

#The heatmap is just for deeper inspection of algorithms scores, as it can look overwhelmed.
#Comparing algorithms across all datasets requires scanning multiple rows, which might not be straightforward for non-expert readers.
#It compares the Adjusted Rand Index (ARI) values of different clustering algorithms across all datasets.
#Each row corresponds to a dataset, and each column corresponds to an algorithm. 
#The color intensity reflects the ARI values (with darker colors representing higher ARI).
#Strenghts: 
#It provides a detailed overview of algorithm performance on individual datasets.
#Highlights datasets where specific algorithms perform well or poorly. 
#Enables quick identification of consistent performers across datasets. 

library(ggplot2)
library(reshape2)

# Melt the data frame for ggplot
R_melt <- melt(R, id.vars = "Dataset", variable.name = "Algorithm", value.name = "ARI")

# Generate the heatmap with adjusted color scale
ggplot(R_melt, aes(x = Algorithm, y = Dataset, fill = ARI)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "white",            # Very light color for low scores
    mid = "lightblue",        # Subtle blue for moderate scores
    high = "darkgreen",       # Strong green for high scores
    midpoint = 0.7,           # Highlighting scores above 0.7
    limit = c(0, 1),          # Ensure the scale covers 0 to 1
    name = "ARI"              # Label for the legend
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Adjusted Rand Index (ARI) for Clustering Algorithms",
    x = "Algorithm",
    y = "Dataset",
    fill = "ARI"
  )

# Save the heatmap
ggsave("ari_heatmap.png", width = 10, height = 6)




#BAR PLOT

#The bar plot summarizes the average ARI scores for each algorithm across all datasets.This provides a concise view of overall performance.
#It's provided with the purpose to give a insightful high-level summary, clear and easy to interpret for comparing overall algorithm effectiveness.
#Be aware it doesn't capture where an algorithm performs exceptionally well or poorly across datasets, if you are searching for it, use Heatmap.

# Calculate the mean ARI for each algorithm
R_mean <- colMeans(R[, -ncol(R)], na.rm = TRUE)  # Exclude "Dataset" column
R_mean_df <- data.frame(Algorithm = names(R_mean), Mean_ARI = R_mean)

# Generate the bar plot
ggplot(R_mean_df, aes(x = reorder(Algorithm, -Mean_ARI), y = Mean_ARI, fill = Algorithm)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Adjusted Rand Index (ARI) by Algorithm",
       x = "Algorithm",
       y = "Mean ARI") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Save the bar plot
ggsave("ari_barplot.png", width = 8, height = 5)


