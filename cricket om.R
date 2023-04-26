# read the CSV file and store it in a data frame called "data"
data <- read.csv("dataset1_test.csv", header = TRUE)

# allocate the data to a variable called "matches"
matches <- data
Records= data.frame(matches
                    )
# print the first few rows of the data frame to verify that it was read correctly
head(matches)

# read the CSV file and store it in a data frame called "data"
data <- read.csv("dataset1_test.csv", header = TRUE)

# allocate the data to a variable called "matches"
matches <- data

# print the first few rows of the data frame to verify that it was read correctly
head(matches)

data <- read.csv("dataset1_test.csv", header = TRUE)
nrow(data) # check the number of rows in the data frame
sum(is.na(data$Batsman)) # check the number of missing values in the Batsman column
sum(is.na(data$Runs)) # check the number of missing values in the Runs column
sum(is.na(data$Batsman)) # check the number of missing values in the Batsman column
sum(is.na(data$Runs)) # check the number of missing values in the Runs column
# repeat for all other columns
str(data$Batsman) # check the data type of the Batsman column
str(data$Runs) # check the data type of the Runs column
# repeat for all other columns
any(duplicated(data)) # check if there are any duplicate rows in the data frame
# repeat for all other columns

data <- read.csv("dataset1_test.csv")

# print the column names
colnames(data)

# read the dataset
data <- read.csv("dataset1_test.csv", header = TRUE)

# split the records by player name
player_records <- split(data, data$Batsman)

batsman1 <- Records[Records$Batsman == 1,]
batsman2 <- Records[Records$Batsman == 2,]
batsman3 <- Records[Records$Batsman == 3,]
batsman4 <- Records[Records$Batsman == 4,]
batsman5 <- Records[Records$Batsman == 5,]
batsman6 <- Records[Records$Batsman == 6,]

# read the dataset
data <- read.csv("dataset1_test.csv", header = TRUE)

# calculate the average runs by player
avg_runs <- aggregate(Runs ~ Batsman, data, mean)

# print the average runs for each player
print(avg_runs)

# read the dataset
data <- read.csv("dataset1_test.csv", header = TRUE)

# calculate the average runs by player
avg_runs <- aggregate(Runs ~ Batsman, data, mean)

# plot a bar graph of the average runs for each player
barplot(avg_runs$Runs, names.arg = avg_runs$Batsman, xlab = "Player", ylab = "Average Runs", main = "Average Runs by Player")

# read the dataset
data <- read.csv("dataset1_test.csv", header = TRUE)

# calculate the total runs, fours, and sixes for each batsman
batsman_stats <- aggregate(cbind(Runs, fours, sixes) ~ Batsman, data, sum)

# create a scatter plot of fours vs. sixes with size representing total runs
plot(batsman_stats$fours, batsman_stats$sixes, xlab = "Fours", ylab = "Sixes", main = "Fours vs. Sixes",
     cex = batsman_stats$Runs/50, col = "blue", pch = 19)

# Calculate runs from fours and sixes
batsman_stats$Runs_from_fours <- batsman_stats$fours * 4
batsman_stats$Runs_from_sixes <- batsman_stats$sixes * 6

# Plot the results in separate bar graphs
library(ggplot2)

# Plot for total runs
ggplot(batsman_stats, aes(x = Batsman, y = Runs)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Runs), vjust = -0.5) +
  ggtitle("Batsman total runs") +
  xlab("Batsman") + ylab("Runs") +
  scale_y_continuous(limits = c(0, max(batsman_stats$Runs) * 1.1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Plot for runs from fours
ggplot(batsman_stats, aes(x = Batsman, y = Runs_from_fours)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = Runs_from_fours), vjust = -0.5) +
  ggtitle("Batsman runs from fours") +
  xlab("Batsman") + ylab("Runs") +
  scale_y_continuous(limits = c(0, max(batsman_stats$Runs_from_fours) * 1.1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Plot for runs from sixes
ggplot(batsman_stats, aes(x = Batsman, y = Runs_from_sixes)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_text(aes(label = Runs_from_sixes), vjust = -0.5) +
  ggtitle("Batsman runs from sixes") +
  xlab("Batsman") + ylab("Runs") +
  scale_y_continuous(limits = c(0, max(batsman_stats$Runs_from_sixes) * 1.1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Load the necessary packages
library(ggplot2)

# Create a data frame with the data
bat_positions <- data.frame(
  batsman = c("batman1", "batman2", "batsman3", "batsman4", "batsman5", "batsman6"),
  position1 = c(1, 0, 0, 0, 0, 0),
  position2 = c(0, 1, 0, 0, 0, 0),
  position3 = c(0, 0, 1, 0, 0, 0),
  position4 = c(0, 0, 0, 1, 0, 0),
  position5 = c(0, 0, 0, 0, 1, 0),
  position6 = c(0, 0, 0, 0, 0, 1)
)

# Create the heatmap using ggplot2
ggplot(data = bat_positions, aes(x = batsman, y = factor(1), fill = position1)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "orange") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("") + ylab("") +
  labs(fill = "Position")

# Load required library
library(ggplot2)

# Calculate basic statistics
runs_mean <- mean(batsman1$Runs)
runs_median <- median(batsman1$Runs)
runs_min <- min(batsman1$Runs)
runs_max <- max(batsman1$Runs)

# Print the statistics
cat(sprintf("Batsman 1 Statistics:\nMean Runs: %.2f\nMedian Runs: %d\nMinimum Runs: %d\nMaximum Runs: %d\n", runs_mean, runs_median, runs_min, runs_max))

# Plot the distribution of runs
ggplot(batsman1, aes(x=Runs)) + 
  geom_histogram(fill="red", color="#e9ecef", alpha=0.9) +
  labs(title="Batsman 1 - Runs Distribution", x="Runs", y="Count")

# Subset the data for batsman 1
batsman1 <- Records[Records$Batsman == 1, ]

# Calculate cumulative sum of runs
batsman1$CumulativeRuns <- cumsum(batsman1$Runs)

# Plot a line graph with Runs on x-axis and Cumulative Runs on y-axis
plot(batsman1$Runs, batsman1$CumulativeRuns, type = "l", xlab = "Runs", ylab = "Cumulative Runs")

# Add a title to the plot
title(main = "Batsman 1 Data Analysis")

# Subset the data for batsman 1
batsman1 <- subset(Records, Batsman == 1)

# Calculate the total runs by batsman 1 in each match
total_runs <- aggregate(Runs ~ Inns, data = batsman1, sum)

# Calculate the number of matches played by batsman 1
num_matches <- length(unique(batsman1$Innings))

# Calculate the average run per match by batsman 1
avg_run_per_match <- sum(total_runs$Runs) / num_matches

# Print the result
cat("Average run per match by Batsman 1:", round(avg_run_per_match, 2))

# filter records for Batsman 1
batsman1_records <- subset(Records, Batsman == 1)

# calculate total runs scored by Batsman 1
batsman1_runs <- sum(batsman1_records$Runs)

# print the total runs scored by Batsman 1
cat("Batsman 1 total runs:", batsman1_runs)

# Subset the data to include only records of Batsman 1
batsman1 <- subset(Records, Batsman == 1)

# Calculate the average runs of Batsman 1
avg_runs <- mean(batsman1$Runs)

# Print the result
cat("The average runs of Batsman 1 is:", avg_runs)

# Subset the records for Batman 1
batsman1 <- Records[Records$Batsman == 1, ]

# Find the total runs scored by Batman 1
total_runs <- sum(batsman1$Runs)

# Print the total runs scored by Batman 1
cat("Total runs scored by Batman 1: ", total_runs, "\n")

# Read the dataset into a data frame
Records <- data.frame(matches)

# Read the dataset
data <- read.csv("dataset1_test.csv")

# Filter the data for batsman 1
batsman1 <- subset(data, Batsman == "Batman1")

# Calculate the average runs of batsman1
avg_runs <- mean(batsman1$Runs)

# Print the average runs of batsman1
cat("Average runs of Batman1: ", round(avg_runs,2))

# Filter the data for batsman 1
batsman1 <- subset(Records, Batsman == 1)

# Calculate the cumulative runs and matches for each match played by batsman 1
batsman1$CumulativeRuns <- cumsum(batsman1$Runs)
batsman1$MatchesPlayed <- 1:nrow(batsman1)

# Calculate the average runs per match for batsman 1
batsman1$AverageRunsPerMatch <- batsman1$CumulativeRuns / batsman1$MatchesPlayed

# Plot the runs and average runs per match for batsman 1
plot(batsman1$MatchesPlayed, batsman1$CumulativeRuns, type = "l", col = "blue", xlab = "Matches Played", ylab = "Runs Scored", main = "Runs Scored by Batsman 1")
lines(batsman1$MatchesPlayed, batsman1$AverageRunsPerMatch, type = "l", col = "red")
legend("topleft", legend = c("Runs Scored", "Average Runs per Match"), col = c("blue", "red"), lty = 1)

# Calculate total runs from boundaries and singles
total_runs_boundary <- sum(Records$fours*4) + sum(Records$sixes*6)
total_runs_singles <- sum(Records$Runs) - total_runs_boundary

# Create a data frame for pie chart
runs_data <- data.frame(Run_type = c("4s", "6s", "Singles"),
                        Runs = c(sum(Records$fours*4), sum(Records$sixes*6), total_runs_singles))

# create a vector with the number of runs scored in each category
runs <- c(fours = 100, sixes = 200, singles = 400)

# calculate the percentage for each category
percentages <- round(100 * runs / sum(runs), 1)

# create the pie chart
pie(runs, labels = paste0(names(runs), ": ", percentages, "%"), col = c("orange", "red", "green"), main = "Runs Scored", clockwise = TRUE)

# subset the dataset to include only records of Batman 1
batsman1 <- Records[Records$Batsman == 1,]

# count the number of occurrences of each type of dismissal for Batman 1
dismissal_counts <- table(batsman1$Dismissal)

# extract counts for specific types of dismissal
lbw_count <- dismissal_counts['lbw']
bowled_count <- dismissal_counts['bowled']
run_out_count <- dismissal_counts['run out']
caught_count <- dismissal_counts['caught']
stumped_count <- dismissal_counts['stumped']
not_out_count <- dismissal_counts['not out']

# read the dataset
data <- read.csv("dataset1_test.csv")

# subset the data for batsman 1
batsman1 <- subset(data, Batsman == "Batsman1")

# get unique positions
positions <- unique(batsman1$Position)

# loop through each position and each batsman at that position
for(pos in positions){
  # subset the data for the current position
  pos_data <- subset(batsman1, Position == pos)
  # get unique batsmen at this position
  batsmen <- unique(pos_data$Batsman)
  # loop through each batsman and get the average runs scored
  for(batsman in batsmen){
    # subset the data for the current batsman
    batsman_data <- subset(pos_data, Batsman == batsman)
    # calculate the average runs scored
    avg_runs <- mean(batsman_data$Runs)
    # print the result
    cat("Batsman:", batsman, "at Position:", pos, "Average Runs Scored:", round(avg_runs, 2), "\n")
  }
}
# Read the dataset into a data frame
data <- read.csv("dataset1_test.csv")

# Find the positions where each batsman played
batsman1_pos <- which(data$Batsman1 != "")
batsman2_pos <- which(data$Batsman2 != "")
batsman3_pos <- which(data$Batsman3 != "")
batsman4_pos <- which(data$Batsman4 != "")
batsman5_pos <- which(data$Batsman5 != "")
batsman6_pos <- which(data$Batsman6 != "")

# Print the positions where each batsman played
cat("Batsman1 played at positions:", batsman1_pos, "\n")
cat("Batsman2 played at positions:", batsman2_pos, "\n")
cat("Batsman3 played at positions:", batsman3_pos, "\n")
cat("Batsman4 played at positions:", batsman4_pos, "\n")
cat("Batsman5 played at positions:", batsman5_pos, "\n")
cat("Batsman6 played at positions:", batsman6_pos, "\n")

# read the dataset
dataset <- read.csv("dataset1_test.csv")

# extract unique batsmen names
batsmen <- unique(dataset$Batsman)

# loop over each batsman and find the positions played
for (i in 1:length(batsmen)) {
  current_batsman <- batsmen[i]
  positions <- unique(dataset$Position[dataset$Batsman == current_batsman])
  print(paste0("Batsman ", i, " (", current_batsman, ") played at positions: ", paste(positions, collapse=", ")))
}
# read the dataset
data <- read.csv("dataset1_test.csv")

# extract the "Position" column
position <- data$Position

# print the position data
print(position)

# Read the dataset
data <- read.csv("dataset1_test.csv")

# Calculate the strike rate of each batsman
SR <- aggregate(Runs ~ Batsman, data, function(x) {
  total_balls <- sum(data$Batsman == x)
  total_runs <- sum(data$Runs[data$Batsman == x])
  strike_rate <- (total_runs / total_balls) * 100
  return(strike_rate)
})

# Print the strike rate of each batsman
print(SR)

# subset data for Batman 1
batsman1_data <- Records[Records$Batsman == 1, ]

# count dismissals for each type
dismissals_count <- table(batsman1_data$Dismissal)

# plot pie chart
pie(dismissals_count, main = "Dismissals for Batman 1")

batman1_index <- which(Records$Batsman == 1)
batman1_position <- Records$Position[batman1_index]

# Subset the data to include only batman 1 at position 3
batsman1_pos3 <- Records[(Records$Batsman == 1) & (Records$position == 3),]

# Count the number of matches played by batman 1 at position 3
num_matches <- nrow(batsman1_pos3)

# Print the result
cat("Batman 1 played", num_matches, "matches at position 3.")

# Subset the data to include only batman 1 at position 4
batsman1_pos4 <- Records[(Records$Batsman == 1) & (Records$position == 4),]

# Count the number of matches played by batman 1 at position 4
num_matches <- nrow(batsman1_pos4)

# Print the result
cat("Batman 1 played", num_matches, "matches at position 4.")

# repeat for all other columns
any(duplicated(data)) # check if there are any duplicate rows in the data frame
# repeat for all other columns
# calculate the average strike rate for each position
avg_sr_pos3 <- mean(batsman1_pos3$Strike_Rate)
avg_sr_pos4 <- mean(batsman1_pos4$Strike_Rate)

# print the results
cat("Average Strike Rate of Batman 1 at Position 3:", round(avg_sr_pos3, 2), "\n")
cat("Average Strike Rate of Batman 1 at Position 4:", round(avg_sr_pos4, 2), "\n")

# Load required libraries
library(ggplot2)

# Read the dataset
data <- read.csv("dataset1_test.csv")

# Filter the data for the required columns
data <- subset(data, select = c(Runs, position, SR))

# Run linear regression analysis
model <- lm(Runs ~ position + SR, data = data)

# Plot a scattered diagram
ggplot(data, aes(x = position, y = Runs, color = SR)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# iii.	Plot the scatter plot and draw the regression.
scatter.smooth(data$Runs~ data$position)

# load the dataset
data <- read.csv("dataset1_test.csv")

# subset the data for batsman 1
batsman1_data <- subset(data, Batsman == "Batsman1")

# select the relevant columns
batsman1_subset <- batsman1_data[, c("position", "Runs","SR")]

# create a data frame with missing values
df <- data.frame(x = c(1, 2, NA, 4), y = c(3, NA, 5, 6))

# subset the data frame for non-missing values
subset_df <- subset(df, !is.na(x) & !is.na(y))

# run linear regression
lm(y ~ x, data = subset_df)

# Read the dataset
data <- read.csv("dataset1_test.csv")

# Perform linear regression on Runs vs Balls Faced
fit <- lm(Runs ~ BF, data = data)

# iii.	Plot the scatter plot and draw the regression.
scatter.smooth(data$Runs~ data$BF)


# print the fit summary
summary(fit)
