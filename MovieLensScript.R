##########################################################
# Create edx set, validation set (final hold-out test set)
# Note: Validation Goal: RMSE < 0.86490
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>%
  mutate(movieId = as.numeric(movieId),
         title = as.character(title),
         genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Remove unused variables
rm(dl, ratings, movies, test_index, temp, movielens, removed)


##########################################################
# Saving and loading data
##########################################################

# First, to reduce the amount of the above code that must be run when R crashes
# Uncomment to save edx and validation data into external file
#save(edx, validation, file = "edx.dat")
# Uncomment to load edx and validation from external file
#load("edx.dat", verbose = TRUE)


##########################################################
# Initial Data Analysis
##########################################################

library(lubridate)
# Get an idea for the data set to be analyzed.
head(edx)
dim(edx)
# 9000055       6

n_distinct(edx$userId)
# 69878

n_distinct(edx$movieId)
# 10677

range(edx$rating)
# 0.5 5.0
qplot(edx$rating, bins = 10)
# Unsurprisingly, most user rate things as 3 or 4, with 4 being the most common
#  rating.

as_datetime(range(edx$timestamp))
# "1995-01-09 11:46:49 UTC" "2009-01-05 05:02:16 UTC"
qplot(as_datetime(edx$timestamp), bins = (2009 - 1995) * 52)
# This reveals some interesting out-liers. There is a spike from 1996 to 1997 
#  normally distributed. Then, a huge spike in late 1999. What are these spikes?
#  what movies are being rated during these spikes? Data seems otherwise evenly 
#  distributed.


# Right away, I note that the year is tied into the title. For ease, can it be extracted?
# Save to mod_edx, so that edx remains structurally identical to the validation set
pattern <- " \\(\\d{4}\\)$"
mod_edx <- edx %>%
  as_tibble() %>%
  mutate(releaseYear = str_extract(title, pattern),
         releaseYear = str_remove_all(releaseYear, " \\(|\\)"),
         releaseYear = as.integer(releaseYear),
         title = str_remove(title, pattern))
head(mod_edx)
# Success!
range(mod_edx$releaseYear)
# 1915 2008
qplot(mod_edx$releaseYear, bins = 2008 - 1915 + 1)
# A lot of ratings were of movies released in the mid to late 90s. This may
#  reveal some information about the average user of this rating system.

# For speed later, I will also create a new column with the date of the ratings
mod_edx <- mod_edx %>%
  mutate(date = as_datetime(timestamp))

# Checking if the number of ratings corresponds with an effect on the average
mod_edx %>%
  group_by(movieId) %>% 
  summarize(n_ratings = n(), mean_rating = mean(rating)) %>%
  ggplot(aes(x = n_ratings, y = mean_rating)) +
  geom_point() +
  geom_smooth()
# There definitely seems to be a possitive effect on mean rating based on number
#  of ratings a movie has, alone

# Check if number of ratings of user corresponds with an effect on their average
mod_edx %>%
  group_by(userId) %>%
  summarize(n_ratings = n(), mean_rating = mean(rating)) %>%
  ggplot(aes(x = n_ratings, y = mean_rating)) +
  geom_point(alpha = 0.1) +
  geom_smooth()
# Interestingly, we see a regression towards the mean. Fewer ratings from users
#  appear to mean they will rate movies higher. Those above ~ 1000 votes on average
#  vote movies closer to what we might expect as an average
# There are also some outliers (n_ratings > 6000) that appear suspicious.
# All in all, there seems to be something hapening here beyond random chance

# Check if the timestamp corresponds to any kind of effect
mod_edx %>%
  mutate(week = round_date(date, unit = "week")) %>%
  group_by(week) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x = week, y = mean_rating)) +
  geom_point()
# This one is harder to track down, but there is a definite parabola that begins in 2000
#  that dips to its lowest in 2005. Regardless, this is worth investigating further
mod_edx %>%
  filter(date > make_date(year = 2000)) %>%
  mutate(week = round_date(date, unit = "week")) %>%
  group_by(week) %>%
  summarize(n_ratings = n(), mean_rating = mean(rating)) %>%
  ggplot(aes(x = week, y = mean_rating)) +
  geom_point() +
  geom_smooth()

# Check if the release year corresponds to any kind of effect
mod_edx %>%
  group_by(releaseYear) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x = releaseYear, y = mean_rating)) +
  geom_point() +
  geom_smooth()
# This has a clear effect as well

mod_edx %>%
  group_by(movieId) %>%
  summarize(title_length = str_length(title), mean_rating = mean(rating)) %>%
  ggplot(aes(x = title_length, y = mean_rating)) +
  geom_point()

##########################################################
# Machine Learning to Make Predictions
##########################################################

# To avoid over-training, first split edx into train_set and test_set
set.seed(1, sample.kind="Rounding")
# %20 of the edx data will go into the test set
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- mod_edx %>% 
  slice(-test_index)
temp <- mod_edx %>% 
  slice(test_index)

# Make sure userId and movieId in test_set are also in train_set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test_set back into train_set
removed <- anti_join(temp, test_set)
train_set <- bind_rows(train_set, removed)

# RMSE method for determining the effectiveness of the following approaches
RMSE <- function(test_ratings, predicted_ratings){
  sqrt(mean((test_ratings - predicted_ratings)^2))
}

# First, I would like to recreate the steps presented in the text to get a base
# line for where this is going. This is based on 34.7.4 A First Model
mu_hat <- mean(train_set$rating)
mu_hat
# 3.512478

# Predict unknown ratings as the average of all ratings. Mostly I just like
#  the sound of "Naive RMSE"
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse
# 1.059904

results <- tibble(method = "Average", RMSE = naive_rmse)

# Modeling the movie effect
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))
predicted_ratings <- mu_hat + test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  pull(b_i)
movie_effect_rmse <- RMSE(predicted_ratings, test_set$rating)
movie_effect_rmse
# 0.9437429
results <- results %>%
  add_row(method = "Movie Effect", RMSE = movie_effect_rmse)

# Modeling the user effect
user_avgs <- train_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)
movie_user_effect_rmse <- RMSE(predicted_ratings, test_set$rating)
movie_user_effect_rmse
# 0.8659319
results <- results %>%
  add_row(method = "Movie + User Effect", RMSE = movie_user_effect_rmse)



# From textbook section 34.11 Matrix factorization
# filter down to a smaller subset of the movies for speed and to capture most of info
train_filtered <- train_set %>%
  group_by(movieId) %>%
  filter(n() >= 50) %>%
  ungroup() %>%
  group_by(userId) %>%
  filter(n() >= 50) %>%
  ungroup() 

y <- train_filtered %>%
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()

# Add row and column names
rownames(y) <- y[, 1]
y <- y[, -1]

movie_titles <- edx %>%
  select(movieId, title) %>%
  distinct()
colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])
y

# convert values to residuals
y <- sweep(y, 2, colMeans(y, na.rm = TRUE))
y <- sweep(y, 1, rowMeans(y, na.rm = TRUE))

# To compute decomposition, make residuals with NA equal to 0
y[is.na(y)] <- 0
#pca <- prcomp(y)

qplot(1:nrow(x), pca$sdev, xlab = "PC")