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
# Uncomment the following line to save variables that take a long time to
# generate into an external file
#save(edx, validation, train_set, test_set, file = "edx.dat")

# Uncomment to load previously saved data
#load("edx.dat", verbose = TRUE)


##########################################################
# Exploratory Data Analysis
##########################################################

# For use of the Date object and some round_dates that will be used for weeks
library(lubridate)
# Get an idea for the data set to be analyzed.
head(edx)
dim(edx)
# 9000055       6

n_distinct(edx$userId)
# 69878

n_distinct(edx$movieId)
# 10677
n_distinct(edx$title)
# 10676
n_distinct(edx$genres)
# 797

# Genres are currently a string divided by |'s. For now, just sepearate them
#  into separate rows to count distinct genres. Save for use later.
edx_genres_split <- edx %>%
  separate_rows(genres, sep = "\\|")
n_distinct(edx_genres_split$genres)
# 20

qplot(edx$rating, bins = 10)
# Unsurprisingly, most user rate things as 3 or 4, with 4 being the most common
#  rating. Many fewer people will use a half rating, around 5 times.

as_datetime(range(edx$timestamp))
# "1995-01-09 11:46:49 UTC" "2009-01-05 05:02:16 UTC"
qplot(as_datetime(edx$timestamp), bins = (2009 - 1995) * 52)
# This reveals some interesting out-liers. There is a spike from 1996 to 1997 
#  normally distributed. Then, a huge spike in late 1999. What are these spikes?
#  what movies are being rated during these spikes? Data seems otherwise evenly 
#  distributed.


# Right away, I note that the year is tied into the title. For ease, can it be extracted?
pattern <- " \\(\\d{4}\\)$"
edx <- edx %>%
  as_tibble() %>%
  mutate(release_year = str_extract(title, pattern),
         release_year = str_remove_all(release_year, " \\(|\\)"), # remove space left-paren or right parent
         release_year = as.integer(release_year),
         title = str_remove(title, pattern))
head(edx)
# Success!
range(edx$release_year)
# 1915 2008
qplot(edx$release_year, bins = 2008 - 1915 + 1)
# A lot of ratings were of movies released in the mid to late 90s. This may
#  reveal some information about the average user of this rating system.

# For speed later, I will also create a new column with the date of the ratings
#  As well as the date between relase and rating
edx <- edx %>%
  mutate(date = as_datetime(timestamp),
         years_since_release = year(date) - release_year)


#####################################################
# CHECKING CORRELATION WITH AVERAGE RATING
#####################################################

# Checking if the number of ratings corresponds with an effect on the average
edx %>%
  group_by(movieId) %>% 
  summarize(n_ratings = n(), mean_rating = mean(rating)) %>%
  ggplot(aes(x = n_ratings, y = mean_rating)) +
  geom_point() +
  geom_smooth()
# There definitely seems to be a positive effect on mean rating based on number
#  of ratings a movie has, alone

# Check if number of ratings of user corresponds with an effect on their average
edx %>%
  group_by(userId) %>%
  summarize(n_ratings = n(), mean_rating = mean(rating)) %>%
  ggplot(aes(x = n_ratings, y = mean_rating)) +
  geom_point(alpha = 0.05) +
  geom_smooth()
# Interestingly, we see a regression towards the mean. Fewer ratings from users
#  appear to mean they will rate movies higher. Those above ~ 1000 votes on average
#  vote movies closer to what we might expect as an average
# There are also some outliers (n_ratings > 6000) that appear suspicious.
# All in all, there seems to be something happening here beyond random chance

# Check what sort of effect genre may have on average
edx %>%
  group_by(genres) %>%
  summarize(avg_rating = mean(rating), count = n()) %>%
  slice_max(order_by = count, n = 20) %>%
  arrange(desc(avg_rating)) %>%
  ggplot(aes(x = reorder(genres, -avg_rating), y = avg_rating)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

edx_genres_split %>%
  group_by(genres) %>%
  summarize(avg_rating = mean(rating), count = n()) %>%
  ggplot(aes(x = reorder(genres, -avg_rating), y = avg_rating)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))
# Note how overlapping genres clump: Mystery/ Drama / Crime, Documentary / War


# Check if the timestamp corresponds to any kind of effect
edx %>%
  mutate(week = round_date(date, unit = "week")) %>%
  group_by(week) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x = week, y = mean_rating)) +
  geom_point()
# This one is harder to track down, but there is a definite parabola that begins in 2000
#  that dips to its lowest in 2005. Regardless, this is worth investigating further
edx %>%
  filter(date > make_date(year = 2000)) %>%
  mutate(week = round_date(date, unit = "week")) %>%
  group_by(week) %>%
  summarize(n_ratings = n(), mean_rating = mean(rating)) %>%
  ggplot(aes(x = week, y = mean_rating)) +
  geom_point() +
  geom_smooth()

# Check if the release year corresponds to any kind of effect
edx %>%
  group_by(release_year) %>%
  summarize(mean_rating = mean(rating) - mu_hat) %>%
  ggplot(aes(x = release_year, y = mean_rating)) +
  geom_point() +
  geom_smooth()
# This has a clear effect as well

# What about time from release?
edx %>% 
  group_by(years_since_release) %>%
  summarize(mean_rating = mean(rating) - mu_hat) %>%
  ggplot(aes(x = years_since_release, y = mean_rating)) +
  geom_point() + 
  geom_smooth()
# Huh, there seems to be something here, too.

# This may be nothing, but I am curious: Does the length of a movie title affect
#  it's average rating?
edx %>%
  group_by(movieId) %>%
  summarize(title_length = str_length(title), mean_rating = mean(rating)) %>%
  ggplot(aes(x = title_length, y = mean_rating)) +
  geom_point()
# Bizarrely enough, the longer a movie title is (or the greater number of alias it
#  has) the higher it tends to be rated.This likely corresponds to movies that are
#  translated tend to get additional titles, and movies that are translated likely
#  performed well enough to justify it.
# What is this movie with a title length of over 150 characters?
edx %>%
  select(movieId, title) %>%
  distinct() %>%
  mutate(title_len = str_length(title)) %>%
  top_n(5, title_len) %>%
  arrange(desc(title_len))
# Wild
# Overall, the effect is likely explained that longer titles correspond to movies
#  released in more languages. They likely did so because they were better movies.


##########################################################
# Machine Learning to Make Predictions
##########################################################

# To avoid over-training, first split edx into train_set and test_set
set.seed(1)
# %20 of the edx data will go into the test set
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx %>% 
  slice(-test_index)
temp <- edx %>% 
  slice(test_index)

# Make sure userId and movieId in test_set are also in train_set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test_set back into train_set
removed <- anti_join(temp, test_set)
train_set <- bind_rows(train_set, removed)

rm(test_index, temp, removed)

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

# Save the results to a tibble to easily check if improvements are being made
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

# Modeling the genre effect
genre_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u))
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  pull(pred)
movie_user_genre_effect_rmse <- RMSE(predicted_ratings, test_set$rating)
movie_user_genre_effect_rmse
results <- results %>%
  add_row(method = "Movie + User + Genres Effect", RMSE = movie_user_genre_effect_rmse)

# Can we do better with the genre effect?

# Lets go with a slow approach that will take a lot of memory, but lets us 
#  really dig into the genre effect.
split_genre_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u))
split_genre_avgs %>%
  ggplot(aes(x = reorder(genres, b_g), y = b_g)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

# A different method for adding the genre effect is necessary here. Now, we want
#  to check if the genre is present. If so, add the avg associated with that to
#  the genre effect. Else, don't change it

###### Original method for recombining genres
# x <- sapply(b_g$genres, function(g) {
#   sum(split_genre_avgs$b_g[str_detect(split_genre_avgs$genres, g)])
# }, simplify = TRUE)
# x <- as_tibble(x) %>%
#   mutate(genres = b_g$genres) %>%
#   slice_max(value, n = 5)
# b_g <- unname(b_g)
# predicted_ratings <- test_set %>%
#   left_join(movie_avgs, by = "movieId") %>%
#   left_join(user_avgs, by = "userId") %>%
#   mutate(pred = mu_hat + b_i + b_u) %>%
#   pull(pred)
# predicted_ratings <- predicted_ratings + b_g

#### NEW method for separaret genre scores into one combined score
combined_genre_avgs <- train_set %>%
  distinct(genres) %>%
  mutate(b_g = map_dbl(genres, function(g){
    sum(split_genre_avgs$b_g[str_detect(split_genre_avgs$genres, g)])
  }))
combined_genre_avgs %>%
  ggplot(aes(x = reorder(genres, b_g), y = b_g)) +
  geom_point()

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(combined_genre_avgs, by = "genres") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  pull(pred)
movie_user_sep_genre_effect_rmse <- RMSE(predicted_ratings, test_set$rating)
movie_user_sep_genre_effect_rmse
results <- results %>%
  add_row(method = "Movie + User + Separated Genres Effect", RMSE = movie_user_sep_genre_effect_rmse)
rm(split_genre_avgs, combined_genre_avgs)
# This reduction in effect would suggest that there is more to a combined genre
#  than simply the sum of its parts

# Modeling the date effect
week_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(week = round_date(date, unit = "week")) %>%
  group_by(week) %>%
  summarize(b_d = mean(rating - mu_hat - b_i - b_u - b_g), n = n())

# Add a fit, too, to see if it is any more effective
fit <- loess(week_avgs$b_d ~ as.numeric(week_avgs$week), weights = week_avgs$n, degree = 2)
week_avgs <-
  week_avgs %>%
  mutate(d_ui = predict(fit))
week_avgs %>%
  ggplot() +
  ylim(-0.5, 0.5) +
  geom_point(aes(x = week, y = b_d), alpha = 0.1) + 
  geom_line(aes(x = week, y = d_ui, color = "red"))

# Only check without fit. Fits in general did not seem to results in better RMSE
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(week = round_date(date, unit = "week")) %>%
  left_join(week_avgs, by = "week") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_d) %>%
  pull(pred)
movie_user_genre_week_rmse <- RMSE(predicted_ratings, test_set$rating)
movie_user_genre_week_rmse
results <- results %>%
  add_row(method = "Movie + User + Genre + Date Effect", RMSE = movie_user_genre_week_rmse)

# Checking the years since release, since that had a compelling graph in the
#  exploratory analysis.
year_avgs <-
  train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(week = round_date(date, unit = "week")) %>%
  left_join(week_avgs, by = "week") %>%
  group_by(years_since_release) %>%
  summarize(b_y = mean(rating - mu_hat - b_i - b_u - b_g - b_d), n = n())

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(week = round_date(date, unit = "week")) %>%
  left_join(week_avgs, by = "week") %>%
  left_join(year_avgs, by = "years_since_release") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_d + b_y) %>%
  pull(pred)

all_rmse <- RMSE(predicted_ratings, test_set$rating)
all_rmse
results <- results %>%
  add_row(method = "Movie + User + Genre + Date + Year Effect", RMSE = all_rmse)

## OTHERS, release year?
year_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(week = round_date(date, unit = "week")) %>%
  left_join(week_avgs, by = "week") %>%
  group_by(release_year) %>%
  summarize(b_y = mean(rating - mu_hat - b_i - b_u - b_g - d_ui), n = n())

year_avgs %>%
  ggplot(aes(x = release_year, y = b_y)) +
  geom_point()

fit <- loess(year_avgs$b_y ~ as.numeric(year_avgs$release_year), degree = 2)
year_avgs <- 
  year_avgs %>%
  mutate(y_ui = predict(fit))

year_avgs %>%
  ggplot() +
  ylim(-0.1, 0.2) +
  geom_point(aes(x = release_year, y = b_y)) + 
  geom_line(aes(x = release_year, y = y_ui, color = "red"))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(week = round_date(date, unit = "week")) %>%
  left_join(week_avgs, by = "week") %>%
  left_join(year_avgs, by = "release_year") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + d_ui + y_ui) %>%
  pull(pred)
all_rmse <- RMSE(predicted_ratings, test_set$rating)
all_rmse
results <- results %>%
  add_row(method = "All + release_year Effects", RMSE = all_rmse)


rm(genre_avgs, movie_avgs, user_avgs, week_avgs, predicted_ratings)
####
# Regularization (From 34.9 in textbook)
####

predict_ratings <- function(l, pred_against = test_set, verbose = TRUE){
  # Movie effect
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + l))
  
  # User effect
  b_u <- train_set %>%
    left_join(b_i, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat) / (n() + l))
  
  # Genre effect
  b_g <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu_hat) / (n() + l))
  
  # Date effect
  b_d <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(week = round_date(date, unit = "week")) %>%
    group_by(week) %>%
    summarize(b_d = sum(rating - mu_hat - b_i - b_u - b_g) / (n() + l), n = n())
  
  # fit <- loess(d_ui$b_d ~ as.numeric(d_ui$week), degree = 2)
  # d_ui <-
  #   d_ui %>%
  #   mutate(d_ui = predict(fit))
  
  # Year effect
  b_y <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(week = round_date(date, unit = "week")) %>%
    left_join(b_d, by = "week") %>%
    group_by(years_since_release) %>%
    summarize(b_y = sum(rating - mu_hat - b_i - b_u - b_g - b_d) / (n() + l))
  
  predicted_ratings <-
    pred_against %>%
    left_join(b_i, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    left_join(b_g, by = "genres") %>%
    mutate(week = round_date(date, unit = "week")) %>%
    left_join(b_d, by = "week") %>%
    left_join(b_y, by = "years_since_release") %>%
    mutate(pred = mu_hat + b_i + b_u + b_g + b_d + b_y) %>%
    pull(pred)
  
  # Clamping. Reduces RMSE, makes sense given the context of the problem
  #  I checked both with and without clamping, clamping seems to reduce RMSE
  predicted_ratings[predicted_ratings > 5] <- 5
  predicted_ratings[predicted_ratings < 1] <- 1
  
  RMSE <- RMSE(predicted_ratings, pred_against$rating)
  
  if (verbose) {
    print(c("Lambda: ", l, " RMSE: ", RMSE))
  }
  return(predicted_ratings)
}

# Splitting up the lambda checks, since the above function takes a while to run
lambdas <- seq(2, 6, 1)
# 5
lambdas <- seq(4.0, 6.0, 0.1)
# 4.75
lambdas <- seq(4.6, 4.8, 0.1)
lambdas <- c(4.6)
predictions <- sapply(lambdas, predict_ratings)
rmses <- apply(predictions, 2, function(pred) {
  return (RMSE(test_set$rating, pred))
})
min(rmses)
lambdas[which.min(rmses)]
qplot(lambdas, rmses)
results <- results %>%
  add_row(method = "Regularized All Effects", RMSE = min(rmses))


############################################################################
# Testing of clamping values to 1, 1,5, 2, 2.5, 3, 3.5, 4, 4.5, 5
# Generally found that this had a neutral to negative effect
predicted_ratings <- rmses[,1]$predicted_ratings
rounding_factor <- seq(0.2, 0.3, 0.01)

rounded <- sapply(c(2.5), function(r){
  base_prediction <- round(predictions)
  dec <- predictions - base_prediction
  base_prediction[dec > r] <- base_prediction[dec > r] + 0.5
  base_prediction[dec < -r] <- base_prediction[dec < - r] - 0.5
  qplot(base_prediction, bins = 10)
  return(RMSE(base_prediction, test_set$rating))
})
qplot(rounding_factor, rounded)


###########################################################
#FINAL TEST
#
lambda <- 4.5
# Movie effect
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_hat) / (n() + lambda))

# User effect
b_u <- train_set %>%
  left_join(b_i, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))

# Genre effect
b_g <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - mu_hat) / (n() + lambda))

# Date effect
b_d <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(week = round_date(date, unit = "week")) %>%
  group_by(week) %>%
  summarize(b_d = sum(rating - mu_hat - b_i - b_u - b_g) / (n() + lambda), n = n())

# fit <- loess(d_ui$b_d ~ as.numeric(d_ui$week), weights = d_ui$n, degree = 2)
# d_ui <-
#   d_ui %>%
#   mutate(d_ui = predict(fit))

# Year effect
b_y <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(week = round_date(date, unit = "week")) %>%
  left_join(b_d, by = "week") %>%
  group_by(years_since_release) %>%
  summarize(b_y = sum(rating - mu_hat - b_i - b_u - b_g - b_d) / (n() + lambda))

predicted_ratings <-
  validation %>%
  mutate(release_year = str_extract(title, " \\(\\d{4}\\)$"),
         release_year = str_remove_all(release_year, " \\(|\\)"),
         release_year = as.integer(release_year),
         date = as_datetime(timestamp),
         week = round_date(date, unit = "week"),
         years_since_release = year(date) - release_year) %>%
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = 'userId') %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_d, by = "week") %>%
  left_join(b_y, by = "years_since_release") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_d + b_y) %>%
  pull(pred)


predicted_ratings[predicted_ratings > 5] <- 5
predicted_ratings[predicted_ratings < 0.5] <- 0.5

RMSE(predicted_ratings, validation$rating)
# 0.8650716
# OOFDA. Not there yet.
# LATER NOTE: After completing the writeup, there were some bugs in the final
#  model that were handled in the paper. See the paper for the final model used,'
#  in which the target RMSE is acheived.
rm(b_g, b_i, b_u, d_ui, fit)
