# HarvardX
# Professional Certificate in Data Science
# PH125.9x - Capstone: MovieLens Project
# Ian Espejo Campos

# 1.0 DATA EXTRACTION

# 1.1 Import libraries
rm(list=ls())

if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2))
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr))
  install.packages("knitr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(knitr)

# 1.2 Get from URL or download file in Documents
zip_file <- "~/ml-10m.zip"
if(file.exists(zip_file)) {dl <- zip_file} else {
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip",
                dl)
}

# 1.3 Unzip and read data
ratings <- fread(text = gsub("::", "\t",
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl,
                                          "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>%
  mutate(movieId = as.numeric(movieId),
         title = as.character(title),
         genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# 1.4 Validation set will be 10% of MovieLens data
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating,
                                  times = 1,
                                  p = 0.1,
                                  list = F)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# 1.5 Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# 1.6 Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# 2.0 DATA ANALYSIS

# 2.1 Descriptive statistics
dim(edx)
## [1] 9000055 6

head(edx) %>% as.tibble()
## # A tibble: 6 x 6
## userId movieId rating timestamp title genres
## <int> <dbl> <dbl> <int> <chr> <chr>
## 1 1 122 5 838985046 Boomerang (1992) Comedy|Romance
## 2 1 185 5 838983525 Net, The (1995) Action|Crime|Thriller
## 3 1 292 5 838983421 Outbreak (1995) Action|Drama|Sci-Fi|T~
## 4 1 316 5 838983392 Stargate (1994) Action|Adventure|Sci-~
## 5 1 329 5 838983392 Star Trek: Generations~ Action|Adventure|Dram~
## 6 1 355 5 838984474 Flintstones, The (1994) Children|Comedy|Fanta~

summary(edx)
## userId movieId rating timestamp
## Min. : 1 Min. : 1 Min. :0.500 Min. :7.897e+08
## 1st Qu.:18124 1st Qu.: 648 1st Qu.:3.000 1st Qu.:9.468e+08
## Median :35738 Median : 1834 Median :4.000 Median :1.035e+09
## Mean :35870 Mean : 4122 Mean :3.512 Mean :1.033e+09
## 3rd Qu.:53607 3rd Qu.: 3626 3rd Qu.:4.000 3rd Qu.:1.127e+09
## Max. :71567 Max. :65133 Max. :5.000 Max. :1.231e+09
## title genres
## Length:9000055 Length:9000055
## Class :character Class :character
## Mode :character Mode :character

summarize(edx,
          unique_users = n_distinct(userId),
          unique_movies = n_distinct(movieId))
## unique_users unique_movies
## 1 69878 10677

edx %>% group_by(movieId) %>%
  summarise(stars = mean(rating),
            votes = n()) %>%
  left_join(edx, by = "movieId") %>%
  select(movieId, title, votes, stars) %>%
  unique() %>%
  filter(title != "NA") %>%
  arrange(desc(stars)) %>%
  slice(1:50) %>%
  mutate(ranking = 1:n()) %>%
  select(ranking, everything()) %>%
  kable()

# 2.2 Rating histogram
y_cuts <- c(0.5, 1.0, 1.5, 2.0, 2.5)
edx %>% ggplot(aes(rating,
                   fill = cut(rating, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "Rating", y = "") +
  ggtitle("Figure 2.1: Rating distribution") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_x_discrete(limits = c(seq(min(edx$rating),
                                  max(edx$rating),
                                  max(edx$rating)/n_distinct(edx$rating)))) +
  scale_y_continuous(labels = paste0(y_cuts, "M"),
                     breaks = y_cuts * 10^6)

# 2.3 Ratings per users histogram
edx %>% count(userId) %>%
  ggplot(aes(n, fill = cut(n, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "Ratings (log scale)", y = "Users") +
  ggtitle("Figure 2.2: Ratings per users") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_x_log10()

# 2.4 Mean rating per users histogram
edx %>% group_by(userId) %>%
  summarise(m = mean(rating)) %>%
  ggplot(aes(m, fill = cut(m, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "Mean rating", y = "Users") +
  ggtitle("Figure 2.3: Mean rating per users") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_x_discrete(limits = c(seq(min(edx$rating),
                                  max(edx$rating),
                                  max(edx$rating)/n_distinct(edx$rating))))

# 2.5 Ratings per movies histogram
edx %>% count(movieId) %>%
  ggplot(aes(n, fill = cut(n, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "Ratings (log scale)", y = "Movies") +
  ggtitle("Figure 2.4: Ratings per movies") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_x_log10()

# 2.6 Mean rating per movies histogram
edx %>% group_by(movieId) %>%
  summarise(m = mean(rating)) %>%
  ggplot(aes(m, fill = cut(m, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "Mean rating", y = "Movies") +
  ggtitle("Figure 2.5: Mean rating per movies") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_x_discrete(limits = c(seq(min(edx$rating),
                                  max(edx$rating),
                                  max(edx$rating)/n_distinct(edx$rating))))

# 2.7 Dates addition
edx <- edx %>%
  mutate(timestamp = as.POSIXct(timestamp,
                                origin = "1970-01-01",
                                tz = "GMT"),
         year_movie = as.numeric(substr(title,
                                        nchar(title)-4,
                                        nchar(title)-1)),
         year_rated = as.numeric(format(timestamp, "%Y")),
         rate_ts = year_rated - year_movie)

edx %>%
  select(movieId, year_movie) %>%
  unique() %>%
  group_by(year_movie) %>%
  summarise(count = n()) %>%
  ggplot(aes(year_movie, count)) +
  geom_line(color = "darkred",
            size = 0.8) +
  labs(x = "Release year", y = "Movies") +
  ggtitle("Figure 2.6: Release year per movies") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

# 3.0 DATA MODELLING

# 3.1 Summary statistics
mu <- mean(edx$rating)
# [1] 3.5124652

# 3.2 Average prediction
rmse_naive <- RMSE(edx$rating, mu)
rmse_results <- tibble(Model = "01. Average only",
                       RMSE = rmse_naive,
                       Goal = ifelse(rmse_naive < 0.8649, "Under", "Over"))
kable(rmse_results)
# |Model              |      RMSE|Goal  |
# |:------------------|---------:|:-----|
# |01. Average only   | 1.0603313|Over  |

# 3.3 Movie effect prediction
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarise(b_Movie = mean(rating - mu))

movie_avgs %>%
  ggplot(aes(b_Movie, fill = cut(b_Movie, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "b_Movie", y = "Movies") +
  ggtitle("Figure 3.1: Movies per computed b_Movie") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

predicts_movie <- mu + edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  pull(b_Movie)

rmse_model1 <- RMSE(predicts_movie, edx$rating)

rmse_results <- rmse_results %>%
  bind_rows(tibble(Model = "02. Movie Effect",
                   RMSE = rmse_model1,
                   Goal = ifelse(rmse_model1 < 0.8649, "Under", "Over")))
kable(rmse_results)
# |Model              |      RMSE|Goal  |
# |:------------------|---------:|:-----|
# |01. Average only   | 1.0603313|Over  |
# |02. Movie Effect   | 0.9423475|Over  |

# 3.4 Movie and user effect prediction
user_avgs <- edx %>% 
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_User = mean(rating - b_Movie - mu))

user_avgs %>%
  ggplot(aes(b_User, fill = cut(b_User, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "b_User", y = "Movies") +
  ggtitle("Figure 3.2: Movies per computed b_User") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

predicts_movie_user <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu + b_Movie + b_User) %>%
  pull(pred)

rmse_model2 <- RMSE(predicts_movie_user,
                    edx$rating)
rmse_results <- rmse_results %>%
  bind_rows(tibble(Model = "03. +User Effect",
                   RMSE = rmse_model2,
                   Goal = ifelse(rmse_model2 < 0.8649, "Under", "Over")))
kable(rmse_results)
# |Model              |      RMSE|Goal  |
# |:------------------|---------:|:-----|
# |01. Average only   | 1.0603313|Over  |
# |02. Movie Effect   | 0.9423475|Over  |
# |03. +User Effect   | 0.8567039|Under |

# 3.5 Movie, user and release year effect prediction
RYear_avgs <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(year_movie) %>%
  summarise(b_RYear = mean(rating - b_User - b_Movie - mu))

RYear_avgs %>%
  ggplot(aes(b_RYear, fill = cut(b_RYear, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "b_RYear", y = "Movies") +
  ggtitle("Figure 3.3: Movies per computed b_RYear") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

predicts_movie_user_RYear <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(RYear_avgs, by = "year_movie") %>%
  mutate(pred = mu + b_Movie + b_User + b_RYear) %>%
  pull(pred)

rmse_model3 <- RMSE(predicts_movie_user_RYear,
                    edx$rating)
rmse_results <- rmse_results %>%
  bind_rows(tibble(Model = "04. +RYear Effect",
                   RMSE = rmse_model3,
                   Goal = ifelse(rmse_model3 < 0.8649, "Under", "Over")))
kable(rmse_results)
# |Model              |      RMSE|Goal  |
# |:------------------|---------:|:-----|
# |01. Average only   | 1.0603313|Over  |
# |02. Movie Effect   | 0.9423475|Over  |
# |03. +User Effect   | 0.8567039|Under |
# |04. +RYear Effect  | 0.8563777|Under |

# 3.6 Movie, user, release year and movie age effect prediction
rateTS_avgs <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(RYear_avgs, by = "year_movie") %>%
  group_by(rate_ts) %>%
  summarise(b_rateTS = mean(rating - b_RYear - b_User - b_Movie - mu))

rateTS_avgs %>%
  ggplot(aes(b_rateTS, fill = cut(b_rateTS, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "b_rateTS", y = "Movies") +
  ggtitle("Figure 3.4: Movies per computed b_rateTS") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

predicts_movie_user_RYear_rateTS <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(RYear_avgs, by = "year_movie") %>%
  left_join(rateTS_avgs, by = "rate_ts") %>%
  mutate(pred = mu + b_rateTS + b_RYear + b_User + b_Movie) %>%
  pull(pred)

rmse_model4 <- RMSE(predicts_movie_user_RYear_rateTS,
                    edx$rating)
rmse_results <- rmse_results %>%
  bind_rows(tibble(Model = "05. +RateTS Effect",
                   RMSE = rmse_model4,
                   Goal = ifelse(rmse_model4 < 0.8649, "Under", "Over")))
kable(rmse_results)
# |Model              |      RMSE|Goal  |
# |:------------------|---------:|:-----|
# |01. Average only   | 1.0603313|Over  |
# |02. Movie Effect   | 0.9423475|Over  |
# |03. +User Effect   | 0.8567039|Under |
# |04. +RYear Effect  | 0.8563777|Under |
# |05. +RateTS Effect | 0.8560683|Under |

# 3.7 Regularized effects
lambdas <- seq(0, 5, 0.25)

RMSEs <- sapply(lambdas, function(l){
  
  b_Movie <- edx %>%
    group_by(movieId) %>%
    summarise(b_Movie = sum(rating - mu)/(n() + l))
  
  b_User <- edx %>%
    left_join(b_Movie, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_User = sum(rating - mu - b_Movie)/(n() + l))
  
  b_RYear <- edx %>%
    left_join(b_Movie, by = "movieId") %>%
    left_join(b_User, by = "userId") %>%
    group_by(year_movie) %>%
    summarise(b_RYear = sum(rating - mu - b_Movie - b_User)/(n() + l))
  
  b_rateTS <- edx %>%
    left_join(b_Movie, by = "movieId") %>%
    left_join(b_User, by = "userId") %>%
    left_join(b_RYear, by = "year_movie") %>%
    group_by(rate_ts) %>%
    summarise(b_rateTS = sum(rating - mu - b_Movie - b_User - b_RYear)/(n() + l))
  
  predicts_ratings <- edx %>%
    left_join(b_Movie, by = "movieId") %>%
    left_join(b_User, by = "userId") %>%
    left_join(b_RYear, by = "year_movie") %>%
    left_join(b_rateTS, by = "rate_ts") %>%
    mutate(pred = mu + b_Movie + b_User + b_RYear + b_rateTS) %>%
    pull(pred)
  
  return(RMSE(predicts_ratings, edx$rating))
})

tibble(lambdas) %>%
  bind_cols(tibble(RMSEs)) %>%
  ggplot(aes(lambdas, RMSEs)) +
  geom_point() +
  theme_update() +
  labs(x = "lambdas", y = "RMSEs") +
  ggtitle("Figure 3.5: RMSE per lambda") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

rmse_model5 <- min(RMSEs)
lambda <- lambdas[which.min(RMSEs)]

rmse_results <- rmse_results %>%
  bind_rows(tibble(Model = "06. Regularized",
                   RMSE = rmse_model5,
                   Goal = ifelse(rmse_model5 < 0.8649, "Under", "Over")))
kable(rmse_results)
# |Model              |      RMSE|Goal  |
# |:------------------|---------:|:-----|
# |01. Average only   | 1.0603313|Over  |
# |02. Movie Effect   | 0.9423475|Over  |
# |03. +User Effect   | 0.8567039|Under |
# |04. +RYear Effect  | 0.8563777|Under |
# |05. +RateTS Effect | 0.8560683|Under |
# |06. Regularized    | 0.8560611|Under |

# 4.0 FINAL RMSE VALUE

# 4.1 Replication with optimal lambda
b_Movie <- edx %>%
  group_by(movieId) %>%
  summarise(b_Movie = sum(rating - mu)/(n() + lambda))

b_User <- edx %>%
  left_join(b_Movie, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_User = sum(rating - mu - b_Movie)/(n() + lambda))

b_RYear <- edx %>%
  left_join(b_Movie, by = "movieId") %>%
  left_join(b_User, by = "userId") %>%
  group_by(year_movie) %>%
  summarise(b_RYear = sum(rating - mu - b_Movie - b_User)/(n() + lambda))

b_RateTS <- edx %>%
  left_join(b_Movie, by = "movieId") %>%
  left_join(b_User, by = "userId") %>%
  left_join(b_RYear, by = "year_movie") %>%
  group_by(rate_ts) %>%
  summarise(b_rateTS = sum(rating - mu - b_Movie - b_User - b_RYear)/(n() + lambda))

# 4.2 Validation data arrangement
validation <- validation %>%
  mutate(timestamp = as.POSIXct(timestamp,
                                origin = "1970-01-01",
                                tz = "GMT"),
         year_movie = as.numeric(substr(title,
                                        nchar(title)-4,
                                        nchar(title)-1)),
         year_rated = as.numeric(format(timestamp, "%Y")),
         rate_ts = year_rated - year_movie)

# 4.3 Final prediction
predicts_validation <- validation %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(RYear_avgs, by = "year_movie") %>%
  left_join(rateTS_avgs, by = "rate_ts") %>%
  mutate(pred = mu + b_rateTS + b_RYear + b_User + b_Movie) %>%
  pull(pred)

rmse_model_final <- RMSE(predicts_validation,
                         validation$rating)
rmse_results <- rmse_results %>%
  bind_rows(tibble(Model = "07. Validation set",
                   RMSE = rmse_model_final,
                   Goal = ifelse(rmse_model_final < 0.8649, "Under", "Over")))
kable(rmse_results)
# |Model              |      RMSE|Goal  |
# |:------------------|---------:|:-----|
# |01. Average only   | 1.0603313|Over  |
# |02. Movie Effect   | 0.9423475|Over  |
# |03. +User Effect   | 0.8567039|Under |
# |04. +RYear Effect  | 0.8563777|Under |
# |05. +RateTS Effect | 0.8560683|Under |
# |06. Regularized    | 0.8560611|Under |
# |07. Validation set | 0.8647026|Under |

ifelse(rmse_model_final < 0.8649, "Task accomplished", "Not accomplished")
# [1] "Task accomplished"

# SOURCE: MovieLens 10M dataset available in https://grouplens.org/datasets/movielens/10m/
