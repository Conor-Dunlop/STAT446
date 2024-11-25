library(dplyr)
library(lme4)
library(lattice)
library(rpart)

dat <- read.csv("cleaned_file.csv", sep=";", header=TRUE)
dat_full <- read.csv("cleaned_file_full.csv", sep=";", header=TRUE)

dat_full <- dat_full %>%
  mutate(across(c(default, housing, loan, poutcome, y), 
                ~ ifelse(. == "yes", 1, 0)))

dat_full$marital <- factor(dat_full$marital)
dat_full$job <- factor(dat_full$job)
dat_full$education <- factor(dat_full$education)
dat_full$default <- factor(dat_full$default)
dat_full$housing <- factor(dat_full$housing)
dat_full$loan <- factor(dat_full$loan)
dat_full$contact <- factor(dat_full$contact)
dat_full$day_of_week <- factor(dat_full$day_of_week)
dat_full$month <- factor(dat_full$month)
dat_full$poutcome <- as.logical(dat_full$poutcome)
dat_full$previous <- factor(dat_full$previous)

dat <- dat %>%
  mutate(across(c(default, housing, loan, poutcome, y), 
                ~ ifelse(. == "yes", 1, 0)))

dat$marital <- factor(dat$marital)
dat$job <- factor(dat$job)
dat$education <- factor(dat$education)
dat$contact <- factor(dat$contact)
dat$day_of_week <- factor(dat$day_of_week)
dat$month <- factor(dat$month)
dat$poutcome <- as.logical(dat$poutcome)

#warning contact, pdays, poutcome have missing vals

gmod <- glm(y ~ 1 + marital + education + job, data = dat,
            family=binomial(link="logit"))

mod <- glm(y ~ 1 + marital + education, data = dat,
           family=binomial(link="logit"))

allmod <- glm(y ~ 1 + ., data = dat,
              family=binomial(link="logit"))
mod_full_reduced <- glm(y ~ 1 + education + default + duration + previous + cons.conf.idx, 
                        data = dat_full, family=binomial(link="logit"))

all_full_mod <- glm(y ~ 1 + ., data = dat_full,
              family=binomial(link="logit"))

summary(mod_full_reduced)

plot(mod_full_reduced, which=1)

anova(allmod, mod, test="Chisq")

#tree_model <- rpart(y ~ ., data = dat)

#plot(tree_model)
#text(tree_model)

confint(mod_full_reduced, method="Wald", oldNames=FALSE)

#pr <- profile(mod)
#splom(dat)
#xyplot(pr)

dataset <- data.frame(dat$y)

predicted_probs <- predict(allmod, type = "response")

# 3. Add predicted probabilities to the original dataset
dataset$predicted_probs <- predicted_probs

# 4. Sort the dataset by predicted probabilities in descending order
dataset_sorted <- dataset[order(-dataset$predicted_probs), ]

# 5. Calculate the number of top 50% cases
n_top_50 <- round(nrow(dataset_sorted) * 0.5)

# 6. Extract the top 50% most likely to have a positive outcome
top_50_percent <- dataset_sorted[1:n_top_50, ]

actual_positives_in_top_50 <- sum(top_50_percent$dat.y == 1)  # Assuming 1 represents the positive outcome
percentage_positive_in_top_50 <- (actual_positives_in_top_50 / n_top_50) * 100


total_positives <- sum(dat$y == 1)
total_cases <- nrow(dat)
percentage_positive_overall <- (total_positives / total_cases) * 100






dataset_full <- data.frame(dat_full$y)

predicted_probs_full <- predict(mod_full_reduced, type = "response")

# 3. Add predicted probabilities to the original dataset
dataset_full$predicted_probs_full <- predicted_probs_full

# 4. Sort the dataset by predicted probabilities in descending order
dataset_sorted_full <- dataset_full[order(-dataset_full$predicted_probs_full), ]

# 5. Calculate the number of top 50% cases
n_top_50_full <- round(nrow(dataset_sorted_full) * 0.5)

# 6. Extract the top 50% most likely to have a positive outcome
top_50_percent_full <- dataset_sorted_full[1:n_top_50_full, ]

actual_positives_in_top_50_full <- sum(top_50_percent_full$dat_full.y == 1)  # Assuming 1 represents the positive outcome
percentage_positive_in_top_50_full <- (actual_positives_in_top_50_full / n_top_50_full) * 100


total_positives_full <- sum(dat_full$y == 1)
total_cases_full <- nrow(dat_full)
percentage_positive_overall_full <- (total_positives_full / total_cases_full) * 100


n_top_25_full <- round(nrow(dataset_sorted_full) * 0.25)
top_25_percent_full <- dataset_sorted_full[1:n_top_25_full, ]
actual_positives_in_top_25_full <- sum(top_25_percent_full$dat_full.y == 1)

n_top_18_full <- round(nrow(dataset_sorted_full) * 0.29)
top_18_percent_full <- dataset_sorted_full[1:n_top_18_full, ]
actual_positives_in_top_18_full <- sum(top_18_percent_full$dat_full.y == 1)



dat_full <- subset(dat_full, select = -poutcome)
dat_full <- subset(dat_full, select = -cons.price.idx)
dat_full <- subset(dat_full, select = -cons.conf.idx)
dat_full <- subset(dat_full, select = -nr.employed)
Train <- dat_full[1:30891,]
test <- dat_full[30891:41188,]

Train$marital <- factor(Train$marital)
Train$job <- factor(Train$job)
Train$education <- factor(Train$education)
Train$default <- factor(Train$default)
Train$housing <- factor(Train$housing)
Train$loan <- factor(Train$loan)
Train$contact <- factor(Train$contact)
Train$day_of_week <- factor(Train$day_of_week)
Train$month <- factor(Train$month)
Train$poutcome <- as.logical(Train$poutcome)
Train$previous <- factor(Train$previous)
Train <- Train %>%
  mutate(across(c(default, housing, loan, y), 
                ~ ifelse(. == "yes", 1, 0)))

test$marital <- factor(test$marital)
test$job <- factor(test$job)
test$education <- factor(test$education)
test$default <- factor(test$default)
test$housing <- factor(test$housing)
test$loan <- factor(test$loan)
test$contact <- factor(test$contact)
test$day_of_week <- factor(test$day_of_week)
test$month <- factor(test$month)
test$poutcome <- as.logical(test$poutcome)
test$previous <- factor(test$previous)
test <- test %>%
  mutate(across(c(default, housing, loan, y), 
                ~ ifelse(. == "yes", 1, 0)))



test_filtered <- test[test$previous %in% levels(Train$previous), ]
test_filtered <- test_filtered[test_filtered$month %in% levels(Train$month), ]

#Train$previous <- factor(Train$previous, levels = c(levels(Train$previous), "4", "5", "6", "7"))

# Ensure 'previous' in the test set has the same levels
#test$previous <- factor(test$previous, levels = levels(Train$previous))

mod_full_reduced_train <- glm(y ~ 1 + ., 
                        data = Train, family=binomial(link="logit"))


dataset_full <- data.frame(test_filtered$y)

predicted_probs_full <- predict(mod_full_reduced_train, newdata = test_filtered, type = "response")

# 3. Add predicted probabilities to the original dataset
dataset_full$predicted_probs_full <- predicted_probs_full

  # 4. Sort the dataset by predicted probabilities in descending order
dataset_sorted_full <- dataset_full[order(-dataset_full$predicted_probs_full), ]

# 5. Calculate the number of top 50% cases
n_top_50_full <- round(nrow(dataset_sorted_full) * 0.5)

# 6. Extract the top 50% most likely to have a positive outcome
top_50_percent_full <- dataset_sorted_full[1:n_top_50_full, ]

actual_positives_in_top_50_full <- sum(top_50_percent_full$test_filtered.y == 1)  # Assuming 1 represents the positive outcome
percentage_positive_in_top_50_full <- (actual_positives_in_top_50_full / n_top_50_full) * 100


test_positives <- sum(test_filtered$y == 1)
total_cases_full <- nrow(dat_full)
percentage_positive_overall_full <- (total_positives_full / total_cases_full) * 100
