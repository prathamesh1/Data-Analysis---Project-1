library(jsonlite)
library(dplyr)
library(purrr)
library(nnet)

#train dataset json to csv
train <- fromJSON("train.json")
vartrain <- setdiff(names(train), c("photos", "features"))
train <- map_at(train, vartrain, unlist) %>% tibble::as_tibble(.)
colnames(train)
train = train[, -12]

# convert into factors
train$interest_level = factor(train$interest_level, levels = c('low', 'medium', 'high'))

#test dataset json to csv
test <- fromJSON("test.json")
vartest <- setdiff(names(test), c("photos", "features"))
test <- map_at(test, vartest, unlist) %>% tibble::as_tibble(.)
colnames(test)
test = test[, -12]

# model creation
model = multinom(interest_level ~ price + bathrooms + bedrooms ,data=train)

# test prediction
probs <- predict(model, test, "probs")
test_df = data.frame(listing_id = test$listing_id, probs)

write.csv(test_df, file = "submission.csv", row.names = FALSE)