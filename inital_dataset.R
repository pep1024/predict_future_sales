## Read and prepare the full dataset


sales_train <- read.csv("data/sales_train.csv", stringsAsFactors = F)
items <- read.csv("data/items.csv", stringsAsFactors = F)
item_categories <- read.csv("data/item_categories.csv", stringsAsFactors = F)
test <- read.csv("data/test.csv", stringsAsFactors = F)
sample_submission <- read.csv("data/sample_submission.csv", stringsAsFactors = F)
shops <- read.csv("data/shops.csv", stringsAsFactors = F)

## Build complete training set

## Convert date field as Date

dataset <- data.frame(sales_train[, -1], stringsAsFactors = F)
dataset$date <- as.Date(sales_train$date, format = "%d.%m.%Y")
dataset$shop_item <- paste(dataset[, ]$shop_id, dataset[]$item_id, sep = "_")

## Prepare testset to merge with dataset


testset <- data.frame(date_block_num = rep(34, 214200), test[, 2:3], 
  item_price = rep(NA, 214200), item_cnt_day = rep(NA, 214200),
  shop_item = paste(test[, 2], test[, 3], sep = "_"),
  stringsAsFactors = F)

## Add item_category

dataset_all <- merge(dataset, items[, -1])
testset_all <- merge(testset, items[, -1])

## A full set with train and test data will be created later with train data aggregated monthly by shop and item

time_0 <- Sys.time()
save(dataset_all, file = "data/dataset_all.RData")
save(testset_all, file = "data/testset_all.RData")
difftime(Sys.time(), time_0, units = "auto")
