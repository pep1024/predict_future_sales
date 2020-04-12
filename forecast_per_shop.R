forecast_per_shop <- function(shop_id, month_forecast){
  
  # fileter shop
  shop_set <- dataset_all[dataset_all$shop_id == shop_id, ]
  # aggregate per month
  shop_agg_by_month <- aggregate(item_cnt_day ~ date_block_num, data = shop_set, FUN = sum)
  # limit series upto month_forecast
  shop_agg_by_month <- shop_agg_by_month[shop_agg_by_month$date_block_num <= month_forecast, ]
  # add zeros when no product is sold
  shop_set_by_month <- data.frame(date_block_num = 0:(month_forecast - 1), item_cnt_day = rep(0, month_forecast))
  shop_set_by_month[shop_agg_by_month$date_block_num + 1, ]$item_cnt_day <- shop_agg_by_month$item_cnt_day
  # Turn series into ts
  shop_set_by_month.ts <- ts(shop_set_by_month$item_cnt_day, start = c(2013, 1), frequency = 12)
  
  # generate prodicition for month forecast
  last_month_ts <- tail(time(shop_set_by_month.ts), 2)[1]
  ts_use <- window(shop_set_by_month.ts, start = 2013, end = last_month_ts)
  shop.hw <- HoltWinters(ts_use)
  shop.forecast <- predict(shop.hw, n.ahead = 1)
  # If prediciton negative, turn it zero
  shop.forecast[1] <- max(round(shop.forecast), 0)
  
  # forecasted month in time form
  month_forecast <- tail(time(shop_set_by_month.ts), 1)
  
  # compute error
  original_value <- window(shop_set_by_month.ts, month_forecast, month_forecast) 
  mse <- (shop.forecast[1] - original_value[1])^2
  rmse <- sqrt(mse)
  
  result <- list(series = shop.hw$x, fitted_series = shop.hw$fitted,
    prediction = shop.forecast,
    original_value = original_value,
    mse = mse, rmse = rmse,
    month_forecast = month_forecast)
  
  return(result)
}