plot_shop <- function(test_shop){
  plot(test_shop$series, 
    xlim = c(2013, 2016), typ = "b", pch = 16, ylim = c(0, 1.1 * max(test_shop$series)),
    las = 1, ylab = "Quantity", xlab = "Month", xaxt = 'n',
    main = paste0("Monthly units sold by shop ", shop_id, ", forecast for month: ", month_forecast))
  #x_ticks <- axis.Date(1, test_shop$series, labels = F)
  #time(ts(0, 2014, 2016, frequency = 12))
  lines(test_shop$fitted_series[ , 1], typ = "b", col = "red")
  
  x_labels <- strftime(seq(as.Date("2013-01-01"), as.Date("2016-01-01"), by = "6 month"), "%b-%y")
  x_ticks <- axis(1, seq(2013, 2016, by = 0.5), labels = x_labels)
  
  points(test_shop$prediction, pch = 16, col ="red")
  points(test_shop$original_value, pch = 16)
  grid()
  abline(v = test_shop$month_forecast, col = "darkred", lty = 3)
}