imspe_df_train <- list()

for (i in ltest){
  
  imspe_df_train[[paste0("symbol ",i)]]$date_id <- df_tickers_for_use[[1]][[i]]["date_id"][dim(df_tickers_for_use_train[[paste0("symbol ",i)]]["date_id"])[1]:nrow(df_tickers_for_use[[1]][[i]]["date_id"]),]
  
  imspe_df_train[[paste0("symbol ",i)]]$y_fda <- df_tickers_for_use[[1]][[i]]["cum_returns"][dim(df_tickers_for_use_train[[paste0("symbol ",i)]]["cum_returns"])[1]:nrow(df_tickers_for_use[[1]][[i]]["cum_returns"]),]
  imspe_df_train[[paste0("symbol ",i)]]$yhat_fda <- (colMeans(mspe_df[[paste0("symbol ",i)]]["alpha_fda"])+colMeans(mspe_df[[paste0("symbol ",i)]]["beta_fda"])*(mkt_df["cum_returns"][dim(df_tickers_for_use_train[[paste0("symbol ",i)]]["cum_returns"])[1]:nrow(df_tickers_for_use[[1]][[i]]["cum_returns"]),]))
  
  imspe_df_train[[paste0("symbol ",i)]]$y_full <- df_tickers_for_use[[1]][[i]]["cum_returns"][dim(df_tickers_for_use_train[[paste0("symbol ",i)]]["cum_returns"])[1]:nrow(df_tickers_for_use[[1]][[i]]["cum_returns"]),]
  imspe_df_train[[paste0("symbol ",i)]]$yhat_full <- (colMeans(mspe_df[[paste0("symbol ",i)]]["alpha_full"])+colMeans(mspe_df[[paste0("symbol ",i)]]["beta_full"])*(mkt_df["cum_returns"][dim(df_tickers_for_use_train[[paste0("symbol ",i)]]["cum_returns"])[1]:nrow(df_tickers_for_use[[1]][[i]]["cum_returns"]),]))
  
  
  imspe_df_train[[paste0("symbol ",i)]]$y_eod <- df_tickers_for_use[[1]][[i]]["cum_returns"][dim(df_tickers_for_use_train[[paste0("symbol ",i)]]["cum_returns"])[1]:nrow(df_tickers_for_use[[1]][[i]]["cum_returns"]),]
  imspe_df_train[[paste0("symbol ",i)]]$yhat_eod <- (colMeans(mspe_df[[paste0("symbol ",i)]]["alpha_eod"])+colMeans(mspe_df[[paste0("symbol ",i)]]["beta_eod"])*(mkt_df["cum_returns"][dim(df_tickers_for_use_train[[paste0("symbol ",i)]]["cum_returns"])[1]:nrow(df_tickers_for_use[[1]][[i]]["cum_returns"]),]))
  
  
}


test_imspe <- imspe_df_train$`symbol HCA` %>%
  select(., date_id)

testsplit <- split(test_imspe, test_imspe$date_id)

stock <- list()

for (i in length(testsplit)){
  stock[i] <- pracma::trapz(as.numeric(unlist(testsplit[[i]]["date_id"])),
                            (as.numeric(unlist(testsplit[[i]]["cum_returns_stock"]))-as.numeric(unlist(testsplit[[i]]["cum_returns_mkt"]))))
}

