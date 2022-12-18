# MSPE for one stock



mspe_df <- merge(mkt_df[,c("date_id", "minute", "cum_returns")],
                 df_tickers_for_use[[1]]$BAX[,c("date_id", "minute", "cum_returns")],
                 by=c("date_id", "minute"),
                 suffixes = c("_mkt", "_BAX"))

# Including predictions in data frame
# Linear - EOD

lin_pred <- null_linear_regression(df_tickers_for_use[[1]]$MAA,mkt_df) 
mspe_df$preds_eod <- predict(lin_pred, data.frame(X=mspe_df$cum_returns_mkt))

# Linear - Full

lin_pred_full <- lm(cum_returns_MAA ~ cum_returns_mkt, data =mspe_df)
mspe_df$preds_full <- predict(lin_pred_full, data.frame(cum_returns_mkt=mspe_df$cum_returns_mkt))

# FDA

fda_pred <- null_fda_regression(df_tickers_for_use[[1]]$MAA, mkt_df)
beta_fda <- summary(fda_pred)$p.table[2, 1]
alpha_fda <- summary(fda_pred)$p.table[1, 1]
mspe_df$preds_fda <- alpha_fda + beta_fda*mspe_df$cum_returns_mkt

mspe_fda <- sum(((length(mspe_df$cum_returns_BAX))^-1)*(mspe_df$cum_returns_BAX-mspe_df$preds_fda)^2)
mspe_eod <- sum(((length(mspe_df$cum_returns_BAX))^-1)*(mspe_df$cum_returns_BAX-mspe_df$preds_eod)^2)
mspe_full <- sum(((length(mspe_df$cum_returns_BAX))^-1)*(mspe_df$cum_returns_BAX-mspe_df$preds_full)^2)
