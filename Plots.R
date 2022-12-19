library("pander")
## Data

AMZN <- av_get(symbol = "AMZN", av_fun = "TIME_SERIES_INTRADAY_EXTENDED", 
               interval = "1min", outputsize = "full")

VOO <- av_get(symbol = "VOO", av_fun = "TIME_SERIES_INTRADAY_EXTENDED", 
              interval = "1min", outputsize = "full")

AMZN_cum <- convert_to_cum(AMZN)
VOO_cum <- convert_to_cum(VOO)

## Figure 1 - Cumulative return curves

# Convert to cum plot - AMZN
amzn_cum_plot <- ggplot(AMZN_cum,
                        aes(x=minute, y=(cum_returns)*100, color=as.factor(date_id)), title ="x") + 
  geom_line() +
  labs(
    x = "Minute",
    y = NULL,
    color = "Trading Day"
  ) +
  theme_bw() +
  ggtitle("AMZN Cumulative Returns")

# Convert to cum plot - VOO
voo_cum_plot <- ggplot(VOO_cum,
                       aes(x=minute, y=(cum_returns)*100, color=as.factor(date_id))) + 
  geom_line() +
  labs(
    x = "Minute",
    y = NULL,
    color = "Trading Day"
  ) +
  theme_bw() +
  ggtitle("VOO Cumulative Returns")

amzn_cum_plot / voo_cum_plot + ylim(-4, 6)


# Figure 2 - Normal return

AMZN_ret <-AMZN %>%
  mutate(lag_price = lag(close))

AMZN_ret1 <- AMZN_ret %>%
  mutate(ret = 100*((close-lag_price)/lag_price))

AMZN_ret2 <- AMZN_ret1 %>%
  select(time,ret) %>%
  filter(time > "2022-12-16")

VOO_ret <-VOO %>%
  mutate(lag_price = lag(close))

VOO_ret1 <- VOO_ret %>%
  mutate(ret = 100*((close-lag_price)/lag_price))

VOO_ret2 <- VOO_ret1 %>%
  select(time,ret) %>%
  filter(time > "2022-12-16")


amzn_daily_ret <-  AMZN_ret2 %>%
  ggplot2::ggplot(aes(x = time, y = ret)) +
  geom_line() +
  labs(
    x = NULL, y = NULL
  ) +
  theme_bw() +
  ggtitle("AMZN Returns")

voo_daily_ret <- VOO_ret2 %>%
  ggplot2::ggplot(aes(x = time, y = ret)) +
  geom_line() +
  labs(
    x = NULL, y = NULL
  ) +
  theme_bw() +
  ggtitle("VOO Returns")

amzn_daily_ret / voo_daily_ret + ylim(-0.5, 0.5)


# Figure 3 - Beta plot

betas_df <- list()
fda_beta_list <- list()
eod_lin_beta_list <- list()
full_lin_beta_list <- list()

r_tickers <- random_tickers[1:41,1]
r_tickers <- r_tickers[-4]

#FDA

for (i in r_tickers){
  fda_beta_list[i] <- coef(null_fda_regression(df_tickers_for_use[[1]][[i]],mkt_df))$pterms[2,1]
}

#EOD

for (i in r_tickers){
  eod_lin_beta_list[i] <- coef(null_linear_regression(df_tickers_for_use[[1]][[i]],mkt_df))[2]
}

#Full

for (i in r_tickers){
  full_lin_beta_list[i] <- coef(lm(cum_returns_stock ~ cum_returns_mkt, data=mspe_df[[paste0("symbol ",i)]]))[2]
}


betas <- cbind(fda_beta_list, eod_lin_beta_list, full_lin_beta_list, (1:40))
betas_df <- as.data.frame(betas)



tickers <- as.list(rownames(betas_df))

tibble(
  FFM = c(unlist(fda_beta_list)),
  SLM = c(unlist(eod_lin_beta_list)),
  FSLM = c(unlist(full_lin_beta_list)),
  cat = factor(tickers, levels = rev(tickers))
) -> betas_for_figure

plot3 <-ggplot() +
  geom_segment(
    data = gather(betas_for_figure, measure, val, -cat) %>% 
      group_by(cat) %>% 
      top_n(-1) %>% 
      slice(1) %>%
      ungroup(),
    aes(x = 0.01, xend = val, y = cat, yend = cat),
    linetype = "dotted", linewidth = 0.5, color = "gray80"
  ) +
  geom_point(
    data = gather(betas_for_figure, measure, value, -cat),
    aes(value, cat, color = measure), 
    size = 3,
  ) + scale_color_manual(values = c("FFM" = "skyblue2", "SLM" = "orange1", "FSLM" = "green4")) + #ÆNDR NAVNE SÅ DE MATCHER TIDLIGERE DEFINITION HER
  labs(
    x = "Beta-value", y = NULL,
  ) +
  theme_bw() +
  theme(legend.position = c(.9,.8875), legend.title = element_blank(), legend.text=element_text(size=12), axis.title = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(size=0.1, color="gray80"),
        panel.grid.minor.x = element_line(size=0.1, color="gray80")
  ) + scale_x_continuous(expand = expand_scale(mult=c(0,0.1)), limits = c(0, 1.8), breaks = seq(0.2,1.8,by=0.2))


# Figure 4 - Boxplot

fda_min_eod <- list()
fda_min_full <- list()
full_min_eod <- list()

fda_min_eod <- mapply('-', fda_beta_list, eod_lin_beta_list, SIMPLIFY = FALSE)
fda_min_full <- mapply('-', fda_beta_list, full_lin_beta_list, SIMPLIFY = FALSE)
full_min_eod <- mapply('-', full_lin_beta_list, eod_lin_beta_list, SIMPLIFY = FALSE)

tibble(
  "FFM - SLM" = c(unlist(fda_min_eod)),
  "FFM - FSLM" = c(unlist(fda_min_full)),
  "FSLM - SLM" = c(unlist(full_min_eod))
) -> boxplot_df

melt(boxplot_df) -> boxplot_melt

plot4 <-ggplot(boxplot_melt, aes(x = variable, y = value)) +
geom_boxplot() + 
  theme_bw() +  
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())

plot4 + stat_summary(fun=mean, geom="point", shape=20, size=4, color ="red", fill ="red")

# Table 2 - t.test

beta_diff_fda_eod <- as.numeric(betas_df$fda_beta_list)-as.numeric(betas_df$eod_lin_beta_list)
beta_diff_fda_full <- as.numeric(betas_df$fda_beta_list)-as.numeric(betas_df$full_lin_beta_list)
beta_diff_full_eod <- as.numeric(betas_df$full_lin_beta_list)-as.numeric(betas_df$eod_lin_beta_list)
beta_diff_fda_eod_1 <- as.numeric(betas_df$eod_lin_beta_list)-as.numeric(betas_df$fda_beta_list)

fda_eod_ttest <- t.test(beta_diff_fda_eod)
pander(fda_eod_ttest)

pander(t.test(beta_diff_fda_full))
pander(t.test(beta_diff_full_eod))

colMeans(boxplot_df
         )

# Figure 5 - MSPE plot

mspe_df_train <- list()
mspe_fda <- list()
mspe_full <- list()
mspe_eod <- list()

for (i in r_tickers){
  
  
  mspe_df_train[[paste0("symbol ",i)]]$y_fda <- df_tickers_for_use[[1]][[i]]["cum_returns"][dim(df_tickers_for_use_train[[paste0("symbol ",i)]]["cum_returns"])[1]:nrow(df_tickers_for_use[[1]][[i]]["cum_returns"]),]
  mspe_df_train[[paste0("symbol ",i)]]$yhat_fda <- (colMeans(mspe_df[[paste0("symbol ",i)]]["alpha_fda"])+colMeans(mspe_df[[paste0("symbol ",i)]]["beta_fda"])*(mkt_df["cum_returns"][dim(df_tickers_for_use_train[[paste0("symbol ",i)]]["cum_returns"])[1]:nrow(df_tickers_for_use[[1]][[i]]["cum_returns"]),]))
  mspe_df_train[[paste0("symbol ",i)]]$y_fda_minus_yhat_fda_squared <- (mspe_df_train[[paste0("symbol ",i)]]$y_fda - mspe_df_train[[paste0("symbol ",i)]]$yhat_fda)^2
  mspe_df_train[[paste0("symbol ",i)]]$fda_MSPE <- colMeans(mspe_df_train[[paste0("symbol ",i)]]$y_fda_minus_yhat_fda_squared)*100
  mspe_fda[[paste0("symbol ",i)]]$fda_MSPE <- colMeans(mspe_df_train[[paste0("symbol ",i)]]$y_fda_minus_yhat_fda_squared)*100
  
  mspe_df_train[[paste0("symbol ",i)]]$y_full <- df_tickers_for_use[[1]][[i]]["cum_returns"][dim(df_tickers_for_use_train[[paste0("symbol ",i)]]["cum_returns"])[1]:nrow(df_tickers_for_use[[1]][[i]]["cum_returns"]),]
  mspe_df_train[[paste0("symbol ",i)]]$yhat_full <- (colMeans(mspe_df[[paste0("symbol ",i)]]["alpha_full"])+colMeans(mspe_df[[paste0("symbol ",i)]]["beta_full"])*(mkt_df["cum_returns"][dim(df_tickers_for_use_train[[paste0("symbol ",i)]]["cum_returns"])[1]:nrow(df_tickers_for_use[[1]][[i]]["cum_returns"]),]))
  mspe_df_train[[paste0("symbol ",i)]]$y_full_minus_yhat_full_squared <- (mspe_df_train[[paste0("symbol ",i)]]$y_full - mspe_df_train[[paste0("symbol ",i)]]$yhat_full)^2
  mspe_df_train[[paste0("symbol ",i)]]$full_MSPE <- colMeans(mspe_df_train[[paste0("symbol ",i)]]$y_full_minus_yhat_full_squared)*100
  mspe_full[[paste0("symbol ",i)]]$full_MSPE <- colMeans(mspe_df_train[[paste0("symbol ",i)]]$y_full_minus_yhat_full_squared)*100
  
  mspe_df_train[[paste0("symbol ",i)]]$y_eod <- df_tickers_for_use[[1]][[i]]["cum_returns"][dim(df_tickers_for_use_train[[paste0("symbol ",i)]]["cum_returns"])[1]:nrow(df_tickers_for_use[[1]][[i]]["cum_returns"]),]
  mspe_df_train[[paste0("symbol ",i)]]$yhat_eod <- (colMeans(mspe_df[[paste0("symbol ",i)]]["alpha_eod"])+colMeans(mspe_df[[paste0("symbol ",i)]]["beta_eod"])*(mkt_df["cum_returns"][dim(df_tickers_for_use_train[[paste0("symbol ",i)]]["cum_returns"])[1]:nrow(df_tickers_for_use[[1]][[i]]["cum_returns"]),]))
  mspe_df_train[[paste0("symbol ",i)]]$y_eod_minus_yhat_eod_squared <- (mspe_df_train[[paste0("symbol ",i)]]$y_eod - mspe_df_train[[paste0("symbol ",i)]]$yhat_eod)^2
  mspe_df_train[[paste0("symbol ",i)]]$eod_MSPE <- colMeans(mspe_df_train[[paste0("symbol ",i)]]$y_eod_minus_yhat_eod_squared)*100
  mspe_eod[[paste0("symbol ",i)]]$eod_MSPE <- colMeans(mspe_df_train[[paste0("symbol ",i)]]$y_eod_minus_yhat_eod_squared)*100
  
}

umspe_fda <- unlist(mspe_fda)
umspe_full <- unlist(mspe_full)
umspe_eod <- unlist(mspe_eod)

tibble(
  FFM = c(umspe_fda),
  SLM = c(umspe_eod),
  FSLM = c(umspe_full),
  cat = factor(r_tickers, levels = rev(r_tickers))
) -> mspe_for_figure

melt_mspe <- melt(na.omit(mspe_for_figure))

plot51 <- ggplot(melt_mspe, aes(value, cat, shape = factor(variable)))

plot511 <- plot51 + geom_point(aes(color = factor(variable))) 

plot5111 <- plot511 + scale_color_manual(values= c("FFM" = "skyblue2", "SLM" = "orange1", "FSLM" = "green4")) + 
  labs(
    x = "MSPE", y = NULL)

plot51111 <- plot5111 + scale_shape_manual(values = c("FFM" = 19, "SLM" = 16, "FSLM" = 8))

plot5 <- plot51111 + theme_bw() + 
  theme(legend.position = c(.9,.8875), legend.title = element_blank(), legend.text=element_text(size=12), axis.title = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(size=0.1, color="gray80"),
        panel.grid.minor.x = element_line(size=0.1, color="gray80")
  )

plot5

mean_mspe <- tibble(
  FFM = c(na.omit(mspe_for_figure[1])),
  SLM = c(na.omit(mspe_for_figure[2])),
  FSLM = c(na.omit(mspe_for_figure[3])),
)

melt_mean_mspe <- melt(mean_mspe)
rownames(melt_mean_mspe) <- c("FFM", "SLM", "FSLM")

plot6 <- ggplot(melt_mean_mspe, aes(x = value, y = variable)) + 
  geom_point(aes(shape=variable, color=variable), size=2)+
  scale_shape_manual(values=c(3, 16, 17))+
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9')) +
  theme(legend.position="top")

pander(t.test(na.omit(mspe_for_figure[1])-na.omit(mspe_for_figure[2])))
pander(t.test(na.omit(mspe_for_figure[1])-na.omit(mspe_for_figure[3])))
pander(t.test(na.omit(mspe_for_figure[2])-na.omit(mspe_for_figure[3])))
