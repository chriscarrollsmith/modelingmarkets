
#Plot yield vs. date
joined_df %>% 
  mutate(twelve_month_pce_strat = factor(round(twelve_month_pce))) %>%
  ggplot(aes(x=date,y=mean_yield)) +
  geom_point() +
  geom_smooth(method="lm",formula=y~log(x)) +
  labs(x="Date",y="Monthly Average 10-Year Yield",title="10-Year Yield over Time",caption="Copyright Wall Street Petting Zoo 2022")

#save the plot
ggsave(filename = "yield-over-time.png",
       scale = 1,
       width = 1920/200,
       height = 1080/200,
       units = "in",
       path="C:/Users/chris/OneDrive/Pictures/Infographics")

#Stratify by PCE inflation rate and plot yield vs. date
joined_df %>% 
  mutate(twelve_month_pce_strat = factor(round(twelve_month_pce))) %>%
  group_by(twelve_month_pce_strat) %>%
  mutate(n=n()) %>%
  filter(n>20) %>%
  ggplot(aes(x=date,y=mean_yield)) +
  geom_point() +
  geom_smooth(method="lm",formula=y~log(x)) +
  facet_wrap( ~ twelve_month_pce_strat) +
  labs(x="Date",y="Monthly Average 10-Year Yield",title="10-Year Yield over Time, Stratified by PCE Inflation Rate",caption="Copyright Wall Street Petting Zoo 2022")

#save the plot
ggsave(filename = "pce-yield-over-time.png",
       scale = 1,
       width = 1920/200,
       height = 1080/200,
       units = "in",
       path="C:/Users/chris/OneDrive/Pictures/Infographics")

#Subtract PCE inflation model from the data and regress for time effect
joined_df %>% 
  mutate(pce_model = predict(lm(mean_yield ~ log(twelve_month_pce), data = .)),
         mean_yield_residual = mean_yield - pce_model) %>%
  ggplot(aes(x=date,y=mean_yield_residual)) +
  geom_point() +
  geom_smooth(method="lm",formula=y~x) +
  labs(x="Date",y="Residual Monthly Average 10-Year Yield",title="Residual 10-Year Yield over Time after Subtracting PCE Inflation-Rate Log Regression Model",caption="Copyright Wall Street Petting Zoo 2022")

#save the plot
ggsave(filename = "residual-yield-over-time.png",
       scale = 1,
       width = 1920/200,
       height = 1080/200,
       units = "in",
       path="C:/Users/chris/OneDrive/Pictures/Infographics")

#Do a multiple linear regression accounting for both PCE inflation and date
joined_df %>%
  do(tidy(lm(mean_yield ~ sequence + log(twelve_month_pce), data = .)))
fit <- joined_df %>% lm(mean_yield ~ log(sequence) + log(twelve_month_pce), data = .)

#Predict the model for rates today
df <- data.frame(twelve_month_pce = c(seq(1,8.5,by=0.5),last(joined_df$twelve_month_pce)),sequence=rep(530,times=17)) %>%
  mutate(predicted_yield = predict(fit,newdata = .)) %>%
  mutate(highlight = twelve_month_pce %in% last(joined_df$twelve_month_pce))
predicted_yield <- predict(fit,newdata=data.frame(twelve_month_pce = last(joined_df$twelve_month_pce),sequence = last(joined_df$sequence)))

#Plot the rate prediction curve for 2022
df %>% 
  ggplot(aes(x=twelve_month_pce,y=predicted_yield,col=highlight)) +
  geom_point(size=2,show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  geom_text(data=df[df$highlight==TRUE,],aes(x=twelve_month_pce,y=predicted_yield,label=round(predicted_yield,1)),nudge_x=-0.2) +
  scale_x_continuous(breaks=seq(0,10,by=.5)) + 
  labs(x="TTM PCE Inflation Rate",y="Monthly Average 10-Year Yield",title="Predicted 2022 10-Year Yield by PCE Inflation Rate",caption="Copyright Wall Street Petting Zoo 2022") + 
  theme(legend.position = "none") 

#save the plot
ggsave(filename = "2022-rate-prediction-curve.png",
       scale = 1,
       width = 1920/200,
       height = 1080/200,
       units = "in",
       path="C:/Users/chris/OneDrive/Pictures/Infographics")
