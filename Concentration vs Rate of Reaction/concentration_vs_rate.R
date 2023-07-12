library("ggplot2")

df <- read.csv("./concen.csv")

table(df)

# changing column names
colnames(df)[colnames(df) == "Time"] <- "rate"
colnames(df)[colnames(df) == "Concen"] <- "concen"

# generate error bars for the experimental results
df$er <- 1 
df$upper_rate <- 1/((1/df$rate) + df$er)
df$lower_rate <- 1/((1/df$rate) - df$er)

# setting errorbar size
errorbar_size <- 0.001


# use the rate of raction model to create a predicted rate curve
set.seed(123)
model<- lm(rate ~ concen + 0, data = df)
pred_df <- data.frame(concen = seq(0,0.11, length.out = 1000))
pred_df$rate <- predict(model, pred_df)



# plotting
ggplot(df, aes(x = concen, y = rate, group=1))+
  geom_point(size = 3, color = "#8be04e", alpha = 0.95)+
  geom_segment(mapping = aes(x = concen, xend = concen, y = lower_rate, yend = upper_rate)) +
  geom_segment(mapping = aes(x = concen + errorbar_size, xend = concen - errorbar_size, y = lower_rate, yend = lower_rate)) +
  geom_segment(mapping = aes(x = concen + errorbar_size, xend = concen - errorbar_size, y = upper_rate, yend = upper_rate)) +
  geom_line(data = pred_df, color = "#dc0ab4", linewidth = 1.5, alpha = 0.35)+
  theme_classic()+
  labs(x = "Concentration (mol/L)", y = "Rate of Reaction")+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,0.14), breaks = seq(0,0.15,0.02), expand = c(0,0)) +
  theme(text = element_text(face = "bold"))



