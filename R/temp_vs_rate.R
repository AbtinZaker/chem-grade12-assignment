library("ggplot2")

df <- read.csv("./data/temp.csv")

dim(df)

head(df)

colnames(df)

# changing column names into desired names
colnames(df)[colnames(df) == "Time"] <- "rate"
colnames(df)[colnames(df) == "Rec_Temp"] <- "rec_temp"

# generate error bars for the experimental results
df$er <- 1
df$upper_rate <- 1/((1/df$rate) + df$er)
df$lower_rate <- 1/((1/df$rate) - df$er)

# use the rate of raction model to create a predicted rate curve
set.seed(123)
model <- lm(rate ~ log(rec_temp, base = exp(1)) + 0, data = df)
pred_df <- data.frame(rec_temp = seq(0,50, length.out = 1000))
pred_df$rate <- predict(model, pred_df)

# plotting
p <- ggplot(df, aes(x = rec_temp, y = rate))+
  geom_point(size = 3, color = "#8be04e", alpha = 0.95)+
  geom_errorbar(aes(ymin = lower_rate, ymax = upper_rate), width = 1)+
  geom_line(data = pred_df, color = "#dc0ab4", linewidth = 1.5, alpha = 0.35)+
  theme_classic()+
  labs(x = "Temperature (°C)", y = "Rate of Reaction")+
  scale_x_continuous(limits = c(0,50), expand = c(0,0), breaks = c(seq(0,50,10))) +
  scale_y_continuous(limits = c(0,0.13), expand = c(0,0)) +
  theme(text = element_text(face = "bold"))

pdf(file = "./results/Temperature vs Rate of Reaction.pdf",
    height = 5,
    width = 5)
plot(p)
dev.off()
