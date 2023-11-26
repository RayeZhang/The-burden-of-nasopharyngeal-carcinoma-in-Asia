library(ggplot2)
data <- read.table("clipboard", sep = '\t', header = T)

data$countries <- factor(data$countries, levels = data$countries[order(data$aapc)])

asia_color <- ifelse(data$countries == "Asia", "#fce681", ifelse(data$aapc > 0, "#f5b0a5", "#75bfcf"))

p <- ggplot(data, aes(x = countries, y = aapc, fill = asia_color)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 0.6), width = 0.5) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.6)) + 
  scale_fill_identity() + 
  coord_flip() + 
  labs(x = "Countries or regions", y = "AAPC") + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 9, face = "plain", family = "Arial", color = "black"),
        axis.title.y = element_text(size = 9, face = "plain", family = "Arial", color = "black"),
        axis.text.x = element_text(size = 9, face = "plain", family = "Arial", color = "black"),
        axis.text.y = element_text(size = 9, face = "plain", family = "Arial", color = "black"))

p
