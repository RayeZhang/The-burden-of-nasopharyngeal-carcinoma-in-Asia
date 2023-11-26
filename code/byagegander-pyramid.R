library(ggplot2)
library(dplyr)

df<-read.table("clipboard",sep = '\t',header = T)

df$val[df$sex == "Male"] <- -df$val[df$sex == "Male"]

ggplot(df, aes(x = age, y = val, fill = sex)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Age groups",
    y = "DALY rates",
    fill = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "#ffdaca", "Male" = "#9ddce2")) +
  theme(
    axis.title.x = element_text(size = 16, color = "black", family = "Arial",),
    axis.title.y = element_text(size = 16, color = "black", family = "Arial",),
    axis.text.x = element_text(size = 14, color = "black", family = "Arial",),
    axis.text.y = element_text(size = 14, color = "black", family = "Arial",),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"  # This line removes the legend
  )+
  scale_x_continuous(breaks = seq(-200, by = 100)) 


