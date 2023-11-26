
data<-read.table("clipboard",sep = '\t',header = T)
library(ggplot2)

ggplot(data, aes(x = age, y = AAPC, fill = gender, group = gender)) +
  geom_bar(stat="identity", position=position_dodge(dodge_width)) + 
  geom_errorbar(
    aes(ymin = AAPC-SE, ymax = AAPC+SE), 
    position=position_dodge(dodge_width), 
    width = 0.25
  ) +
  scale_fill_manual(values = c("Male" = "#86dde5", "Female" = "#ffc6b2"), 
                    labels = c("Male" = "Male", "Female" = "Female")) +
  theme_light() +
  labs(y="AAPC(%)", x="Age", fill="gender") +
  theme(
    axis.title.x = element_text(size = 16, family = "Arial", colour = "black"),
    axis.title.y = element_text(size = 16, family = "Arial", colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, family = "Arial", colour = "black"),
    axis.text.y = element_text(size = 16,family = "Arial", colour = "black"),
    legend.text = element_text(size = 16,family = "Arial"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  )
