library(ggplot2)
df<- read.table(file = "clipboard", sep = "\t", header = TRUE)

df_filtered <- df[df$measure == "DALYs (Disability-Adjusted Life Years)" & df$year == 2019 & df$risk == "Tobacco", ]

df_filtered$age <- factor(df_filtered$age, levels = unique(df_filtered$age))

ggplot(df_filtered, aes(x = age, y = val, group = sex)) +
  geom_ribbon(data = df_filtered[df_filtered$sex == "Male", ], aes(ymin = lower, ymax = upper, fill = sex), alpha = 0.2, fill = "#96dafa") +
  geom_ribbon(data = df_filtered[df_filtered$sex == "Female", ], aes(ymin = lower, ymax = upper, fill = sex), alpha = 0.2, fill = "#cbb6ff") +
  geom_ribbon(data = df_filtered[df_filtered$sex == "Both", ], aes(ymin = lower, ymax = upper, fill = sex), alpha = 0.3, fill = "#b2c8da") +
  geom_line(aes(color = sex), size = 1.5) + 
  geom_point(aes(color = sex), size = 3) +  
  scale_color_manual(values = c("Both" = "#7c93bd", "Male" = "#96dafa", "Female" = "#cbb6ff")) + 
  labs(
    x = "Age Group",
    y = "DALY rates" 
  ) +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 10, face = "bold", family = "Arial", color = "black"),
    axis.title.y = element_text(size = 10, face = "bold", family = "Arial", color = "black"),
    axis.text.x = element_text(size = 10, family = "Arial", color = "black"),
    axis.text.y = element_text(size = 10, family = "Arial", color = "black"),
    legend.position = "none"  
  )

