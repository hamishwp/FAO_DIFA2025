# Create example time series data
yrs<-35
time <- 1991:(1990+yrs)
data <- data.frame(
  time = rep(time, 5),
  value = c(
    cumsum(rnorm(yrs, mean = 0.05, sd = 0.1)),  # Dataset 1
    cumsum(rnorm(yrs, mean = 0.05, sd = 0.13)),  # Dataset 2
    cumsum(rnorm(yrs, mean = 0.05, sd = 0.06)),  # Dataset 3
    cumsum(rnorm(yrs, mean = 0.05, sd = 0.15)),  # Dataset 4
    cumsum(rnorm(yrs, mean = 0.05, sd = 0.1))   # Dataset 5
  ),
  category = rep(c("Income", "Fertiliser", "Facebook Accounts", "Sunglasses", "Pollution"), each = yrs)
) |> dplyr::mutate(value=sqrt(value^2))

# Plot the data using ggplot2
p<-ggplot(data, aes(x = time, y = value, colour = category)) +
  geom_line(size = 1) +  # Add lines
  labs(
    title = "Time Series Data with Spontaneous Correlation",
    x = "Time",
    y = "Value",
    colour = "Category"
  ) +
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  )

print(p)

ggsave("spontaneous_cor_ex.png",plot = p,path = "Plots/Other/",width=8,height = 6)
