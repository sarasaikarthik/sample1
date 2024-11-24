# Load the dataset
file_path <- "Average_Food_Cost_Per_Person.csv"
data <- read.csv(file_path)

# Rename columns for consistency
colnames(data) <- c("State", paste0("Month_", 1:12), "Cumulative_Avg")

# Compute the median of the cumulative averages
median_cum_avg <- median(data$Cumulative_Avg, na.rm = TRUE)

# Classify states into higher or lower groups
data$Group <- ifelse(data$Cumulative_Avg > median_cum_avg, 
                     "Higher Cumulative Avg Group", 
                     "Lower Cumulative Avg Group")

# Prepare data for visualization
months <- paste0("Month_", 1:12)
long_data <- data.frame(
  State = rep(data$State, each = 12),
  Month = rep(1:12, times = nrow(data)),
  Food_Cost = as.numeric(as.vector(as.matrix(data[, months]))), # Ensure numeric
  Group = rep(data$Group, each = 12)
)

# Remove rows with missing or invalid Food_Cost values
long_data <- long_data[!is.na(long_data$Food_Cost), ]

# Save visualizations to a PDF
pdf("Visualization.pdf")


# Histogram for monthly food costs with bell curves
hist(
  long_data$Food_Cost[long_data$Group == "Higher Cumulative Avg Group"], 
  col = rgb(0, 0, 1, 0.5), breaks = 10, xlim = range(long_data$Food_Cost, na.rm = TRUE),
  main = "Histogram with Bell Curves of Monthly Food Costs by Group", xlab = "Monthly Food Cost", ylab = "Frequency"
)
# Add bell curve for Higher Group
higher_mean <- mean(long_data$Food_Cost[long_data$Group == "Higher Cumulative Avg Group"], na.rm = TRUE)
higher_sd <- sd(long_data$Food_Cost[long_data$Group == "Higher Cumulative Avg Group"], na.rm = TRUE)
x_higher <- seq(min(long_data$Food_Cost, na.rm = TRUE), max(long_data$Food_Cost, na.rm = TRUE), length = 100)
y_higher <- dnorm(x_higher, mean = higher_mean, sd = higher_sd) * length(long_data$Food_Cost[long_data$Group == "Higher Cumulative Avg Group"]) * diff(hist(long_data$Food_Cost, plot = FALSE)$breaks[1:2])
lines(x_higher, y_higher, col = "blue", lwd = 2)

# Add histogram for Lower Group
hist(
  long_data$Food_Cost[long_data$Group == "Lower Cumulative Avg Group"], 
  col = rgb(0, 1, 0, 0.5), breaks = 10, add = TRUE
)
# Add bell curve for Lower Group
lower_mean <- mean(long_data$Food_Cost[long_data$Group == "Lower Cumulative Avg Group"], na.rm = TRUE)
lower_sd <- sd(long_data$Food_Cost[long_data$Group == "Lower Cumulative Avg Group"], na.rm = TRUE)
x_lower <- seq(min(long_data$Food_Cost, na.rm = TRUE), max(long_data$Food_Cost, na.rm = TRUE), length = 100)
y_lower <- dnorm(x_lower, mean = lower_mean, sd = lower_sd) * length(long_data$Food_Cost[long_data$Group == "Lower Cumulative Avg Group"]) * diff(hist(long_data$Food_Cost, plot = FALSE)$breaks[1:2])
lines(x_lower, y_lower, col = "green", lwd = 2)

# Add legend
legend("topright", legend = c("Higher Cumulative Avg Group (Histogram)", 
                              "Lower Cumulative Avg Group (Histogram)", 
                              "Higher Cumulative Avg Group (Bell Curve)", 
                              "Lower Cumulative Avg Group (Bell Curve)"),
       fill = c(rgb(0, 0, 1, 0.5), rgb(0, 1, 0, 0.5), "blue", "green"), 
       lty = c(NA, NA, 1, 1), col = c(NA, NA, "blue", "green"), cex=0.75, title = "Groups")

# Boxplot comparing food costs between groups
boxplot(
  Food_Cost ~ Group, data = long_data,
  main = "Comparison of Monthly Food Costs by Group",
  xlab = "Group", ylab = "Monthly Food Cost",
  col = c("lightblue", "lightgreen")
)
legend("topright", legend = c("Higher Cumulative Avg Group", "Lower Cumulative Avg Group"), 
       fill = c("lightblue", "lightgreen"), title = "Groups")

dev.off()

