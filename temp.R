library(ggplot2)
library(dplyr)

# Step 1: Create sample data with more positions
set.seed(42)

# Defining chromosomes and their position ranges
chromosomes <- c("2L", "4R", "3R")
n_samples <- 5
n_points <- 100

# Expanding the range of positions for each chromosome
data <- data.frame(
  Sample = rep(paste("Sample", 1:n_samples), each = n_points * length(chromosomes)),
  Chromosome = rep(chromosomes, each = n_points * n_samples),
  Score = c(rnorm(n_points * n_samples, mean = 5, sd = 1),
            rnorm(n_points * n_samples, mean = 10, sd = 2),
            rnorm(n_points * n_samples, mean = 15, sd = 3)),
  Position = c(seq(1, 20, length.out = n_points),
               seq(20, 30, length.out = n_points),
               seq(30, 60, length.out = n_points))
)

# Step 2: Define custom colors for each sample
sample_colors <- c(
  "Sample 1" = "blue",  # Blue
  "Sample 2" = "blue",  # Orange
  "Sample 3" = "#2ca02c",  # Green
  "Sample 4" = "#d62728",  # Red
  "Sample 5" = "#9467bd"   # Purple
)

# Step 3: Plot using ggplot2
ggplot(data, aes(x = Position, y = Score, fill = Sample)) +  # Position on X, Score on Y
  geom_col(position = "dodge", width = 1) +  # Use geom_col() for bar plot
  facet_wrap(~ Sample, scales = "fixed", ncol = 1, strip.position = "left") +  # Separate track for each sample, fixed Y-axis scale
  scale_x_continuous(
    breaks = c(10, 25, 45),  # Set breaks in the middle of each chromosome range
    labels = chromosomes  # Add labels at chromosome centers
  ) +  # Chromosome labels on X-axis
  scale_fill_manual(values = sample_colors) +  # Set custom colors for samples
  #theme_minimal() +
  theme(
    #strip.text = element_blank(),  # Remove facet label text at the top
    #strip.text = element_text(size = 12, vjust = 1),  # Adjust facet label text size and alignment
    strip.text = element_text(size = 12, angle = 270),  # Adjust facet label text size and alignment
    panel.grid = element_blank(),  # Remove grid lines
    axis.text.x = element_text(hjust = 0.5),  # Center X-axis labels
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12),  # Remove default Y axis title
    strip.background = element_rect(fill = "lightblue",color="black"),  # Remove background for facet labels
    axis.text.y = element_text(size = 5),  # Adjust Y-axis labels (sample names)
    panel.background = element_rect(fill = "white"),  # Set plot background color
    panel.border = element_rect(color = "black", fill = NA),  # Add border around the plot
  ) +
  # Add vertical lines to separate chromosomes
  geom_vline(xintercept = c(20, 30), color = "black", size = 0.2)  # Chromosome separator lines
