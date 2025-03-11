# Load required packages
library(rtracklayer)  # For importing BigWig files
library(Gviz)         # For genome visualization
library(ggplot2)      # Alternative for custom plots
library(cowplot)      # For combining plots
# Allow arbitrary chromosome identifiers
options(ucscChromosomeNames = FALSE)
# List the Bigwig Files
fivea_file <- "C:\\temp20240826\\20250109\\05a_coverage_100bp.bw"
sixb_file <- "C:\\temp20240826\\20250109\\06b_coverage_100bp.bw"
fiveb_17_file <- "C:\\temp20240826\\20250109\\05b_barcode17_coverage_100bp.bw"
fiveb_18_file <- "C:\\temp20240826\\20250109\\05b_barcode18_coverage_100bp.bw"
fiveb_19_file <- "C:\\temp20240826\\20250109\\05b_barcode19_coverage_100bp.bw"
fiveb_20_file <- "C:\\temp20240826\\20250109\\05b_barcode20_coverage_100bp.bw"
sevenb_21_file <- "C:\\temp20240826\\20250109\\07b_barcode21_coverage_100bp.bw"
sevenb_22_file <- "C:\\temp20240826\\20250109\\07b_barcode22_coverage_100bp.bw"
sevenb_23_file <- "C:\\temp20240826\\20250109\\07b_barcode23_coverage_100bp.bw"
sevenb_24_file <- "C:\\temp20240826\\20250109\\07b_barcode24_coverage_100bp.bw"
ML82_file <- "C:\\temp20240826\\20250109\\ML82_coverage_100bp.bw"
Ras3_file <- "C:\\temp20240826\\20250109\\Ras3_coverage_100bp.bw"
# 文件路径
bigwig_files <- c(fivea_file, sixb_file, fiveb_17_file, fiveb_18_file, fiveb_19_file, fiveb_20_file, sevenb_21_file, sevenb_22_file, sevenb_23_file, sevenb_24_file, ML82_file, Ras3_file  )
track_names <- c("5A", "6B", "5B_1","5B_2","5B_3","5B_4","7B_1","7B_2","7B_3","7B_4","ML82","Ras3") 
chromosomes_to_plot <- c("2L", "2R", "3L", "3R", "X", "4")

# Use a loop to read bigwig files
bw_list <- list()
for (i in 1:length(bigwig_files)) {
  bw_name <- track_names[i]
  bw_list[[bw_name]] <- import.bw(bigwig_files[i])
}

# Use a loop to calculate the sum of the score of all samples.
bw_sum <- 0
for (i in 1:length(bigwig_files)) {
  bw_name <- track_names[i]
  cat("The sum of the scores of", bw_name, "is", sum(score(bw_list[[bw_name]])), "\n")
  bw_sum <- bw_sum + sum(score(bw_list[[bw_name]]))
}
cat("The sum of the scores of all samples is", bw_sum, "\n")


plot_list <- list()
# Check the data distribution before normalization
for (i in 1:length(bigwig_files)) {
  bw_name <- track_names[i]
  bw_temp <- bw_list[[bw_name]]
  plot_list[[i]] <- ggplot(data.frame(value = bw_temp$score), aes(x = value)) +
    geom_density(fill = "blue", color = "black", alpha = 0.5) +
    labs(title = paste0("Counts Distribution of ",bw_name), x = "Counts", y = "Density") +
    theme_minimal()+
    theme(
      panel.grid = element_blank(), # 去除背景网格线
      panel.border = element_rect(color = "black", fill = NA),
      panel.background = element_rect(fill = "white", color = NA), # 背景填充为白色
      #axis.line = element_line(color = "black") # 添加坐标轴线
    )
}
final_plot <- plot_grid(plotlist = plot_list, ncol = 3)

# Use a loop to perform the normalization
for (i in 1:length(bigwig_files)) {
  bw_name <- track_names[i]
  bw_temp <- bw_list[[bw_name]]
  bw_temp_subsum <- sum(score(bw_temp))
  bw_temp$score <- bw_temp$score / bw_temp_subsum * bw_sum
  bw_temp$score <- log2(bw_temp$score + 1)
  bw_list[[bw_name]] <- bw_temp
}


# Plot the normalized data, every sample a track
# Transfer the list to a data frame
# 创建一个空的数据框
all_data <- data.frame()

# 遍历bw_list中的每个样本
for (sample_name in names(bw_list)) {
  bw_data <- bw_list[[sample_name]]
  # 提取需要的染色体数据
  bw_data_filtered <- bw_data[seqnames(bw_data) %in% chromosomes_to_plot]
  # 转换成数据框，包含染色体、得分和样本信息
  sample_data <- data.frame(
    chrom = as.character(seqnames(bw_data_filtered)),
    score = score(bw_data_filtered),
    sample = sample_name
  )
  
  # 将数据合并
  all_data <- rbind(all_data, sample_data)
}
ggplot(all_data, aes(x = chrom, y = score, fill = sample)) +
  geom_bar(stat = "identity", position = "dodge") +  # 可以使用position = "dodge"来分隔不同样本的条形
  theme_minimal() +
  facet_wrap(~sample, scales = "free_y") +  # 每个样本分面显示
  labs(title = "Score by Chromosome and Sample",
       x = "Chromosome",
       y = "Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # 调整X轴标签显示
ggplot(all_data, aes(x = score, fill = sample)) +
  geom_density(alpha = 0.6) +  # 调整透明度使得不同样本不重叠
  theme_minimal() +
  facet_wrap(~sample, scales = "free_y") +  # 每个样本分面显示
  labs(title = "Density Plot of Scores across Chromosomes and Samples",
       x = "Score",
       y = "Density")
## New lines
library(ggplot2)
library(dplyr)
library(GenomicRanges)

# 假设你有一个包含多个样本的GRanges对象bw_list
# 和一个感兴趣的染色体列表chromosomes_to_plot
chromosomes_to_plot <- c("2L", "2R", "3L", "3R", "X", "4")

# 创建一个空的数据框来保存所有样本的数据
all_data <- list()

# 遍历每个样本的数据
for (sample_name in names(bw_list)) {
  bw_data <- bw_list[[sample_name]]
  
  # 筛选感兴趣的染色体
  bw_data_filtered <- bw_data[seqnames(bw_data) %in% chromosomes_to_plot]
  
  # 将结果存储到列表中
  all_data[[sample_name]] <- as.data.frame(bw_data_filtered)
}

# 将所有样本的数据合并为一个数据框
all_data_df <- bind_rows(all_data, .id = "sample")

# 归一化得分（如果需要的话）
all_data_df <- all_data_df %>%
  group_by(sample) %>%
  mutate(scaled_score = (score - min(score)) / (max(score) - min(score)))

# 获取每个染色体的长度
chrom_lengths <- sapply(chromosomes_to_plot, function(chrom) {
  chrom_data <- bw_list[[1]][seqnames(bw_list[[1]]) == chrom]
  return(sum(width(chrom_data)))
})

# 创建染色体位置的变量
all_data_df$chrom_position <- mapply(function(chrom, start) {
  chrom_length <- chrom_lengths[chrom]
  return(start / chrom_length)
}, all_data_df$seqnames, all_data_df$start)

# 创建染色体的分隔线
chromosome_lines <- data.frame(
  chrom = unique(all_data_df$seqnames),
  x = cumsum(sapply(unique(all_data_df$seqnames), function(chrom) {
    sum(all_data_df$end[all_data_df$seqnames == chrom]) # 计算每个染色体的总长度
  }))
)

# 绘制图形
ggplot(all_data_df, aes(x = start, y = score, color = sample)) +
  geom_segment(aes(xend = end, yend = score), size = 0.5) +  # 绘制每个窗口的水平线
  facet_wrap(~ sample, scales = "free_y", ncol = 1) +  # 每个样本一个轨道
  scale_x_continuous(
    breaks = chromosome_lines$x, 
    labels = chromosome_lines$chrom, 
    expand = c(0, 0)
  ) +  # X轴为不同染色体
  labs(x = "Chromosome", y = "Density", title = "Score Density per Sample Across Chromosomes") +  # 设置标签
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # X轴标签旋转
    strip.text = element_text(size = 10),  # 调整每个样本的标题大小
    axis.ticks.x = element_blank(),  # 移除X轴的刻度线
    panel.spacing = unit(0.5, "lines")  # 增加样本轨道之间的间隔
  ) +
  geom_vline(data = chromosome_lines, aes(xintercept = x), color = "black", linetype = "dashed")  # 染色体分隔线




