# Generate a file with `pairtools stats mapping.mapped.pairs > mapping.mapped.pairs.stats.txt`
# We only need the lines start from 'chrom_freq'

# 读取文件的所有内容
lines <- readLines(choose.files())

# 筛选以 chrom_freq 开头的行
chrom_freq_lines <- grep("^chrom_freq", lines, value = TRUE)

# Split the lines
library(stringr)
chrom_freq <- str_split(chrom_freq_lines, "\t|/")
chrom_freq_table <- do.call(rbind, chrom_freq)
chrom_freq_table <- as.data.frame(chrom_freq_table, stringsAsFactors = FALSE)
colnames(chrom_freq_table) <- c("Title","ChrA","ChrB","Count")
# Remove the title
chrom_freq_table <- chrom_freq_table[, !colnames(chrom_freq_table) %in% "Title"]
chrom_freq_table$Count <- as.numeric(chrom_freq_table$Count)


# Check a table for one chromosoome
data_selected <- subset(chrom_freq_table, ChrA == "chr2L_sim")
data_selected$Percentage <- data_selected$Count / sum(data_selected$Count)

# 加载 scales 包
library(scales)
library(ggplot2)

# 绘制饼图并添加标签
ggplot(data_selected, aes(x = "", y = Percentage, fill = ChrB)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5)) +  # 在堆叠中心添加文本
  labs(x = NULL, y = NULL, fill = "Category") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
