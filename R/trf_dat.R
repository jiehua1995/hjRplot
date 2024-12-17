pak::pkg_install("tidyverse")
library(tidyverse)

# Read the '.dat' file
lines <- readLines(choose.files())
print(head(lines, 20))

# 初始化空列表用于存储结果
sequence_list <- list()
current_sequence <- NULL

# 循环解析每一行
for (line in lines) {
  # 提取Sequence名称
  if (grepl("^Sequence:", line)) {
    current_sequence <- str_trim(str_extract(line, "(?<=Sequence:\\s)\\S+"))
  }
  
  # 提取数据行，跳过空行和非数据行
  else if (grepl("^\\d+", line)) {
    # 使用 \t 进行分隔
    row_data <- str_split(line, "\t")[[1]]
    
    # 动态填充缺失字段
    row_data <- c(row_data, rep(NA, 15 - length(row_data)))
    
    # 构建数据框
    temp_df <- data.frame(
      Sequence = current_sequence,                # 序列名称
      Start = as.numeric(row_data[1]),            # 起始位置
      End = as.numeric(row_data[2]),              # 结束位置
      Period_size = as.numeric(row_data[3]),      # 周期大小
      Copy_number = as.numeric(row_data[4]),      # 重复次数
      Consensus_size = as.numeric(row_data[5]),   # 共识序列大小
      Percent_Matches = as.numeric(row_data[6]),  # 匹配百分比
      Percent_Indels = as.numeric(row_data[7]),   # 插入/缺失百分比
      Score = as.numeric(row_data[8]),            # 分数
      A = as.numeric(row_data[9]),                # A碱基含量
      C = as.numeric(row_data[10]),               # C碱基含量
      G = as.numeric(row_data[11]),               # G碱基含量
      T = as.numeric(row_data[12]),               # T碱基含量
      Entropy = as.numeric(row_data[13]),         # 熵
      Repeat_unit = row_data[14],                 # 重复单位
      Repeat_sequence = row_data[15]              # 重复的具体序列
    )
    
    # 添加到列表
    sequence_list <- append(sequence_list, list(temp_df))
  }
}

# 合并所有数据框
final_df <- bind_rows(sequence_list)

# Filteration
data <- subset(final_df, Copy_number > 50)

# Read sequences length
seq_len <- read.table(choose.files(), header = FALSE, sep = "\t")
colnames(seq_len) <- c("Sequence", "Length", "xxxx", "xxx","xx")


final_df <- final_df %>%
  left_join(seq_len, by = "Sequence")




# 加载 ggplot2
library(ggplot2)
pak::pkg_install("ggrepel")
library(ggrepel)
library(dplyr)

# 绘制特定Sequence上的分布
plot_sequence <- "dmel_3_3R_5_1_77241" # 替换为你想绘制的Sequence名称

# 筛选特定的Sequence数据
plot_data <- final_df %>%
  filter(Sequence == plot_sequence)

# 获取该Sequence的总长度
sequence_length <- seq_len %>%
  filter(Sequence == plot_sequence) %>%
  pull(Length)

# 绘制分布图
# 只显示Repeat_unit长度小于等于20的标签
plot_data <- plot_data %>%
  mutate(Show_Label = ifelse(nchar(Repeat_unit) <= 20, Repeat_unit, NA))  # 只有Repeat_unit长度<=20时显示标签

# 绘制分布图，并在标签上使用 ggrepel 来避免重叠
# 绘制分布图，并在标签上使用 ggrepel 来避免重叠
# 去除重复的Repeat_unit，只保留一次
plot_data_unique <- plot_data %>%
  distinct(Repeat_unit, .keep_all = TRUE)  # 保留唯一的Repeat_unit
# 绘制分布图，并在标签上使用 ggrepel 来避免重叠
ggplot(plot_data, aes(x = Start, xend = End, y = Sequence, yend = Sequence)) +
  geom_segment(color = "blue", size = 1) +  # 绘制重复区域的蓝色线段
  geom_text_repel(data = plot_data_unique,  # 仅使用唯一的Repeat_unit进行标注
                  aes(x = (Start + End) / 2, y = Sequence, label = Show_Label),
                  size = 3, 
                  box.padding = 0.5,        # 标签的边框填充
                  point.padding = 0.5,      # 点到标签之间的距离
                  segment.color = "gray",   # 标签连接线颜色
                  segment.size = 0.2,       # 标签连接线粗细
                  max.overlaps = 10,        # 控制标签最大重叠数量
                  nudge_y = 0.5,            # 标签的垂直偏移量，避免重叠
                  na.rm = TRUE,             # 移除没有标签的数据
                  force = 1) +              # 控制标签的推动力度，避免重叠
  scale_x_continuous(limits = c(0, sequence_length), expand = c(0, 0)) +
  labs(
    title = paste("Tandem Repeat Distribution on", plot_sequence),
    x = "Position on Sequence",
    y = "Sequence"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))  # 可选，调整y轴文本的大小



# 绘制所有Sequence的分布
ggplot(final_df, aes(x = Start, xend = End, y = Sequence, yend = Sequence)) +
  geom_segment(color = "blue", size = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title = "Tandem Repeat Distribution on Multiple Sequences",
    x = "Position on Sequence",
    y = "Sequence"
  ) +
  facet_wrap(~ Sequence, scales = "free_y") + # 每个Sequence独立显示
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))
