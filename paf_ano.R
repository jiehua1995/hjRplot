library(ggplot2)
library(dplyr)

# 读取数据
# df <- read.table("data.txt", header=TRUE, sep="\t")

# 选择需要可视化的染色体
selected_chromosomes <- c("2L", "2R", "3L", "3R", "4", "X")
selected_chromosomes <- c("2L")
df_filtered <- subset(ali_selected, qname %in% selected_chromosomes)

# 计算染色体范围
chromosomes <- df_filtered %>%
  select(qname, qlength) %>%
  distinct() %>%
  rename(name = qname, length = qlength) %>%
  mutate(type = "query")

targets <- df_filtered %>%
  select(tname, tlength) %>%
  distinct() %>%
  rename(name = tname, length = tlength) %>%
  mutate(type = "target")

chrom_data <- bind_rows(chromosomes, targets)

# 绘制图形
p <- ggplot() +
  
  # 1. 画出 query 和 target 染色体
  geom_segment(data=chrom_data, aes(x=0, xend=length, y=name, yend=name, color=type), size=2) +
  
  # 2. 连接 qstart → tstart 和 qend → tend
  geom_segment(data=df_filtered, 
               aes(x=qstart, xend=tstart, y=qname, yend=tname), 
               color="blue", alpha=0.5, size=0.8) +
  geom_segment(data=df_filtered, 
               aes(x=qend, xend=tend, y=qname, yend=tname), 
               color="red", alpha=0.5, size=0.8) +
  
  # 设置坐标轴
  scale_x_continuous("染色体坐标", expand=expansion(mult=c(0.05, 0.05))) +
  scale_y_discrete("染色体 & tname",expand=expansion(mult=c(0.05, 0.05))) +
  
  labs(title="染色体之间的对应关系") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    legend.position = "top"
  )

# 显示图
print(p)




p <- ggplot() +
  
  # 1. 画出 query 和 target 染色体
  geom_segment(data=chrom_data, aes(x=0, xend=length, y=name, yend=name, color=type), size=2) +
  
  # 2. 连接 qstart → tstart 和 qend → tend
  geom_segment(data=df_filtered, 
               aes(x=qstart, xend=tstart, y=qname, yend=tname), 
               color="blue", alpha=0.5, size=0.8) +
  geom_segment(data=df_filtered, 
               aes(x=qend, xend=tend, y=qname, yend=tname), 
               color="red", alpha=0.5, size=0.8) +
  
  # 3. 在 tname 染色体上标记 qstart、qend 和 tstart、tend 数值
  geom_point(data=df_filtered, aes(x=tstart, y=tname), color="blue", size=2) +
  geom_text(data=df_filtered, aes(x=tstart, y=tname, label=tstart), 
            color="blue", vjust=-1, size=3) +
  
  geom_point(data=df_filtered, aes(x=tend, y=tname), color="red", size=2) +
  geom_text(data=df_filtered, aes(x=tend, y=tname, label=tend), 
            color="red", vjust=-1, size=3) +
  
  geom_point(data=df_filtered, aes(x=qstart, y=tname), color="purple", size=2) +
  geom_text(data=df_filtered, aes(x=qstart, y=tname, label=qstart), 
            color="purple", vjust=1, size=3) +
  
  geom_point(data=df_filtered, aes(x=qend, y=tname), color="orange", size=2) +
  geom_text(data=df_filtered, aes(x=qend, y=tname, label=qend), 
            color="orange", vjust=1, size=3) +
  
  # 修改：允许 X 和 Y 轴 expand
  scale_x_continuous("染色体坐标", expand=expansion(mult=c(0.05, 0.05))) +
  scale_y_discrete("染色体 & tname", expand=expansion(mult=c(0.05, 0.05))) +
  
  labs(title="染色体之间的对应关系") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    legend.position = "top"
  )

# 显示图
print(p)

