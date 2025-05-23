library(hjRplot)
# Read paf file generated from minimap2
ali <- read.paf(choose.files())
# Filter the alignment with mapping quality > 30
unique(ali$tname)

ali_selected <- data.frame(ali)

dm6_chromosomes <- c("2L", "2R", "3L", "3R", "4", "X","Y")
ali_selected <- subset(ali_selected, tname %in% dm6_chromosomes)

ali_selected_clean <- ali_selected
list_cols <- sapply(ali_selected, is.list)  # 找出 List 类型的列
ali_selected_clean[list_cols] <- lapply(ali_selected_clean[list_cols], function(x) sapply(x, toString))
write.csv(ali_selected_clean, file = choose.files(), row.names = FALSE)

ali_selected <- subset(ali_selected, nmatch >= 10000)
ali_selected <- subset(ali_selected, alen >= 10000)

# Take chromosome 2
library(dplyr)

filtered_data <- ali_selected_clean %>%
  group_by(qname) %>%
  filter(any(tname == "2L_RagTag") & any(tname == "2R_RagTag")) %>%
  ungroup()

# 查看筛选后的数据
head(filtered_data)
write.csv(filtered_data, file = "filtered_data.csv", row.names = FALSE)
