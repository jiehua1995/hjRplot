library(hjRplot)
# Read paf file generated from minimap2
ali <- read.paf(choose.files())
# Filter the alignment with mapping quality > 30
unique(ali$tname)

ali_selected <- data.frame(ali)

dm6_chromosomes <- c("2L", "2R", "3L", "3R", "4", "X","Y")
ali_selected <- subset(ali_selected, tname %in% dm6_chromosomes)
s8_centro <- c("tig00057289","3R_5","Contig119","Contig79")
ali_selected <- subset(ali, tname %in% s8_centro)

ali_selected_clean <- ali_selected

list_cols <- sapply(ali_selected, is.list)  # 找出 List 类型的列
ali_selected_clean[list_cols] <- lapply(ali_selected_clean[list_cols], function(x) sapply(x, toString))
write.csv(ali_selected_clean, file = choose.files(), row.names = FALSE)

ali_selected <- subset(ali_selected, nmatch >= 1000)
ali_selected <- subset(ali_selected, alen >= 1000)
ali_selected <- subset(ali_selected, mapq >= 40)
ali_selected$percentage <- ali_selected$nmatch / ali_selected$alen


