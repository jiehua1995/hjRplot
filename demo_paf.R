library(hjRplot)
# Read paf file generated from minimap2
ali <- read.paf(choose.files())
# Filter the alignment with mapping quality > 30
unique(ali$tname)

ali_selected <- data.frame(ali)
ali_selected <- subset(ali_selected, mapq >= 40)
ali_selected <- subset(ali_selected, tp == "P")




dm6_chromosomes <- c("2L", "2R", "3L", "3R", "4", "X","Y")
ali_selected <- subset(ali_selected, tname %in% dm6_chromosomes)


ali_selected <- subset(ali_selected, nmatch >= 10000)
ali_selected <- subset(ali_selected, alen >= 10000)


ali_selected$percentage <- ali_selected$nmatch / ali_selected$alen
ali_selected <- as.data.frame(ali_selected)

# 将 List 类型的列转换为字符
ali_selected_clean <- ali_selected
list_cols <- sapply(ali_selected, is.list)  # 找出 List 类型的列
ali_selected_clean[list_cols] <- lapply(ali_selected_clean[list_cols], function(x) sapply(x, toString))

# 写入 CSV 文件
write.csv(ali_selected_clean, file = "ali_selected.csv", row.names = FALSE)

#selected_reads <- c("39fec261-134e-4b8a-b1f2-adad54f8f7ab","c8a109ee-ab3f-4584-abad-76cb30f050e4","00feb67d-a366-4cc7-babf-16cd19871e8d","c8a109ee-ab3f-4584-abad-76cb30f050e4;00352179-85eb-459a-9d25-b56756a10f75","87195fb4-04ef-4ac2-9bcd-06ae724aaf64")
#ali_selected <- subset(ali_selected, qname %in% selected_reads)

ali_selected <- subset(ali_selected, alen >= 1000)
ali_selected <- subset(ali_selected, nmatch >= 1000)

ali_selected_filter <- subset(ali_selected, percentage >= 0.8)

write.csv(ali_selected, choose.files(), row.names = FALSE)

ali_selected <- subset(ali_selected, tend >= 27920-1000 & tstart <= 118193+1000)
paf_selected_coverage(ali_selected, chr="3L", mapping_type="P")

# Genomic fasta NNN finding
fasta <- "C:\\Users\\ra35yun\\Downloads\\temp\\temp\\Ras3_renamed_sorted.fasta"
chromosomes_to_keep <- c("chrX", "chr2L", "chr2R", "chr3L", "chr3R", "chr4")
genome_gap_finding()

# For genomic_coverage
bam <- "C:\\temp20240826\\ras3_centromere_sorted.bam"
gtf <- "C:\\temp20240826\\ras3_centromere.paf"
region_start <- 0
region_end <- 70181
chromosome <- "Contig79"

plot_genomic_coverage_and_annotations(bam, gtf, chromosome, region_start, region_end, by = 500, smooth = TRUE, smooth_color="red", bin_coverage = TRUE, bin_size = 100, coverage_color="blue", annotation_color="yellow")






### Qname as chr
selected_chromosome <- "2L"
df_filtered <- subset(ali_selected, qname == selected_chromosome)


# 2. 绘制可视化
p <- ggplot(df_filtered, aes(x=qstart, xend=qend, y=tname, yend=tname)) +
  geom_segment(aes(color=tname), size=1) +  # 连线
  geom_point(aes(x=qstart, y=tname), color="blue", size=2) +  # 起点
  geom_point(aes(x=qend, y=tname), color="red", size=2) +    # 终点
  scale_x_continuous("染色体坐标", expand = c(0, 0)) +
  scale_y_discrete("tname") +
  labs(title=paste("染色体", selected_chromosome, "与 tname 的对应关系")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=45, hjust=1)
  )

# 3. 显示图
print(p)
