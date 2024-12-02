library(hjRplot)
# Read paf file generated from minimap2
ali <- read.paf(choose.files())
# Filter the alignment with mapping quality > 30
# ali <- ali[ali$mapq > 30,]
ali_selected <- subset(ali, tname == "seed_2_001")
ali_selected <- subset(ali_selected, mapq >= 50)
ali_selected$percentage <- ali_selected$nmatch / ali_selected$alen
selected_reads <- c("e0bb0da1-8313-4a0c-8c68-65e9c1d14321;65e15d69-fa5f-4a68-be41-115563e4e9af","f180bd40-dc54-4cff-be60-10cbb5b35d06")
ali_selected <- subset(ali_selected, qname %in% selected_reads)

ali_selected <- subset(ali_selected, nmatch >= 1000)

ali_selected_filter <- subset(ali_selected, percentage >= 0.8)

ali_selected <- subset(ali_selected, tend >= 50000 & tstart <= 68785+1000)
paf_selected_coverage(ali_selected, chr="Seed_2_01", mapping_type="P")

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
