library(hjRplot)
# Read paf file generated from minimap2
ali <- read.paf(choose.files())
# Filter the alignment with mapping quality > 30


ali_selected <- subset(ali, tname == "chrX_mel")
ali_selected <- ali
ali_selected <- subset(ali_selected, mapq >= 30)
ali_selected <- subset(ali_selected, tp == "P")
#ali_selected$percentage <- ali_selected$nmatch / ali_selected$alen
#selected_reads <- c("39fec261-134e-4b8a-b1f2-adad54f8f7ab","c8a109ee-ab3f-4584-abad-76cb30f050e4","00feb67d-a366-4cc7-babf-16cd19871e8d","c8a109ee-ab3f-4584-abad-76cb30f050e4;00352179-85eb-459a-9d25-b56756a10f75","87195fb4-04ef-4ac2-9bcd-06ae724aaf64")
#ali_selected <- subset(ali_selected, qname %in% selected_reads)

ali_selected <- subset(ali_selected, alen >= 1000)

ali_selected_filter <- subset(ali_selected, percentage >= 0.8)


ali_selected <- subset(ali_selected, tend >= 27920-1000 & tstart <= 118193+1000)
paf_selected_coverage(ali_selected, chr="chrX_mel", mapping_type="P")

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
