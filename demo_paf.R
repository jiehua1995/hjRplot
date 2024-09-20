# Read paf file generated from minimap2
ali <- read.paf(choose.files())
ali <- read.paf("C:\\Users\\ra35yun\\Downloads\\temp\\temp\\tig00057289_selected.paf")
ali_s8 <- read.paf("C:\\Users\\ra35yun\\Downloads\\temp\\s8temp\\S8_chopped_q10_10kb_S8genome.paf")
# Filter the alignment with mapping quality > 30
ali <- ali[ali$mapq > 30,]
ali_selected <- subset(ali, tname == "tig00057289_selected")
ali_selected <- subset(ali_selected, mapq >= 30)
selected_reads <- c("03de177e-a051-44dd-a9b2-312b1e7bdefe","c8a109ee-ab3f-4584-abad-76cb30f050e4")
ali_selected <- subset(ali_selected, qname %in% selected_reads)


ali_selected <- subset(ali_selected, tend >= 23650026-10000 & tstart <= 23675388+10000)
paf_selected_coverage(ali_selected, chr="tig00057289_selected", mapping_type="P")

# Genomic fasta NNN finding
fasta <- "C:\\Users\\ra35yun\\Downloads\\temp\\temp\\Ras3_renamed_sorted.fasta"
chromosomes_to_keep <- c("chrX", "chr2L", "chr2R", "chr3L", "chr3R", "chr4")
genome_gap_finding()

# For genomic_coverage
bam <- "C:\\Users\\ra35yun\\Downloads\\temp\\07b_barcode21.bam"
gtf <- "C:\\Users\\ra35yun\\Downloads\\temp\\dmel-all-r6.56_no_comments.gtf"
region_start <- 13484000
region_end <- 13487000
chromosome <- "2R"

plot_genomic_coverage_and_annotations(bam, gtf, chromosome, region_start, region_end, by = 500, smooth = TRUE, smooth_color="red", bin_coverage = TRUE, bin_size = 100, coverage_color="blue", annotation_color="yellow")
