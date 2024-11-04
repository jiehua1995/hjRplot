# Read paf file generated from minimap2
ali <- read.paf(choose.files())
ali <- read.paf("C:\\Users\\ra35yun\\Downloads\\temp\\temp\\tig00057289_selected.paf")
ali_s8 <- read.paf("C:\\Users\\ra35yun\\Downloads\\temp\\s8temp\\S8_chopped_q10_10kb_S8genome.paf")
# Filter the alignment with mapping quality > 30
# ali <- ali[ali$mapq > 30,]
ali_selected <- subset(ali, tname == "Seed_X")
ali_selected <- subset(ali_selected, mapq >= 50)
ali_selected$qstart_per <- ali_selected$qstart / ali_selected$qlength * 100
ali_selected$qend_per <- ali_selected$qend / ali_selected$qlength * 100

ali_selected$tstart_per <- ali_selected$tstart / ali_selected$tlength * 100
ali_selected$tend_per <- ali_selected$tend / ali_selected$tlength *100

ali_selected$q_percentage <- (ali_selected$qend - ali_selected$qstart) / ali_selected$qlength *100
ali_selected$t_percentage <- (ali_selected$tend - ali_selected$tstart) / ali_selected$tlength *100
selected_reads <- c("dc83cefd-0d3d-459d-90df-b1dac991857b")
ali_selected <- subset(ali_selected, qname %in% selected_reads)

ali_selected <- subset(ali_selected, tend >= 50000 & tstart <= 68785+1000)
paf_selected_coverage(ali_selected, chr="Seed_X", mapping_type="P")

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
