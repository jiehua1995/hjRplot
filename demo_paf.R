# Read paf file generated from minimap2
ali <- read.paf(choose.files())
ali <- read.paf("C:\\Users\\ra35yun\\Downloads\\temp\\Ras3_S8.paf")
# Filter the alignment with mapping quality > 30
ali <- ali[ali$mapq > 30,]



# For genomic_coverage
bam <- "C:\\Users\\ra35yun\\Downloads\\temp\\07b_barcode21.bam"
gtf <- "C:\\Users\\ra35yun\\Downloads\\temp\\dmel-all-r6.56_no_comments.gtf"
region_start <- 13484000
region_end <- 13487000
chromosome <- "2R"

plot_genomic_coverage_and_annotations(bam, gtf, chromosome, region_start, region_end, by = 500, smooth = TRUE, smooth_color="red", bin_coverage = TRUE, bin_size = 100, coverage_color="blue", annotation_color="yellow")
