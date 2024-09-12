# Read paf file generated from minimap2
ali <- read.paf(choose.files())
# Filter the alignment with mapping quality > 30
ali <- ali[ali$mapq > 30,]
#
bam <- "C:\\Users\\ra35yun\\Downloads\\temp\\07b_barcode21.bam"
gtf <- "C:\\Users\\ra35yun\\Downloads\\temp\\dmel-all-r6.56_no_comments.gtf"
region_start <- 13484000
region_end <- 13487000
chromosome <- "2R"
plot_genomic_coverage_and_annotations(bamfile, gtf_file, chromosome, visible_start, visible_end,use_smooth = FALSE, use_bin_coverage = FALSE, bin_size = 500)

plot_genomic_coverage_and_annotations(bam, gtf, chromosome, start, end, by = 500, smooth = FALSE, smooth_color="red", bin_coverage = FALSE, bin_size = 500, coverage_color="blue", annotation_color="yellow")