#' Plot Genomic Coverage and Gene Annotations
#'
#' This function plots genomic coverage from a BAM file and gene annotations from a GTF file.
#'
#' @param bam The path to the BAM file.
#' @param gtf The path to the GTF file.
#' @param chromosome The chromosome or contig name (e.g., "2R").
#' @param start The start position of the genomic region.
#' @param end The end position of the genomic region.
#' @param break The break size for the coverage plot.
#' @param smooth Logical; if TRUE, applies a smooth line to the coverage plot.
#' @param smooth_color The color of the smooth line. Default is 'red'.
#' @param bin_coverage Logical; if TRUE, calculates average coverage per bin.
#' @param bin_size Numeric; bin size for averaging coverage, applicable if use_bin_coverage is TRUE.
#' @param coverage_color The color of the coverage plot. Default is 'blue'.
#' @param annotation_color The color of the gene annotations. Default is 'yellow'.
#'
#' @return A combined ggplot object showing the coverage plot and gene annotations.
#' @import ggplot2
#' @import Rsamtools
#' @import GenomicRanges
#' @import rtracklayer
#' @import gridExtra
#' @import dplyr
#' @import cowplot
#' @import IRanges
#' @import S4Vectors
#' @export
#'
#' @examples
#' # Example usage:
#' plot_genomic_coverage_and_annotations(
#'   bamfile = "path/to/bamfile.bam",
#'   gtf_file = "path/to/gtf_file.gtf",
#'   chromosome = "2R",
#'   visible_start = 13484000,
#'   visible_end = 13487000,
#'   use_smooth = FALSE,
#'   use_bin_coverage = FALSE,
#'   bin_size = 500
#' )
plot_genomic_coverage_and_annotations <- function(bam, gtf, chromosome, region_start, region_end, by = 500, smooth = FALSE, smooth_color="red", bin_coverage = FALSE, bin_size = 500, coverage_color="blue", annotation_color="yellow") {
  
  # 1. Coverage plot
  # Define the genomic region
  region <- GRanges(chromosome, IRanges(start = start, end = end))
  # Define the pileup parameters
  pileup_param <- PileupParam(distinguish_nucleotides = FALSE, distinguish_strands = FALSE)
  # Read the BAM file
  cat("Reading BAM file...\n")
  bam_coverage <- pileup(bamfile, scanBamParam = ScanBamParam(which = region), pileupParam = pileup_param)
  # Save the data into a coverage data frame
  coverage_df <- bam_coverage
  # Should we use smooth line>
  if (bin_coverage) {
    coverage_df <- coverage_df %>%
      mutate(bin = floor(pos / bin_size) * bin_size) %>%
      group_by(bin) %>%
      summarise(avg_count = mean(count)) %>%
      rename(pos = bin, count = avg_count)
    p1 <- ggplot(coverage_df, aes(x = pos, y = count)) +
      geom_step(direction = "hv", color = coverage_color, size = 1) +  # 使用步状线图
      labs(title = "Coverage Plot", x = "", y = "Average Coverage per Bin") +
      theme_minimal()
  } else {
    p1 <- ggplot(coverage_df, aes(x = pos, y = count)) +
      geom_line(color = coverage_color) +
      labs(title = "Coverage Plot", x = "", y = "Coverage") +
      theme_minimal()
  }

  # 如果选择平滑曲线，使用 geom_smooth
  if (smooth) {
    p1 <- p1 + geom_smooth(method = "loess", color = "red", se = FALSE)  # 添加平滑曲线
  }
  
  
  # 2. Gene annotations
  library(rtracklayer)
  gtf_data <- import(gtf, format = "gtf")
  gtf_mcols <- mcols(gtf_data)
  genes_in_region <- subset(gtf_data, seqnames(gtf_data) == chromosome & start(gtf_data) <= region_end & end(gtf_data) >= region_start & 
                              gtf_mcols$type == "gene")
  
  genes_df <- as.data.frame(genes_in_region)
  genes_gr <- GRanges(seqnames = genes_df$seqnames, ranges = IRanges(start = genes_df$start, end = genes_df$end))
  overlaps <- findOverlaps(genes_gr)
  genes_df$y_position <- rep(1, nrow(genes_df)) 
  
  # 查找重叠的基因并为其分配不同的Y轴层级
  for(i in seq_along(genes_gr)) {
    overlaps_i <- queryHits(overlaps)[queryHits(overlaps) == i]
    if(length(overlaps_i) > 0) {
      genes_df$y_position[overlaps_i] <- max(genes_df$y_position) + 1
    }
  }
  unique_genes <- genes_df %>%
    select(y_position, gene_id) %>%
    distinct()  # 确保每个基因ID和它的y_position是唯一的
  # 调整注释位置，如果基因符号超出可视范围
  genes_df <- genes_df %>%
    mutate(
      adjusted_label_pos = ifelse((start + end) / 2 < visible_start, visible_start + 100, 
                                  ifelse((start + end) / 2 > visible_end, visible_end - 100, (start + end) / 2))
    )
  
  
  # Step 4: 确保 Y 轴范围足够大以显示所有基因和基因符号
  max_y_position <- max(genes_df$y_position, na.rm = TRUE)
  p2 <- ggplot(genes_df) +
    geom_segment(aes(x = start, xend = end, y = y_position, yend = y_position), linewidth = 2, color = annotation_color ) +
    geom_text(aes(x = adjusted_label_pos, y = y_position, label = gene_symbol), size = 3, vjust = -0.5) +  # 调整后的标签位置
    labs(x = paste0("Genomic Position (",chromosome," :",visible_start,"-",visible_end,")"), y = "Gene") +  # 移除 Y 轴标签
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),  # 移除 Y 轴刻度
      axis.ticks.y = element_blank(),  # 移除 Y 轴刻度线
    ) +
    scale_y_continuous(limits = c(0, max_y_position + 0.5), expand = c(0, 0)) +
    coord_cartesian(xlim = c(visible_start, visible_end))  # 用 coord_cartesian 调整显示范围
  
  
  # Merge the plot 
  x_limits <- c(start, end)  # 确保范围与你定义的区域一致
  p1 <- p1 + scale_x_continuous(breaks = seq(start, end, by = by))  # 保持 X 轴刻度一致
  p2 <- p2 + scale_x_continuous(breaks = seq(start, end, by = by))  # 只设置刻度，不裁剪
  
  # Step 6: 合并覆盖度图和基因注释图
  combined_plot <- plot_grid(p1, p2, ncol = 1, align = "v", axis = "lr")
  return(combined_plot)
  
}
