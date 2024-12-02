#' Plot the coverage of a selected contig
#'
#' @param data alignment matrix
#' @param chr selected contig
#' @param mapping_type selected type, 'P' or 'S'
#' @param target_region selected target region
#' @import ggplot2
#' @importFrom dplyr %>% group_by mutate ungroup row_number
#' 
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' # Example usage:
#' paf_selected_coverage(ali_selected, chr="chr2", mapping_type="P")
paf_selected_coverage <- function(data, chr="Contig119", mapping_type="P", target_region=NULL){
  alignment_matrix <- subset(data, tname == chr & tp == mapping_type)
  alignment_matrix$index <- seq(1, nrow(alignment_matrix))
  contig_length <- unique(alignment_matrix$tlength)
  
  if (length(contig_length) != 1) {
    stop("[WARNING] This contig has different lengths in the alignment matrix, please check the data!")
  }
  alignment_matrix <- alignment_matrix %>%
    group_by(qname) %>%
    mutate(duplicate_count = row_number()) %>%
    ungroup()
  
  # 自定义target区域，如果没有定义则根据数据的tstart和tend决定范围
  if (!is.null(target_region)) {
    target_start <- 0
    target_end <- contig_length
  } else {
    target_start <- min(alignment_matrix$tstart)
    target_end <- max(alignment_matrix$tend)
  }
  
  
  plot <- ggplot(alignment_matrix) +
    # 使用 geom_segment 来表示每个 read 的位置 (Contig119)
    geom_segment(aes(x = tstart, xend = tend, y = index, yend = index, color = mapq), linewidth = 2) +
    
    # 标注每个 reads 的 qstart-qend 和 tstart-tend
    geom_text(aes(x = (tstart + tend) / 2, y = index + 0.5, 
                  label = paste0("q: ", qstart, "-", qend, "/",qlength,"; t: ", tstart, "-", tend, "; nmatch: ", nmatch, "; alen: ", alen)), 
              size = 3, hjust = 0.5) +
    # 标记相同 qname 的 reads，用不同形状表示
    geom_point(aes(x = tstart, y = index, shape = factor(duplicate_count)), size = 3) +
    
    # 设置颜色渐变显示 mapq 的不同值
    scale_color_gradientn(colors = c("blue", "red")) +
    # 设置横轴范围为自定义target区域
    xlim(target_start, target_end) +
    
    # 调整坐标轴和标签
    labs(x = paste0(chr, " Position"), y = "Read Index", color = "Mapping Quality (mapq)") +
    
    # 美化图形
    theme_minimal() +
    theme(axis.text.y = element_blank(),  # 隐藏 y 轴标签
          axis.ticks.y = element_blank()) # 隐藏 y 轴刻度
  return(plot)
  
}