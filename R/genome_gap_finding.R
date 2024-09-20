#' Genome gap finding
#'
#' @param fasta Fasta file conting genome sequence
#' @param chromosomes Selected chromosomes
#' @param min_gap Minimum gap size
#' @param gap_symbol Gap symbol, default is 'N'
#' @importFrom Biostrings readDNAStringSet
#'
#' @return A plot showing the gap regions
#' @export
#'
#' @examples
#' plot <- genome_gap_finding("genome.fasta", c("2", "3"), min_gap=50, gap_symbol="N")
genome_gap_finding <- function(fasta, chromosomes, min_gap=50, gap_symbol="N"){
  # 读取FASTA文件
  genome <- Biostrings::readDNAStringSet(fasta)
  
  # 创建一个空的数据框来存储gap区域
  gap_regions_df <- data.frame(Chromosome = character(), Start = integer(), End = integer(), stringsAsFactors = FALSE)
  
  # 遍历FASTA中的染色体，筛选出用户感兴趣的染色体
  for (chromosome in names(genome)) {
    if (chromosome %in% chromosomes) {
      # 获取该染色体的DNA序列
      seq <- genome[[chromosome]]
      seq_char <- as.character(seq)
      
      # 用rle函数找到连续的相同符号，找到gap_symbol的区域
      rle_seq <- rle(seq_char == gap_symbol)
      
      # 计算每段的起始和结束位置
      gap_starts <- cumsum(c(1, rle_seq$lengths))[rle_seq$values & rle_seq$lengths >= min_gap]
      gap_ends <- gap_starts + rle_seq$lengths[rle_seq$values & rle_seq$lengths >= min_gap] - 1
      
      # 将gap区域的起始和结束位置存储在数据框中
      if (length(gap_starts) > 0) {
        temp_df <- data.frame(Chromosome = chromosome, Start = gap_starts, End = gap_ends)
        gap_regions_df <- rbind(gap_regions_df, temp_df)
      }
    }
  }
}
