#' Read paf file into a dataframe
#'
#' @param file paf file generated from minimap2
#' @param header whether the paf file has header
#' @param tag whether the paf file has tag
#'
#' @importFrom data.table fread
#' @importFrom stringr str_split
#' @importFrom dplyr bind_rows
#' @return paf data in a dataframe
#' @export
#'
#' @examples
#' read.paf("test.paf", header = FALSE, tag = TRUE)
read.paf <- function(file, header = FALSE, tag = TRUE){
  # file=choose.files()
  # 1. Read the paf file
  paf_data <- data.table::fread(file, header = header, fill=TRUE)
  # 2. Extract the first 12 columns and name the columns
  # extract the first 12 columns
  paf_cols <- paf_data[,1:12]
  # name the columns
  #colnames(paf_cols) <- c("query_name", "query_length", "query_start", "query_end", "strand", "target_name", "target_length", "target_start", "target_end", "num_match", "block_length", "mapping_quality")
  colnames(paf_cols) <- c("qname", "qlength", "qstart", "qend", "strand", "tname", "tlength", "tstart", "tend", "nmatch", "alen", "mapq")
  # 3. Extract the tags
  tag_cols <- paf_data[, 13:ncol(paf_data), with = FALSE]
  tag_strings <- apply(tag_cols, 1, function(row) paste(row[!is.na(row)], collapse = "\t"))
  # 4. Define the tags
  parsed_tags <- lapply(tag_strings, function(tag_string) {
    # 拆分标签成键值对
    tags <- stringr::str_split(tag_string, "\t")[[1]]
    
    # 提取tag中的信息并生成命名的键值对
    tag_list <- list()
    for (tag in tags) {
      # 跳过空字符串
      if (nchar(tag) > 0) {
        key_value <- stringr::str_split(tag, ":")[[1]]
        tag_name <- key_value[1]
        tag_value <- ifelse(length(key_value) == 3, key_value[3], NA)
        tag_list[[tag_name]] <- tag_value
      }
    }
    return(tag_list)
  })
  
  # 5. 将所有标签组合成数据框格式
  tag_df <- dplyr::bind_rows(lapply(parsed_tags, function(tag) as.data.frame(t(tag), stringsAsFactors = FALSE)))
  # 6. 合并前12列与标签数据框
  final_df <- cbind(paf_cols, tag_df)
  
  return(final_df)
  
  
}