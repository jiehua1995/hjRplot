vcf_mutation_ratio <- function(vcffile){
  
# Read vcf file to a dataframe
vcf <- read.table(vcffile, header = FALSE, sep = "\t", comment.char = "#", stringsAsFactors = FALSE, na.strings = "NA")
colnames(vcf) <- c("CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", "SAMPLE")
vcf_high <- subset(vcf, QUAL >0)
vcf_high_AtoG <- subset(vcf_high, REF == "A" & ALT == "G")
vcf_high_Atoother <- subset(vcf_high, REF == "A" & ALT != "G")
vcf_high_TtoC <- subset(vcf_high, REF == "T" & ALT == "C")
vcf_high_Ttoother <- subset(vcf_high, REF == "T" & ALT != "C")

dmel6_56_total_a <- 41351058
dmel6_56_total_t <- 41332481

AtoG_ratio <- nrow(vcf_high_AtoG) / dmel6_56_total_a
TtoC_ratio <- nrow(vcf_high_TtoC) / dmel6_56_total_t

data <- data.frame(REF = c("A", "T"), ALT = c("G", "C"), Ratio = c(AtoG_ratio, TtoC_ratio))

return(data)

}

# For 4-UV
treated <- vcf_mutation_ratio(choose.files())
untreated <- vcf_mutation_ratio(choose.files())
data <- merge(treated, untreated, by = c("REF", "ALT"))
