# Install pak
install.packages("pak")
# Install roxygen2 and devtools via pak
pak::pkg_install("roxygen2")
# Install brio for devtools
pak::pkg_install("brio")
# Install devtools
pak::pkg_install("devtools")
# Install data.table
pak::pkg_install("data.table")
pak::pkg_install("GenomicRanges")
pak::pkg_install("stringr")
pak::pkg_install("dplyr")
pak::pkg_install("gridExtra")
pak::pkg_install("rtracklayer")
pak::pkg_install("Gviz")
pak::pkg_install("IRanges")
pak::pkg_install("S4Vectors")
pak::pkg_install("Biostrings")
# Build document
devtools::document()
devtools::load_all()
