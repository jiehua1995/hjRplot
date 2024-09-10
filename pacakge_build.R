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
pak::pkg_install("stringr")
pak::pkg_install("dplyr")
# Build document
devtools::document()
devtools::load_all()
