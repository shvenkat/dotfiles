#!/usr/bin/env Rscript

install.packages("devtools")
library(devtools)

install.packages("setwidth")
download.file("http://www.lepem.ufc.br/jaa/colorout_1.0-3.tar.gz",
              destfile = "/tmp/colorout_1.0-3.tar.gz")
install.packages("/tmp/colorout_1.0-3.tar.gz", type = "source", repos = NULL)
system("rm -f /tmp/colorout_1.0-3.tar.gz")

install_github('jalvesaq/VimCom')

install.packages(c("ggplot2", "roxygen2", "gplots", "kinship2"))
