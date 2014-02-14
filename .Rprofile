#.First <- function() cat("\n   Welcome to R!\n\n")
#.Last <- function()  cat("\n   Goodbye!\n\n")

biocLite.url <- "http://bioconductor.org/biocLite.R"

options( max.print=999, digits=4, show.signif.stars=FALSE )
#ps.options( horizontal=FALSE )
options( menu.graphics=FALSE )
options('tikzLatex'='/home/shvenkat/texlive/2010/bin/x86_64-linux/pdflatex')
options('tikzXelatex'='/home/shvenkat/texlive/2010/bin/x86_64-linux/xelatex')
options('tikzMetricsDictionary' = '/home/shvenkat/R/tikzMetricsDictionary')
options(width=76)

if(interactive()){
    options(setwidth.verbose = 0,
            colorout.verbose = 0,
            vimcom.verbose = 0)
    library(colorout)
    library(setwidth)
    if(Sys.getenv("VIMRPLUGIN_TMPDIR") != "") {
        library(vimcom.plus)
    }
}
