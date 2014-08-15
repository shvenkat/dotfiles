options(prompt = "R> ", digits = 4, show.signif.stars = FALSE, max.print = 99,
        digits.secs = 3, menu.graphics = FALSE)
options(width=76)
options(browserNLdisabled = TRUE, deparse.max.lines = 4)
# options("pdfviewer"="kpdf")
# ps.options( horizontal=FALSE )
# options('tikzLatex'='/home/shvenkat/texlive/2010/bin/x86_64-linux/pdflatex')
# options('tikzXelatex'='/home/shvenkat/texlive/2010/bin/x86_64-linux/xelatex')
# options('tikzMetricsDictionary' = '/home/shvenkat/R/tikzMetricsDictionary')

local({
    repo <- getOption("repos")
    repo["CRAN"] <- "http://cran.cnr.berkeley.edu/"
    options("repos" = repo)
})

# setHook(packageEvent("grDevices", "onLoad"),
#     function(...) grDevices::X11.options(width=8, height=8,
#                                          xpos=0, pointsize=10,
#                                          #type="nbcairo"))  # Cairo device
#                                          #type="cairo"))    # other Cairo dev
#                                          type="xlib"))      # old default
# setHook(packageEvent("grDevices", "onLoad"),
#     function(...) grDevices::ps.options(horizontal=FALSE))

if(interactive()){
    options(setwidth.verbose = 0,
            colorout.verbose = 0,
            vimcom.verbose = 0)
    suppressMessages(require(colorout))
    suppressMessages(require(setwidth))
    if(Sys.getenv("VIMRPLUGIN_TMPDIR") != "") {
        suppressMessages(require(vimcom))
    }
    if(grepl("^CYGWIN_", Sys.info()["sysname"])) {
        Sys.setenv("LANG" = "en_US.CP1252")
        Sys.setlocale("LC_MESSAGES", "en_US.CP1252")
        Sys.setlocale("LC_ALL",      "en_US.CP1252")
        # Sys.setlocale("LC_CTYPE",    "en_US.CP1252")
    }
    invisible(NULL)
}

# .First <- function() {
#     # final session config
# }
# .Last <- function() {
#     # session cleanup
# }
