local({
    repo <- getOption("repos")
    repo["CRAN"] <- "http://cran.cnr.berkeley.edu/"
    options("repos" = repo)
})
options(prompt = "R> ", digits = 4, show.signif.stars = FALSE, max.print = 99,
        digits.secs = 3, menu.graphics = FALSE)
options(width=76)
options(browserNLdisabled = TRUE, deparse.max.lines = 4)

if(interactive()){
    options(setwidth.verbose = 0,
            colorout.verbose = 0,
            vimcom.verbose = 0)
    suppressMessages(require(colorout))
    suppressMessages(require(setwidth))
    if(Sys.getenv("VIMRPLUGIN_TMPDIR") != "") {
        suppressMessages(require(vimcom))
    }
    invisible(NULL)
}

if(grepl("^CYGWIN_", Sys.info()["sysname"])) {
    Sys.setenv("LANG" = "en_US.CP1252")
    Sys.setlocale("LC_MESSAGES", "en_US.CP1252")
    Sys.setlocale("LC_ALL",      "en_US.CP1252")
    # Sys.setlocale("LC_CTYPE",    "en_US.CP1252")
}

# .First <- function() {
#     # final session config
# }
# .Last <- function() {
#     # session cleanup
# }

# setHook(packageEvent("grDevices", "onLoad"),
#     function(...) grDevices::X11.options(width=8, height=8,
#                                          xpos=0, pointsize=10,
#                                          #type="nbcairo"))  # Cairo device
#                                          #type="cairo"))    # other Cairo dev
#                                          type="xlib"))      # old default
# setHook(packageEvent("grDevices", "onLoad"),
#     function(...) grDevices::ps.options(horizontal=FALSE))
