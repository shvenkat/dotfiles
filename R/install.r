suppressMessages(require("devtools")) || install.packages("devtools")
suppressMessages(require("ggplot2"))  || install.packages("ggplot2")
suppressMessages(require("plyr"))     || install.packages("plyr")
if (!suppressMessages(require("foo"))) {
    tryCatch(install.packages("foo"), warning = function(w) stopifnot(FALSE))
}
# install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 'uuid', 'digest'))
# devtools::install_github('IRkernel/IRkernel')
