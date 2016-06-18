devtools::install_github(c(
  "gaborcsardi/progress",
  "dide-tools/context",
  "richfitz/ids",
  "richfitz/queuer",
  "dide-tools/didewin"))

library(didewin)

#test below without
didewin::didewin_config_global(credentials="C:\\Users\\Oliver\\.smbcredentials",
                               home=didewin::path_mapping("OJ","O:","//fi--san02.dide.ic.ac.uk/Homes/olw13","O:"),
                               cluster="fi--dideclusthn")

didewin::web_login()
getwd()
didewin::didewin_config()

root <- "contexts"
ctx <- context::context_save(packages=c("XML","rvest","xml2"),
                             sources="R/pull.R", root=root, storage_args=list(compress=TRUE))

root <- "contexts"

obj <- didewin::queue_didewin(ctx)

obj

t <- obj$enqueue(Pull())

