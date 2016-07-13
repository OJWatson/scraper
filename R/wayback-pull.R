#' Wayback machine scrape example for table information
#'
#' \code{Wayback_Pull} takes a list of urls from the archive pages for your url of interest (See default url.vector
#' argument for an example) and scrapes from your wanted urls a minimum css selector based table. Somewhat arbritray
#' based on table structure of this page but could easily be extended to loop over css selection for table columns etc.
#'
#'
#'
#' @param url.vector Vector of urls from wayback machine archives
#' @param css Minimal css selector for a table of information
#'
#' @importFrom rvest html_children html_nodes html_attrs html_attr html_text "%>%"
#'
#'
#' @export
#'
#' @return list of dataframes of scraped information
#'
#'

Wayback_Pull <- function(url.vector=c("https://web.archive.org/web/20150401000000*/https://www.fxcm.com/forex/forex-pricing/",
                                      "https://web.archive.org/web/20160901000000*/https://www.fxcm.com/forex/forex-pricing/"),
                         css = ".center td",
                         col.names = c("Pair","Spread")){

  ## EXTRA FUNCTIONS ##

  # function to find the urls of wayback machine urls from list of archive urls
  URL_Lists_From_Archive <- function(url.vector){

    ## pull the xml files from the urls
    xml.list <- lapply(url.vector, FUN = xml2::read_html)

    ### First start with the first webpage

    ## draw down the node set for the date captures
    node.set <- rvest::html_nodes(xml.list[[2]],".captures") %>% rvest::html_children()

    ## isolate the position in contents that will corresponds to the url
    day.node.set <- node.set[which(rvest::html_attrs(node.set)=="day")]

    ## create positions for url scrapes
    href.pos <- which(unlist(lapply(html_attrs(xml2::xml_contents(day.node.set[1])),function(x){is.element("href",names(x))})))
    pos.length <- length(xml2::xml_contents(day.node.set[1]))

    ## cycle through xml.list to generate list of urls from which to scrape
    url.list <- c()

    for(i in 1:length(xml.list)){

      ## draw down the node set for the date captures
      node.set <- rvest::html_nodes(xml.list[[i]],".captures") %>% rvest::html_children()

      ## isolate the position in contents that will corresponds to the url
      day.node.set <- node.set[which(rvest::html_attrs(node.set)=="day")]

      ## positions in contents that correspond to href
      pos.hrefs <- seq(href.pos, ((length(day.node.set)-1)*pos.length)+href.pos, pos.length)
      ## href atrribute pull
      urls <- rvest::html_attr(xml2::xml_contents(day.node.set)[pos.hrefs],"href")
      ## tidy urls
      url.list <- c(url.list,paste("https://web.archive.org",urls,sep=""))

    }

    return(url.list)

  }

  archive.scraped.urls <- URL_Lists_From_Archive(url.vector)

  ## pull the xml files from the urls
  xml.list <- lapply(archive.scraped.urls, FUN = xml2::read_html)

  ## create results list
  res <- list()
  dates <- sapply(strsplit(archive.scraped.urls,split = "/"),function(x) x[5])
  length(res) <- length(dates)
  names(res) <- dates

  ## draw down the text set for the date captures
  res <- lapply(X = xml.list,FUN = rvest::html_nodes,css = css) %>%
    lapply(FUN=rvest::html_text) %>%
    lapply(FUN=matrix,ncol=2,byrow=T) %>%
    lapply(FUN=as.data.frame) %>%
    lapply(function(x) {colnames(x) <- col.names
    return(x)}) %>%
    lapply(subset,Pair!="") %>%
    lapply(subset,nchar(as.character(Pair))!=1) %>%
    lapply(function(x) {row.names(x) <- 1:(dim(x)[1])
    return(x)})

  names(res) <- dates

  return(res)

}
