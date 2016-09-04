
#' PDF scrape text from a list of pdf urls generated from a scrape of href urls given minimal css
#'
#' \code{URL_PDF_Text_Pull} takes a url and scrapes your wanted urls from a minimum css selector. These urls are
#' pdfs which are then downloaded and text scraped for 2 words that appear after a provided string.
#'
#'
#'
#' @param url Url containing links to pdfs
#' @param css Minimal css selector for links in url
#' @param string String which is to eb matched from pdf.
#' @param pdf.dir.dump Directory path where pdfs are downloaded to
#' @param downloaded Boolean determining if \code{URL_PDF_Text_Pull} has already
#' been called and thus there is no need to redownload pdfs. Default = FALSE
#' @param col.names vector of length 2 for data frame result names
#' @param search.length Integer giving the length of the pdf text to search after the occurence of string
#' @param words Vector of integer determining which words to store from the search length. N.B. function will fail if the
#' number of words is greater than the actual number of words that appear after the search string search length
#'
#' @importFrom rvest html_children html_nodes html_attrs html_attr html_text
#' @importFrom magrittr %>%
#' @importFrom stringr str_trim str_locate str_detect
#' @importFrom qdap clean
#' @importFrom pdftools pdf_text
#'
#' @export
#'
#' @return list of dataframes of scraped information
#'
#'
#' @return list of dataframes of scraped information

URL_PDF_Text_Pull <- function(url = "http://www.who.int/globalchange/resources/country-profiles/en/",
                              css = ".a_z a",
                              string = "Population (2013)",
                              pdf.dir.dump,
                              downloaded = FALSE,
                              col.names = c("Number","Size"),
                              search.length = 200,
                              words = c(2,3)){


  ## EXTRA FUNCTIONS ##

  # function to find urls of links in provided url given css
  URL_Lists_From_CSS_Selector <- function(url.vector,css){

    ## pull the xml files from the urls
    xml.list <- xml2::read_html(url.vector)

    ## draw down the node set from the css selector
    node.set <- rvest::html_nodes(xml.list,css)

    ## isolate the position in contents that will corresponds to the url
    link.names <- xml2::xml_contents(node.set) %>% rvest::html_text()

    ## fetch urls
    internal.urls <- rvest::html_attr(node.set,"href")

    ## store these as the function output
    urls <- internal.urls

    ## find urls that are full urls:
    full.boolean <- unlist(lapply(strsplit(internal.urls,"/"),function(x){return(x[[1]]=="http:")}))

    ## split the truly internal url references and the full url
    internal.split <- unlist(strsplit(internal.urls[which.min(full.boolean)],"/"))
    full.split <- unlist(strsplit(url.vector,"/"))

    ## find the first match after the first case which will be a blank match
    first.common <- min(tail(match(internal.split,full.split),length(match(internal.split,full.split))-1),na.rm = T)

    ## create the prefix url
    prefix.url <- unlist(strsplit(url.vector,full.split[first.common]))[1]

    ## vector of ending urls
    end.urls <- unlist(strsplit(internal.urls[!full.boolean],full.split[first.common]))[seq(2,length(internal.urls[!full.boolean])*2,2)]

    ## tidy the output of the truly internal urls
    urls[which(!full.boolean)] <- paste(prefix.url,full.split[first.common],end.urls,sep="")


    ## turn into df

    df <- data.frame(urls,row.names = link.names)


    return(df)

  }

  ## grab pdf urls
  scraped.urls <- URL_Lists_From_CSS_Selector(url,css)

  ## create matrix for storing the 2 words
  mat <- matrix(nrow=length(scraped.urls$urls),ncol = length(words))
  no.pdf <- rep(FALSE,length(scraped.urls$urls))

  for (i in 1:length(scraped.urls$urls)){

    tryCatch({

      if(!downloaded){
        download.file(url = as.character(scraped.urls$urls[i]),
                      destfile = paste(pdf.dir.dump,row.names(scraped.urls)[i],".pdf",sep=""),
                      mode = "wb")
      }

      txt <- pdftools::pdf_text(paste(pdf.dir.dump,row.names(scraped.urls)[i],".pdf",sep=""))


      text.present <- stringr::str_detect(txt,stringr::fixed(string))
      page.no <- which(text.present)

      if(length(page.no)>0){

        pos <- stringr::str_locate(txt[page.no],stringr::fixed(string))

        tc <- strsplit(txt, "")[[page.no]]

        string.search <-   paste(tc[(pos[2]+1):(pos[2]+search.length)],sep="",collapse = "")

        mat[i,] <- unlist(strsplit(stringr::str_trim(qdap::clean(string.search))," "))[words]
      }

    }, error=function(e){no.pdf[i] <- TRUE})

  }


  df <- data.frame(mat,row.names = row.names(scraped.urls))
  names(df) <- col.names

  return(df)

}
