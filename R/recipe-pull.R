#' BBC Food recipes scrape
#'
#' \code{Recipe_Pull} takes the saved sitemap of the bccfood website and uses it to pull all the
#' recipes from the website into a list of lists for futher use.
#'
#' @param sitemap.url URL of sitemap. Default = NULL which triggers reading a sitemap listed in sitemap.path. If sitemap.path is
#' not NULL then this will be used in stead of sitemap.url
#' @param sitemap.path Path of sitemap file. Default = NULL which triggers reading the saved bbc.co.uk/food/sitemap.xml data.
#' @param failed Vector which is returned from \code{Recipe_Pull} which contains the positions of failed urls
#'
#' @return List of list of recipes
#'
#' @importFrom rvest html_nodes html_node html_text
#'
#' @export
#'
#'

Recipe_Pull <- function(sitemap.url=NULL,sitemap.path=NULL,failed=NULL){

## Read in the source of the sitemap
if(!is.null(sitemap.url)){

  sitemap <- xml2::read_html(sitemap.url) %>% XML::xmlTreeParse()

} else if (!is.null(sitemap.path)) {

  sitemap <- XML::xmlTreeParse(sitemap.path) %>% XML::xmlTreeParse()

} else {

  sitemap <- xml2::read_xml("inst/sitemap.xml") %>% XML::xmlTreeParse()

}

## Convert to List for easy manipulation
l <- XML::xmlToList(sitemap)

## Clarify the start occcurence of "recipes" in the urls
sruc <- lapply(l,strsplit,"/") %>%
  lapply(function(x){return(is.element("recipes",x[[1]]))}) %>%
  unlist() %>%
  which.max()

sruc.length <- length(strsplit(l[[sruc]],"/")[[1]])

## Check that this is simply www.x.x.x/recipes and if so redo
if(sruc.length==which(strsplit(l[[sruc]],"/")[[1]]=="recipes")){

  start <- lapply(l[-sruc],strsplit,"/") %>%
    lapply(function(x){return(is.element("recipes",x[[1]]))}) %>%
    unlist() %>%
    which.max() + 1

} else {

  start <- sruc

}

## find last recipe position
end <- lapply(l,strsplit,"/") %>%
  lapply(function(x){return(is.element("recipes",x[[1]]))}) %>%
  unlist() %>%
  sum() - start

## subset list that contains the urls
r <- l[start:end]
pos <- 1:length(r)

## create collection vector for http errors
ht.errors <- c()

## preallocate html list
html.list <- list()
length(html.list) <- length(r)

## try reading from the recipe urls and catch those that fail for repeating after
## if failed is specified only consider those urls
if(!is.null(failed)){
  pos <- failed
}

for (i in pos){

  tryCatch({
    html.list[[i]] <- xml2::read_html(r[i]$url.loc)
  }, error=function(e){ht.errors <- c(ht.errors,i)})
  message(paste("html pull ",i,sep=""))

}

## preallocate list of recipes
recipe.list <- list()
length(recipe.list) = length(html.list)

for (h in pos){
ls <- list()

message(paste("scrape ",h,sep=""))
tryCatch({
ls$name <- rvest::html_text(rvest::html_node(html.list[[h]],".content-title__text"))
}, error=function(e){h})

tryCatch({
ls$prepTime <- rvest::html_text(rvest::html_node(html.list[[h]],".recipe-metadata__prep-time"))
}, error=function(e){h})

tryCatch({
ls$cookTime <- rvest::html_text(rvest::html_node(html.list[[h]],".recipe-metadata__cook-time"))
}, error=function(e){h})

tryCatch({
ls$recipeYield <- rvest::html_text(rvest::html_node(html.list[[h]],".recipe-metadata__serving"))
}, error=function(e){h})

tryCatch({
ls$desription <- rvest::html_text(rvest::html_node(html.list[[h]],".recipe-description__text"))
}, error=function(e){h})

tryCatch({
ls$author <- rvest::html_text(rvest::html_node(html.list[[h]],".chef__link"))
}, error=function(e){h})

tryCatch({
ls$recipeInstructions <- rvest::html_text(rvest::html_nodes(html.list[[h]],".recipe-method__list-item-text"))
}, error=function(e){h})

tryCatch({
ls$ingredients <- rvest::html_text(rvest::html_nodes(html.list[[h]],".recipe-ingredients__list-item"))
}, error=function(e){h})

tryCatch({
ls$ingred.links <- rvest::html_text(rvest::html_nodes(html.list[[h]],".recipe-ingredients__link"))
}, error=function(e){h})


recipe.list[[h]] <- ls

}

return(append(recipe.list,ht.errors))


}
