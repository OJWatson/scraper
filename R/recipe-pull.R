#' BBC Food recipes scrape
#'
#' \code{Recipe_Pull} takes the saved sitemap of the bccfood website and uses it to pull all the
#' recipes from the website into a list of lists for futher use.
#'
#' @param sitemap url of sitemap. Defauly NULL but in theory could specify url of recipe site sitemap that uses
#' recipe schema
#' @param start Start of recipe urls in sitemap data
#' @param end End of recipe urls in sitemap data
#'
#' @return List of list of recipes
#'
#' @export
#'
#'

Recipe_Pull <- function(sitemap=NULL,start=15,end=11141){


if(is.null(sitemap)){
  data(sitemap)
} else {
sitemap <- XML::xmlTreeParse(sitemap)
}
l <- XML::xmlToList(sitemap)
l <- unlist(l)

max2 <- 0
for (i in 1:length(l)){
  max2 <- max(max2,length(strsplit(l[i],"/")$url.loc))
}

spl.mat <- matrix(data =0,nrow = length(l),ncol = max2)

for (i in 1:length(l)){
  spl.mat[i,1:length(strsplit(l[i],"/")$url.loc)] <- strsplit(l[i],"/")$url.loc
}

start <- 15

r <- l[start:end]

ht.errors <- c()


html.list <- list()

length(html.list) <- length(r)
for (i in 1:length(r)){
  tryCatch({
html.list[[i]] <- xml2::read_html(r[i])
  }, error=function(e){ht.errors <- c(ht.errors,i)})
message(paste("html pull ",i,sep=""))
}

recipe.list <- list()
length(recipe.list) = length(html.list)

for (h in 1:length(html.list)){
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
