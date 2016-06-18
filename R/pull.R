#' BBC Food recipes scrape
#'
#' \code{Recipe_Pull} takes the saved sitemap of the bccfood website and uses it to pull all the
#' recipes from the website into a list of lists for futher use.
#'
#'
#' @export
#'
#'

Recipe_Pull <- function(){




data(sitemap)
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


html.list <- list()
for (i in 15:20){
html.list[[i]] <- xml2::read_html(l[i])
message(i)
}

recipe.list <- list()
length(recipe.list) = length(html.list)

for (h in 1:length(html.list)){
ls <- list()

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


  tbr <- c("Â¼","Â½","Â¾","â€“","â€™","Ã©e","â€˜","Ã¨","Ã®")
  r <- c(0.25,0.5,0.75,"-","'","é","'","è","î")
  for(j in 1:length(tbr)){
    ls$recipeYield <- gsub(ls$recipeYield ,pattern = tbr[j],replacement = r[j])
    ls$recipeInstructions <- gsub(ls$recipeInstructions ,pattern = tbr[j],replacement = r[j])
    ls$ingredients <- gsub(ls$ingredients ,pattern = tbr[j],replacement = r[j])
    ls$ingred.links <- gsub(ls$ingred.links ,pattern = tbr[j],replacement = r[j])
  }


recipe.list[[h]] <- ls

}

return(recipe.list)


}
