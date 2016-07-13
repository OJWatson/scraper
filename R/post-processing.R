#' BBC Food recipes post processing
#'
#' \code{Post_Process_Recipes} takes the sraped recipe list from \code{Recipe_Pull} and cleans text encode errors
#'
#' @export
#'
#'


Post_Process_Recipes <- function(recipe.list){


  recipe.list <- r

  tbr <- c("Â¼","Â½","Â¾","â€“","â€™","Ã©","â€˜","Ã¨","Ã®")
  rep <- c(0.25,0.5,0.75,"-","'","é","'","è","î")

  for (rl in 1:length(recipe.list)){
    message(rl)
    for(j in 1:length(tbr)){

      recipe.list[[rl]]$name <- gsub(recipe.list[[rl]]$name ,pattern = tbr[j],replacement = rep[j])
      recipe.list[[rl]]$prepTime <- gsub(recipe.list[[rl]]$prepTime ,pattern = tbr[j],replacement = rep[j])
      recipe.list[[rl]]$cookTime <- gsub(recipe.list[[rl]]$cookTime ,pattern = tbr[j],replacement = rep[j])
      recipe.list[[rl]]$author <- gsub(recipe.list[[rl]]$author ,pattern = tbr[j],replacement = rep[j])
      recipe.list[[rl]]$recipeYield <- gsub(recipe.list[[rl]]$recipeYield ,pattern = tbr[j],replacement = rep[j])
      recipe.list[[rl]]$recipeInstructions <- gsub(recipe.list[[rl]]$recipeInstructions ,pattern = tbr[j],replacement = rep[j])
      recipe.list[[rl]]$ingredients <- gsub(recipe.list[[rl]]$ingredients ,pattern = tbr[j],replacement = rep[j])
      recipe.list[[rl]]$ingred.links <- gsub(recipe.list[[rl]]$ingred.links ,pattern = tbr[j],replacement = rep[j])
    }
  }

return(recipe.list)
}
