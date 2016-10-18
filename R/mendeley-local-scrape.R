#' Scrape information from a mendeley sqlite local database
#'
#' \code{Mendeley_DF} takes a provided file path for the location of your local mendeley sqlite database.
#' Function ultimately links with the shiny directory (to be moved to own package soon) to help with
#' visualisation and semantic creation of useful literature tool.
#'
#' @param path File path for local mendeley database. Usually found in users/x/AppData/Local/Mendeley Ltd/Mendeley Desktop/...sqlite
#'
#' @importFrom RSQLite dbConnect dbGetQuery SQLite
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @return dataframe of scraped information relating to mendeley database
#'


Mendeley_DF <- function(path = "C:/Users/Oliver/AppData/Local/Mendeley Ltd/Mendeley Desktop/oj.watson@hotmail.co.uk@www.mendeley.com.sqlite"){

  #### EXTRA INTERNAL FUNCTIONS ####

  author_collate <- function(documentId){
    rows <- which(authors$documentId==documentId)
    if(length(rows)==0){
      authors <- NA
    } else {
      authors <- paste(authors$Names[rows],collapse = ", ")
    }
    return(authors)
  }

  keywords_collate <- function(documentId){
    rows <- which(keywords$documentId==documentId)
    if(length(rows)==0){
      keywords <- NA
    } else {
      keywords <- paste(keywords$keyword[rows],collapse = ", ")
    }
    return(keywords)
  }

  notes_collate <- function(documentId){
    rows <- which(notes$documentId==documentId)
    if(length(rows)==0){
      notes <- NA
    } else {
      notes <- notes$text[rows]
    }
    return(notes)
  }

  folder_dag <- function(folderId,dag=FALSE){

    rows <- which(Folders$id==folderId)
    parentId <- Folders$parentId[rows]
    name <- Folders$name[rows]
    while(parentId!=-1){

      new.rows <- which(Folders$id==parentId)
      name <- c(Folders$name[new.rows],name)
      parentId <- Folders$parentId[new.rows]
    }

    if(!dag){
      name <- name[1]
    } else {
      name <- paste(name[-1],collapse = " > ")
    }
    return(name)
  }

  folder_collate <- function(documentId,dag){
    rows <- which(documentfolders$documentId==documentId)
    if(length(rows)==0){
      folders <- NA
    } else {
      folder.ids <- documentfolders$folderId[rows]
      if(dag){
        folders <- Folders$dag[match(folder.ids,Folders$id)]
      } else {
        folders <- Folders$main[match(folder.ids,Folders$id)]
      }
    }
    return(paste(unique(folders),collapse = " || "))
  }

  paths_collate <- function(documentId){
    rows <- which(filehashes$documentId==documentId)
    if(length(rows)==0){
      path <- NA
    } else {
      hash <- filehashes$hash[rows]
      path <- cleaned.paths[match(hash,paths$hash)]
    }
    return(path[1])
  }


  notes_splitting <- function(notes,semantic){

    sub.notes <- strsplit(notes,"]",fixed=T)

    pos.methods <- which(unlist(lapply(sub.notes,FUN=grepl,pattern="METHODS",fixed=T)))
    pos.findings <- which(unlist(lapply(sub.notes,FUN=grepl,pattern="FINDINGS",fixed=T)))
    pos.data <- which(unlist(lapply(sub.notes,FUN=grepl,pattern="DATA",fixed=T)))
    pos.ideas <- which(unlist(lapply(sub.notes,FUN=grepl,pattern="IDEAS",fixed=T)))

    if(semantic=="METHODS"){
      semantics <- gsub(pattern = "METHODS[<br/>",replacement = "",sub.notes[[1]][pos.methods],fixed = T)
    } else if (semantic=="FINDINGS"){
      semantics <- gsub(pattern = "FINDINGS[<br/>",replacement = "",sub.notes[[1]][pos.findings],fixed = T)
    } else if (semantic=="DATA"){
      semantics <- gsub(pattern = "DATA[<br/>",replacement = "",sub.notes[[1]][pos.data],fixed = T)
    } else {
      semantics <- gsub(pattern = "IDEAS[<br/>",replacement = "",sub.notes[[1]][pos.ideas],fixed = T)
    }

    if(length(semantics)==0) {
      semantics <- ""
    }

    return(semantics)

  }
  #### Main Function ####

  # Previous exploratory functions if interest at further tables is needed

  #path <- "C:/Users/Oliver/AppData/Local/Mendeley Ltd/Mendeley Desktop/oj.watson@hotmail.co.uk@www.mendeley.com.sqlite"
  #RSQLite::dbListTables(con)
  #RSQLite::dbListFields(sql,name = RSQLite::dbListTables(con)[22])

  con <- RSQLite::dbConnect(drv=RSQLite::SQLite(), dbname=path)

  ## pull elements needed

  Documents <- RSQLite::dbGetQuery( con,"SELECT id,read,abstract,title,pages,length,pmid,year,publication FROM Documents" )

  Folders <- RSQLite::dbGetQuery( con,"SELECT id,name,parentId FROM Folders")

  documentfolders <- RSQLite::dbGetQuery( con,"SELECT documentId ,folderId FROM DocumentFolders")

  keywords <- RSQLite::dbGetQuery( con,"SELECT documentId ,keyword FROM DocumentKeywords")

  tags <- RSQLite::dbGetQuery( con,"SELECT documentId ,tag FROM DocumentTags")

  authors <- RSQLite::dbGetQuery( con,"SELECT id,documentId,firstNames,lastName,contribution FROM DocumentContributors" )

  annotations <- RSQLite::dbGetQuery( con,"SELECT id, documentId, page, note, createdTime, modifiedTime FROM FileNotes")

  notes <- RSQLite::dbGetQuery( con,"SELECT id, documentId, text FROM DocumentNotes")

  filehashes <- RSQLite::dbGetQuery( con,"SELECT documentId, hash FROM DocumentFiles")

  paths <- RSQLite::dbGetQuery( con,"SELECT localUrl, hash FROM Files")

  cleaned.paths <- lapply(paths$localUrl,FUN = URLdecode) %>%
    lapply(`Encoding<-`,"UTF-8") %>%
    unlist()


  #### organisation ###

  authors$Names <- paste(authors$firstNames,authors$lastName)
  Encoding(authors$lastName) <- "UTF-8"
  Encoding(authors$firstNames) <- "UTF-8"

  Folders$dag <- unlist(sapply(Folders$id,FUN=folder_dag,dag=TRUE))
  Folders$main <- unlist(sapply(Folders$id,FUN=folder_dag,dag=FALSE))

  res <- data.frame("Id"=Documents$id,
                    "Title"=Documents$title,
                    "Authors"=unlist(sapply(Documents$id,FUN = author_collate)),
                    "Year"=Documents$year,
                    "Journal"=Documents$publication,
                    "Keywords"=unlist(sapply(Documents$id,FUN = keywords_collate)),
                    "Notes"=as.character(unlist(sapply(Documents$id,FUN = notes_collate))),
                    "Abstract"=Documents$abstract,
                    "Main.Folder"=unlist(sapply(Documents$id,FUN = folder_collate,dag=FALSE)),
                    "Folder.DAG"=unlist(sapply(Documents$id,FUN = folder_collate,dag=TRUE)),
                    "Paths"=unlist(sapply(Documents$id,FUN = paths_collate)),
                    stringsAsFactors = FALSE)

  res$Methods <- unlist(lapply(res$Notes,notes_splitting,semantic="METHODS"))
  res$Findings <- unlist(lapply(res$Notes,notes_splitting,semantic="FINDINGS"))
  res$Data <- unlist(lapply(res$Notes,notes_splitting,semantic="DATA"))
  res$Ideas <- unlist(lapply(res$Notes,notes_splitting,semantic="IDEAS"))

  return(res)

}
