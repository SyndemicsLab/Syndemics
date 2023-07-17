#' GitHub file listing
#' 
#' @param user string: username
#' @param repo string: repository name
#' @param branch string: github branch, "main" by default
#' @param folder string: reposority subdirectory
#' @param pattern string: a pattern of files to match
#' 
#' @importFrom httr GET stop_for_status content
#' 
#' @export

list_files_github <- function(user, repo, branch = "main", folder, pattern){
  req <- httr::GET(paste0("https://api.github.com/repos/", 
                          user, "/", 
                          repo, "/git/trees/", 
                          branch, "?recursive=1"))
 httr::stop_for_status(req)
 
 file_list <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = FALSE)
 
 if(!missing(folder)){
   grep(paste0(folder, "/"), file_list, value = TRUE, fixed = TRUE)
   if(!missing(pattern)){
     grep(pattern, file_list, value = TRUE, fixed = TRUE)
   }
  }
}
