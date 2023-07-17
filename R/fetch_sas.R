#' MA DPH PHDW Script Pull
#' Fetches most recent .SAS files for MA DPH work and saves them to a SAS/ subdirectory
#'
#' @importFrom httr GET
#'
#' @export
fetch_sas <- function(){
  if(!dir.exists("SAS")) dir.create("SAS")

  file_list <- list_files_github("SyndemicsLab", "PHD", "main", pattern = ".sas")

  for(i in seq_along(file_list)){
    response <- httr::GET(paste0("https://raw.githubusercontent.com/SyndemicsLab/PHD/main/", file_list[i]),
                    config = config(ssl_verifypeer = FALSE))
    writeBin(content(response, "raw"), paste0("SAS/", gsub(".*/", "", file_list[i])))
  }
}
