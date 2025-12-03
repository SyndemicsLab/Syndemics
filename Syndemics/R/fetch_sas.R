#' MA DPH PHDW Script Pull
#' Fetches most recent .SAS files for MA DPH work and saves them to a SAS/ subdirectory
#'
#' @importFrom httr GET stop_for_status content config
#'
#' @export
fetch_sas <- function() {
    if (!dir.exists("SAS")) {
        dir.create("SAS")
    }
    req <- httr::GET(
        "https://api.github.com/repos/SyndemicsLab/PHD/git/trees/main?recursive=1"
    )
    httr::stop_for_status(req)

    file_list <- grep(
        ".sas",
        unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = FALSE),
        value = TRUE,
        fixed = TRUE
    )

    for (i in seq_along(file_list)) {
        response <- httr::GET(
            paste0(
                "https://raw.githubusercontent.com/SyndemicsLab/PHD/main/",
                file_list[i]
            ),
            config = config(ssl_verifypeer = FALSE)
        )
        writeBin(
            content(response, "raw"),
            paste0("SAS/", gsub(".*/", "", file_list[i]))
        )
    }
}
