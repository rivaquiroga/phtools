#' Check if lesson links are active
#'
#' @param lesson_file path to the .md file you want to check (or just
#'     the name if it is on your working directory)
#' @return a data frame with status of each checked link
#' @export
#' @examples
#' \dontrun{check_links("awesome-new-lesson.md")}

check_links <- function(lesson_file){

  # error if file not found

  # get lesson content
  lesson_content <- readLines(lesson_file)

  # split yaml from lesson
  yaml_limits <- which(lesson_content %in% c("---"))

  lesson_yaml <- lesson_content[yaml_limits[1]:yaml_limits[2]]
  lesson_start <- yaml_limits[2]+1
  lesson_text <- paste(lesson_content[lesson_start:length(lesson_content)], collapse = "\n")


  # extract urls

  lesson_urls <- unique(unlist(stringr::str_extract_all(lesson_text, "((ftp|http)s?:\\/\\/(www\\.)?|@)[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-z]{2,4}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)")))

  # check urls


  get_urls <- function(){
    tryCatch(purrr::map(lesson_urls, httr::GET),
             error = function(c) {
               safely_GET <- purrr::safely(httr::GET)
               url_errors <- purrr::map(lesson_urls, safely_GET)
               error_messages <- unlist(purrr::map(url_errors, "error"))
               fails <<- suppressWarnings(stringr::str_subset(error_messages, "^Could not resolve"))
             })
  }

  url_results <- get_urls()

  if(is.character(url_results)){
    print(fails)
  } else {
    status_results <- purrr::map(url_results, httr::http_status)

    tibble::tibble(url = lesson_urls, status = purrr::map_chr(status_results, "message"))
  }

}
