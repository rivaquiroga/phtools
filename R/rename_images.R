#' Rename images to match PH guidelines
#'
#' Rename images in the .md and files in the images folder
#'     for them to comply Programming Historian editorial
#'     guidelines.
#'
#' @param lesson_file the .md file with the lesson you want to edit
#' @param images_folder the folder where the images of the
#'     lesson are stored
#' @param backup_files whether you want to keep a copy of the
#'     original files or not. Default value is TRUE.
#' @export
#' @examples
#' \dontrun{
#' rename_images(lesson_file = "awesome-new-lesson.md",
#'     images_folder = "awesome-new-lesson")}

rename_images <- function(lesson_file, images_folder, backup_files = TRUE) {


  lesson_content <- paste(readLines(lesson_file), collapse = "\n")

  folder_images_path <- paste0(images_folder, "/")

  lesson_slug <- tools::file_path_sans_ext(lesson_file)


  # check arguments

  if (!stringr::str_detect(lesson_file, "\\.md$")) {
    stop("You need to provide the complete name of the file, including the .md extension.", call. = FALSE)
  }


  if (!file.exists(folder_images_path)) {
    stop(paste0(
      "Can't find the image folder in your working directory (", getwd(), "). Check if you set your working directory correctly, or if there is a typo in the folder name you provided (", images_folder, ")."
    ), call. = FALSE)

  }


  ## check if lesson slug uses not allowed characters

  if(stringr::str_detect(lesson_slug, "[^a-zA-Z0-9\\-]")) {
    special_characters_found <- glue::glue_collapse(stringr::str_extract_all(lesson_slug, "[^a-zA-Z0-9\\-]"), sep = " ")
    stop(glue::glue("The lesson slug has characters that are not allowed: {special_characters_found}"), call. = FALSE)
  }


  # fix folder name if not slug

  if(!identical(lesson_slug, images_folder)){
    usethis::ui_info("Remember that the folder with the images must have the same name as the lesson file. You need to rename it to match lesson slug ({usethis::ui_code(lesson_slug)})")
    if(usethis::ui_yeah("Do you want me to change it for you now?")){

      file.rename(from = images_folder, to = lesson_slug)

      images_folder <- lesson_slug
      folder_images_path <- paste0(images_folder, "/")


    } else {

      usethis::ui_info("That's ok. You will have to change the folder name manually and then run again rename_images() to complete the process.")

    }
  }


  # get files info

  original_lesson_images_names <- unlist(stringr::str_extract_all(lesson_content, '(?<=figure.html\\sfilename=").+\\.\\w{3,}(?=")'))


  original_folder_images_names <- list.files(folder_images_path, pattern = "\\.jpg$|\\.png$|\\.gif$|\\.jpeg$|\\.bmp$|\\.tiff$")

  original_folder_images_total <- length(original_folder_images_names)

  original_folder_images_names_ordered <-  original_folder_images_names[order(match(original_folder_images_names,original_lesson_images_names))]

  original_images_extensions <- tools::file_ext(original_folder_images_names_ordered)


  # check if images in lesson and folder match

  if(!identical(original_lesson_images_names, original_folder_images_names_ordered)) {

    stop("Images names in the lesson file don't match the filenames in image folder. You need to run first check_images() to identify the source of the mismatch.", call. = FALSE)
  }


  # backup files
  if(backup_files == TRUE) {

    invisible(file.copy(from = lesson_file, to = paste0("BACKUP-", lesson_file)))

    if (!dir.exists(paste0("BACKUP-", images_folder))){
      dir.create(path = paste0("BACKUP-", images_folder))
    }

    original_images_with_path <- paste0(folder_images_path, original_folder_images_names_ordered)

    invisible(file.copy(from = original_images_with_path, to = paste0("BACKUP-", folder_images_path, original_folder_images_names_ordered)))

    usethis::ui_done("backup files created")

  }


  # rename images in lesson


  lesson_images_numbers <- 1:length(original_lesson_images_names)

  new_names_lesson <- paste0(lesson_slug, lesson_images_numbers, ".", original_images_extensions)

  replacements_lesson <- stats::setNames(new_names_lesson, original_lesson_images_names)

  lesson_content_edited <- stringr::str_replace_all(lesson_content, replacements_lesson)


  writeLines(lesson_content_edited, paste0(lesson_slug,".md"))

  # TODO: ADD "DONE!" MESSAGE AFTER EDITING LESSON


  # rename images in folder

  original_images_with_path <- paste0(folder_images_path, original_folder_images_names_ordered)

  new_names_folder <- paste0(folder_images_path, new_names_lesson)

  invisible(file.rename(from = original_images_with_path, to = new_names_folder))

  # TODO: ADD "DONE!" MESSAGE AFTER EDITING IMAGE FOLDER

}
