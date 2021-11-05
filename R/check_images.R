#' Check images
#'
#' `check_images()` checks if images in a lesson file comply
#'     Programming Historian editorial guidelines. It will check:
#'     1. if images names in the lesson file match the names of
#'     files in the images folder. If not, it will return
#'     names of images producing the mismatch.
#'     2. if images has the lesson slug as name.
#'     3. if images has the expected extensions.
#'
#' @param lesson_file the .md file with the lesson you want to check
#' @param images_folder the folder where the images of the
#'     lesson are stored
#' @param ignore_extensions whether you want to ignore checking the
#'     extensions of the image files or not. Default value is FALSE,
#'     and that is a good idea, unless the lesson you are editing
#'     has a very good reason to use other formats.
#' @export
#' @examples
#' \dontrun{
#' check_images(lesson_file = "awesome-new-lesson.md",
#'     images_folder = "awesome-new-lesson")}
#'

check_images <- function(lesson_file, images_folder, ignore_extensions = FALSE){

  # check arguments

  if (!stringr::str_detect(lesson_file, "\\.md$")) {
    stop("You need to provide the complete name of the file, including the .md extension.", call. = FALSE)
  }

  folder_images_path <- paste0(images_folder, "/")

  if (!file.exists(folder_images_path)) {
    stop(paste0(
      "Can't find the image folder in your working directory (", getwd(), "). Check if you set your working directory correctly, or if there is a typo in the folder name you provided (", images_folder, ")."
    ), call. = FALSE)
  }

  # get images names and other relevant info

  lesson_content <- paste(readLines(lesson_file), collapse = "\n")

  lesson_slug <- tools::file_path_sans_ext(lesson_file)

  ## check if lesson slug uses unallowed characters

  if(stringr::str_detect(lesson_slug, "[^a-zA-Z0-9\\-]")) {
    special_characters_found <- glue::glue_collapse(stringr::str_extract_all(lesson_slug, "[^a-zA-Z0-9\\-]"), sep = " ")
    stop(glue::glue("The lesson slug has characters that are not allowed: {special_characters_found}"), call. = FALSE)
  }


  folder_images_path <- paste0(images_folder, "/")

  folder_images_names <- list.files(folder_images_path, pattern = "\\.jpg$|\\.png$|\\.gif$|\\.jpeg$|\\.bmp$|\\.tiff$")

  folder_images_total <- length(folder_images_names)

  lesson_images_names <- unlist(stringr::str_extract_all(lesson_content, '(?<=figure.html\\sfilename=").+\\.\\w{3,}(?=")'))

  lesson_images_names_sorted <- stringr::str_sort(lesson_images_names)

  lesson_images_slugs <- unlist(stringr::str_extract_all(lesson_images_names_sorted, "[a-z\\-]+(?=\\d*\\.\\w{3,}$)"))

  lesson_extra_images <- setdiff(lesson_images_names_sorted, folder_images_names)

  folder_extra_images <- setdiff(folder_images_names, lesson_images_names_sorted)

  folder_images_extensions <- tools::file_ext(folder_images_names)


  # check images

  usethis::ui_info("CHECK RESULTS:")

  # if images names in the lesson match the images in the folder, check if they are using the correct slug

  if(identical(lesson_images_names_sorted, folder_images_names)) {

    usethis::ui_done("Images filenames match folder and lesson.")

    if(all.equal.character(rep(lesson_slug, length(lesson_images_slugs)), lesson_images_slugs) != TRUE) {

      usethis::ui_oops("Images don't have the same slug as the lesson file. You can fix this problem by runing {usethis::ui_code('phtools::rename_images()')} ")

      all_names <- paste(lesson_images_names_sorted, collapse = "\n")

      usethis::ui_info("FYI, these are the current filenames: \n{all_names}\n")

    } else {

      usethis::ui_done("The images are using the same slug as the lesson file.")

      if(any(stringr::str_detect(folder_images_extensions, stringr::regex("jpe*g|png"))) == FALSE) {
      usethis::ui_done("You are ready with this part of the process. {phtools:::praise()}") # ESTE MENSAJE NO PUEDE APARECER SI LAS EXTENSIONES DE LOS ARCHIVOS NO SON LAS ACEPTADAS
      }
    }
  }

  # if images names in the lesson do not match the images in the folder, check origin of mismatch


  if(!identical(lesson_images_names_sorted, folder_images_names)) {

    usethis::ui_oops("Images names in the lesson file does not match the filenames in image folder")

    if(!identical(folder_extra_images, character(0))) {

      folder_extra_images_names <- glue::glue_collapse(folder_extra_images, sep = ", ", last = " & ")
      usethis::ui_oops("There are images in the folder that are not included in the lesson: {folder_extra_images_names}")

    }

    if(!identical(lesson_extra_images, character(0))) {

      lesson_extra_images_names <- glue::glue_collapse(lesson_extra_images, sep = ", ", last = " & ")

      usethis::ui_oops("The lesson is using images that were not found in the folder: {lesson_extra_images_names}")
    }

    usethis::ui_todo("Please solve this issues before runing again {usethis::ui_code('check_images()')}")
  }

  # Check images extension



  if(ignore_extensions == FALSE) {


    if(any(stringr::str_detect(folder_images_extensions, stringr::regex("jpe*g")))) {
      usethis::ui_info("(There are images with .jpg or .jpeg extensions. That is ok, but .png is better)")
    }

    if(any(stringr::str_detect(folder_images_extensions, stringr::regex("jpe*g|png")))) {

      images_other_formats <- glue::glue_collapse(folder_images_names[!stringr::str_detect(folder_images_names, stringr::regex(".+jpe*g$|png$"))], sep = ", ", last = " & ")

      usethis::ui_oops(glue::glue("At least one image in the folder is not using .png or .jpg format, which are the ones required: {images_other_formats}"))
    }
  }


  # Check image folder name

  if(!identical(lesson_slug, images_folder)){
    usethis::ui_info("Remember that the folder with the images must have the same name as the lesson file. You need to rename it to match lesson slug ({usethis::ui_code(lesson_slug)})")
  }


}
