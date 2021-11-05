#' Open PH guidelines in your default browser
#'
#' `open_*_guidelines()` functions allow you to open any editorial
#' guidelines in the language of your preference
#'
#' @param language the language of the guidelines page you want to open
#' @name open_guidelines
#' @examples
#' \dontrun{
#' open_author_guidelines("en")
#' open_editor_guidelines("es")
#' open_reviewer_guidelines("fr")
#' open_translator_guidelines("pt")
#' }
NULL

#' @rdname open_guidelines
#' @export
open_editor_guidelines <- function(language){

  if(language == "en") {
    utils::browseURL("https://programminghistorian.org/en/editor-guidelines")
  } else if(language == "es") {
    utils::browseURL("https://programminghistorian.org/es/guia-editor")
  } else if(language == "fr") {
    utils::browseURL("https://programminghistorian.org/fr/consignes-redacteurs")
  } else if(language == "pt"){
    utils::browseURL("https://programminghistorian.org/pt/directrizes-editor")
  }
}



#' @rdname open_guidelines
#' @export
open_reviewer_guidelines <- function(language){

  if(language == "en") {
    utils::browseURL("https://programminghistorian.org/en/reviewer-guidelines")
  } else if(language == "es") {
    utils::browseURL("https://programminghistorian.org/es/guia-para-revisores")
  } else if(language == "fr") {
    utils::browseURL("https://programminghistorian.org/fr/consignes-evaluateurs")
  } else if(language == "pt"){
    utils::browseURL("https://programminghistorian.org/pt/directrizes-revisor")
  }

}


#' @rdname open_guidelines
#' @export
open_translator_guidelines <- function(language){

  if(language == "en") {
    utils::browseURL("https://programminghistorian.org/en/translator-guidelines")
  } else if(language == "es") {
    utils::browseURL("https://programminghistorian.org/es/guia-para-traductores")
  } else if(language == "fr") {
    utils::browseURL("https://programminghistorian.org/fr/consignes-traducteurs")
  } else if(language == "pt"){
    utils::browseURL("https://programminghistorian.org/pt/directrizes-tradutor")
  }

}


#' @rdname open_guidelines
#' @export
open_author_guidelines <- function(language){

  if(language == "en") {
    utils::browseURL("https://programminghistorian.org/en/author-guidelines")
  } else if(language == "es") {
    utils::browseURL("https://programminghistorian.org/es/guia-para-autores")
  } else if(language == "fr") {
    utils::browseURL("https://programminghistorian.org/fr/consignes-auteurs")
  } else if(language == "pt"){
    utils::browseURL("https://programminghistorian.org/pt/directrizes-autor")
  }

}

