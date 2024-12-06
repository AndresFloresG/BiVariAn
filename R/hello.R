# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @name hello
#' @aliases hello
#' @title Hello, World!
#' @param hello Function that prints hello world
#' @description
#' Prints 'Hello, world!'
#'
#' @examples
#' hello()
#' "Hello, world!"
#' @export

hello <- function() {
  print("Hello, world!")
}
