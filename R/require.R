#' Loading/Attaching Packages
#' 
#' Same functions as \code{library} and \code{require} from base with 
#' improved warnings when overloading S3 methods. These functions use 
#' the underlying base functions together with \code{\link{warnS3Methods}}
#' 
#' @param package,help the name of a package, given as a name or literal character string, 
#' or a character string, depending on whether character.only is FALSE (default) or TRUE).
#' @param pos the position on the search list at which to attach the loaded namespace. 
#' Can also be the name of a position on the current search list as given by search().
#' @param lib.loc a character vector describing the location of R library trees to search 
#' through, or NULL. The default value of NULL corresponds to all libraries currently known 
#' to .libPaths(). Non-existent library trees are silently ignored.
#' @param character.only  a logical indicating whether package or help can be assumed to 
#' be character strings.
#' @param logical.return  logical. If it is TRUE, FALSE or TRUE is returned to indicate success.
#' @param warn.conflicts  logical. If TRUE, warnings are printed about conflicts from attaching 
#' the new package. A conflict is a function masking a function, or a non-function masking 
#' a non-function or a method masking a method. 
#' @param verbose	a logical. If TRUE, additional diagnostics are printed.
#' @param quietly	a logical. If TRUE, no message confirming package attaching is printed, 
#' and most often, no errors/warnings are printed if package attaching fails.
#' @seealso \code{\link[base]{library}}, \code{\link[base]{require}}
#' @examples 
#' \dontrun{
#' require(dplyr)
#' library(roxygen2)
#' }
#' @export
library <- function(package, help, pos = 2, lib.loc = NULL, character.only = FALSE, 
                     logical.return = FALSE, warn.conflicts = TRUE, quietly = FALSE, 
                     verbose = getOption("verbose")){
  if (!character.only) package <- as.character(substitute(package))
  loaded <- paste("package", package, sep = ":") %in% search()
  value <- base::library(package = package, 
                         help = help, 
                         pos = pos, 
                         lib.loc = lib.loc, 
                         character.only = TRUE, 
                         logical.return = logical.return, 
                         warn.conflicts = warn.conflicts, 
                         quietly = quietly, 
                         verbose = verbose)
  if (warn.conflicts & !loaded) warnS3Methods(package)
  invisible(value)
}

#' @rdname library
#' @export
require <- function(package, lib.loc = NULL, quietly = FALSE,
                     warn.conflicts = TRUE,
                     character.only = FALSE){
  if (!character.only) package <- as.character(substitute(package))
  loaded <- paste("package", package, sep = ":") %in% search()
  value <- base::require(package = package, 
                         lib.loc = lib.loc, 
                         quietly = quietly,
                         warn.conflicts = warn.conflicts,
                         character.only = TRUE)
  if (warn.conflicts & !loaded) warnS3Methods(package)
  invisible(value)
}