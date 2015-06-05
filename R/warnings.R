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
#' @param warn.conflicts	logical. If TRUE, warnings are printed about conflicts from attaching 
#' the new package. A conflict is a function masking a function, or a non-function masking 
#' a non-function or a method masking a method. 
#' @param verbose	a logical. If TRUE, additional diagnostics are printed.
#' @param quietly	a logical. If TRUE, no message confirming package attaching is printed, 
#' and most often, no errors/warnings are printed if package attaching fails.
#' @seealso \code{\link[base]{library}}, \code{\link[base]{require}}
#' @examples 
#' \dontrun{
#' require2(dplyr)
#' library2(roxygen2)
#' }
#' @export
library2 <- function(package, help, pos = 2, lib.loc = NULL, character.only = FALSE, 
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
  
#' @rdname library2
require2 <- function(package, lib.loc = NULL, quietly = FALSE,
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

#' Warn if package overloads S3 methods
#' 
#' Warn if a package overloads S3 methods from other loaded packages
#'
#' @param pkg character of package to ckeck. If NULL, check all loaded packages.
#' 
#' @examples
#' warnS3Methods()
#' @export
warnS3Methods <- function(pkg = NULL){
  if (is.null(pkg)) pkg <- loadedNamespaces()
  masked <- getMaskedS3Methods()
  masked_id <- unique(masked[masked[, "package"] %in% pkg, "id"])
  masked <- masked[masked[, "id"] %in% masked_id, ]
  if (nrow(masked) == 0) return(NULL)
  if (length(pkg) == 1){ 
    cat("The following methods are masked by 'package:", pkg, "':\n\n", file = stderr(), sep = "")
    for (id in masked_id){
      df <- masked[masked[, "id"] == id & masked[, "package"] != pkg, ]
      cat("    '", df[, "method"], ".", df[, "class"], "' from 'package:", df[, "package"], "'\n", 
          file = stderr(), sep = "")
    }
  } else {
    cat("The following methods are available in multiple packages: \n\n")
    for (id in masked_id){
      df <- masked[masked[, "id"] == id, ]
      cat("    '", df[1, "method"], ".", df[1, "class"], "' in packages: ", 
          paste(df[, "package"], collapse = ", "), "\n", sep = "")
    }
  }
  invisible(masked)
}

# Get all S3 methods from a package
# 
# Find all S3 methods from a package
# 
# @param pkg can be either the name of an installed package
# or the path of a package
getPkgS3Methods <- function(pkg){
  if (basename(pkg) == pkg) pkg <- path.package(pkg)
  ns <- parseNamespaceFile(basename(pkg), 
                           dirname(pkg), 
                           mustExist = FALSE)
  if (length(ns$S3methods) == 0) return(NULL)
  df <- cbind.data.frame(basename(pkg), ns$S3methods, stringsAsFactors = FALSE)
  colnames(df) <- c("package", "method", "class", "other")
  df
}

# Get masked S3 methods
# 
# Finds all S3 methods that are currently available that are
# duplicated
getMaskedS3Methods <- function(){
  paths <- find.package(loadedNamespaces())
  lst <- lapply(paths, getPkgS3Methods)
  all_methods <- do.call(rbind, lst)
  duplicates <- 
    duplicated(all_methods[, c("method", "class")]) |
    duplicated(all_methods[, c("method", "class")], fromLast = TRUE)
  res <- all_methods[duplicates, ]
  res <- res[order(res$method, res$class, res$package), ]
  res[, "id"] <- cumsum(!duplicated(res[, c("method", "class")]))
  res
}