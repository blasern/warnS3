#' Double Colon and Triple Colon Operators
#' 
#' Accessing exported and internal variables in a namespace
#' with a warning when loading the namespace.
#' 
#' @param pkg package name
#' @param name variable name
#' 
#' @export
#' @name ns-dblcolon
`::` <- function (pkg, name) 
{
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  loaded_before <- loadedNamespaces()
  value <- getExportedValue(pkg, name)
  loaded_pkgs <- setdiff(loadedNamespaces(), loaded_before)
  if (length(loaded_pkgs)) warnS3Methods(loaded_pkgs)
  value
}

#' @export 
#' @rdname ns-dblcolon
`:::` <- function (pkg, name) 
{
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  loaded_before <- loadedNamespaces()
  value <- get(name, envir = asNamespace(pkg), inherits = FALSE)
  loaded_pkgs <- setdiff(loadedNamespaces(), loaded_before)
  if (length(loaded_pkgs)) warnS3Methods(loaded_pkgs)
  value
}