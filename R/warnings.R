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
  if (nrow(masked) == 0) return(NULL)
  masked_id <- unique(masked[masked[, "package"] %in% pkg, "id"])
  masked <- masked[masked[, "id"] %in% masked_id, ]
  # get used package
  masked[, "masking_package"] <- NA_character_
  for (id in masked_id){
    df <- masked[masked[, "id"] == id, ]
    used_method <- getS3method(df[1, "method"], df[1, "class"])
    used_package <- environmentName(environment(used_method))
    masked[masked[, "id"] == id, "masking_package"] <- used_package
  }
  same <- masked[, "package"] == masked[, "masking_package"]
  masked <- masked[!same, c("method", "class", "package", "masking_package")]
  if (nrow(masked) == 0) return(NULL)
  # messages
  cat("The following S3 methods are masked:\n\n", file = stderr(), sep = "")
  cat(paste0("    '", masked[, "method"], ".", masked[, "class"],
      "' from 'package:", masked[, "package"], 
      "' by package:", masked[, "masking_package"], "'\n"),
      file = stderr(), sep = "")
  # return 
  row.names(masked) <- seq(nrow(masked))
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