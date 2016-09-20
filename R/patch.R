#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

require(dplyr)
require(lazyeval)

#' patch if there as a non-NA value present in patch
if_present <- function(new_val, old_val) { if_else(is.na(new_val), old_val, new_val) }

#' patch only on NA values
if_missing <- function(new_val, old_val) { if_else(is.na(old_val), new_val, old_val) }

#' patch a tibble
#'
#' @param data
#' @param patch_data
#' @param ... columns to patch
#' @param by columns to join patch on
#' @param patch_fun default: coalesce
#'
#' @return `data` patched
#' @export
#'
#' @examples
patch <- function(data, patch_data, ..., by = NULL, patch_fun=if_present) {
  patch_cols <- unname(dplyr::select_vars(colnames(data), ...))
  if(length(patch_cols) == 0) patch_cols <- NULL

  patch_(data, patch_data, patch_cols, by, patch_fun)
}

patch_ <- function(data, patch_data, patch_cols = NULL, by = NULL, patch_fun=coalesce) {
  if(is.null(by)) {
    error("`by` must be specified")
  }

  # Find common cols
  common_cols  <- intersect(colnames(data), colnames(patch_data))

  # If patch_cols is unspecified, patch using the intersection instead.
  if(is.null(patch_cols)) {
    patch_cols <- common_cols %>% setdiff(by)
    message("Patching, Columns = ", utils::capture.output(dput(by)))
  }

  # No missing columns
  missing_cols <- setdiff(c(patch_cols, by), common_cols)
  if( length(missing_cols) > 0 ) {
    stop("Can not apply patch, columns ", paste(missing_cols, sep=", "),
         "must be in both the original data and the patch")
  }

  # No columns being patched, warn and return data as-is
  if( length(patch_cols) == 0 ) {
    warning("No rows in y to patch onto x")
    return(data)
 }

  # Can not join by a patching column
  if( length(intersect(by, patch_cols)) != 0 ) {
    stop("Cannot patch a joining column")
  }

  # Builds each term of transmute expressions for colname of x
  build_expr <- function(colname) {
    if(colname %in% patch_cols) {
      # coalesce the two columns together x = coalesce(y, x)
      interp(
        ~patch_fun(colname_y, colname_x),
        colname_x = as.name( paste(colname, "x", sep=".") ),
        colname_y = as.name( paste(colname, "y", sep=".") )
      )
    } else {
      # identity x = x
      interp( ~colname, colname=as.name(colname) )
    }
  }

  expr <- Map(build_expr, colnames(data))

  # number of rows produced by join should be unchanged,
  # keep only needed columns and use only distinct rows
  joined <- left_join(
    data,
    patch_data %>% select(one_of(union(patch_cols, by))) %>% distinct,
    by=by
  )

  if( nrow(data) != nrow(joined) ) {
    stop("patch cannot be many-to-one with respect to `data`")
  }

  joined %>% transmute_(.dots=expr)
}


