#' NHS Area Team geographies for England
#'
#' A dataframe containing the geographies for NHS Area Teams.
#'
#' @format A data frame with ~87.5k observations and 12 columns:
#' \describe{
#'  \item{long}{Longitude}
#'  \item{lat}{Lattitude}
#'  \item{group}{Group id for polygons}
#'  \item{id}{Numeric id for Area Team, 0-9}
#'  \item{NHSATCD}{ONS code for Area Team}
#'  \item{NHSATNM}{Area Team name}
#' }
#' @source Office for National Statistics
"at_sp_df"
