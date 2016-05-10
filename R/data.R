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
#'  \item{OBJECTID}{Id for an object. Only used for plotting}
#'  \item{hole}{Logical value for whether geography is a hole. Only used for plotting}
#'  \item{piece}{Only used for plotting}
#'  \item{order}{Gives plotting order}
#'  \item{Shape_STAr}{Only used for plotting}
#'  \item{Shape_STLe}{Only used for plotting}
#' }
#' @source Office for National Statistics
"at_sp_df"

#' NHS subregion geographies for England
#'
#' A dataframe containing the geographies for NHS subregions.
#'
#' @format A data frame with ~71.5k observations and 16 columns:
#' \describe{
#'  \item{long}{Longitude}
#'  \item{lat}{Lattitude}
#'  \item{group}{Group id for polygons}
#'  \item{id}{Numeric id for subregion, 0-9}
#'  \item{GSS_CD}{ONS code for subregion}
#'  \item{GSS_NM}{ONS name for subregion}
#'  \item{GSS_P_CD}{ONS name for parent region}
#'  \item{ODS_CD}{HSCIC ODS code for subregion}
#'  \item{ODS_P_CD}{HSCIC ODS code for parent region}
#'  \item{OBJECTID}{Id for an object. Only used for plotting}
#'  \item{hole}{Logical value for whether geography is a hole. Only used for plotting}
#'  \item{piece}{Only used for plotting}
#'  \item{order}{Gives plotting order}
#'  \item{Shape_STAr}{Only used for plotting}
#'  \item{Shape_STLe}{Only used for plotting}
#'  \item{centroid_lat}{Latitude values for subregion centroids for plotting labels}
#'  \item{centroid_long}{Longitude values for subregion centroids for plotting labels}
#' }
#' @source Office for National Statistics
"subregions_sp_df"
