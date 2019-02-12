#' Compare two data frames to identify cells which are different
#'
#' Used to identify cells which have changed between publications for later highlighting in Excel export.
#' With thanks to Amy Mikhail for writing this function for me.
#' Not currently production.
#'
#' @param currentdf A data frame in wide format giving the values for the current publication.
#' @param lastdf A data frame in wide format giving the values for the most recent publication prior to the one being prepared.
#' @return A matrix specifying cells that have changed.
#' @export

data_compare <- function(currentdf, lastdf) {

  # Handy wrapper for 'not in':
  `%!in%` = Negate(`%in%`)

  # Create a data.frame showing results of value comparison for each cell
  results = as.data.frame(currentdf[, names(currentdf) %in% names(lastdf)] ==
                            lastdf[, names(lastdf) %in% names(currentdf)])

  # Use 'not in' to get column names not included in the comparison
  cols2add = names(currentdf)[names(currentdf) %!in% names(lastdf)]

  # Add the extra columns to the results and fill with NAs:
  for(i in cols2add)
    results[,i] <- NA

  # Make sure columns match the original order in currentdf:
  results = results[names(currentdf)]

  # Now get the array indices of the values that have changed:
  cells2flag = which(results == FALSE, arr.ind = TRUE)

  # Return the matching data.frame that can be used to colour cells:
  return(cells2flag)
}

# Added function after "original"

#' Compare workbooks for differences and highlight in red cells that are different.
#'
#' Function compares numererical data in worksheets and highlight cells that were changed. Function is limited to
#' specific regions in worksheets.
#'
#' @param old_file An excel file with old data.
#' @param new_file An excel file with new data.
#' @return Modified excel file.
#' @export


# Highligh function - main function for applying red font colour to changes in the workbooks
highlight <- function(old_file, new_file){

  #Font colour red
  red_style = openxlsx::createStyle(fontColour =  "#FF0000")

  #Copy of new file, all changes will be highlighted in this file
  temp_workbook <- openxlsx::loadWorkbook(file = new_file)

  #sheet_number checks how many worksheets is in the file, first worksheet is omitted by default
  sheet_number <- c(2:length(openxlsx::getSheetNames(old_file)))

  #in this loop for every worksheet ("i") we compare old and new worksheets
  for (i in sheet_number) {

    #read new/old worksheet, no skipping empty cells for more clear structure of data frames
    new_sheet <- openxlsx::read.xlsx(xlsxFile = new_file, sheet = i, skipEmptyRows = F, skipEmptyCols = F, colNames = F, rowNames = F)
    old_sheet <- openxlsx::read.xlsx(xlsxFile = old_file, sheet = i, skipEmptyRows = F, skipEmptyCols = F, colNames = F, rowNames = F)

    #new_cols - we compare all the columns from the old file except the last one which does not contain data
    new_cols <- (length(old_sheet)-1)

    #trim functions below make sure data frames are of the same length, which means any new columns added to new file will be excluded
    new_sheettrim <- subset(new_sheet, select = 0:new_cols)
    old_sheettrim <- subset(old_sheet, select = 0:new_cols)

    #using data_compare function (see above) return rows and columns to be highlighted
    dcomp <- data_compare(new_sheettrim, old_sheettrim)
    rows_com <- dcomp[,1]
    cols_com <- dcomp[,2]

    #apply highlighting to workbook, keep other formatting (stack = T)
    openxlsx::addStyle(wb = temp_workbook, sheet = i, style = red_style, rows = rows_com, cols = cols_com, stack = T)
  }
  #save workbook with highlighted cells
  openxlsx::saveWorkbook(wb = temp_workbook, file = "highlight_results.xlsx", overwrite = T)
    }
