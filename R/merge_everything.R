#' Merge annual tables after organisation mergers have occurred
#'
#' Create an Excel file with applied mergers.
#'
#' Function takes as the inputs: a path to the file, list(s) with Trust/CCG codes
#' and indication if this is MRSA/non-MRSA file. It utilizes these information to create new .xlsx file
#' that have applied mergers according to the codes provided. During this process all formatting
#' used in original file is lost. To use this function the right file must be used. Using wrong file type
#' could results in wrong calculations/errors. Structure of input code(s) is very important.
#'
#' @param merge_file A path to an existing .xlsx file.
#' @param file_type Character string ("other", "mrsa") or corresponding numerical value (1, 2)
#' @param trust_list A list of Trust codes to be merged.
#' @param ccg_list A list of CCG codes to be merged.
#'
#' @return "Result.xlsx" file with applied merges.
#'
#' @note Some warnings are hidden with SupressWarnings() function.
#'
#' @examples
#' Excel file with the right structure must be used. Function works differently for MRSA and non-MRSA files:
#' MRSA_file <- "C:/My Documents/MRSA.xlsx"
#'
#' Trust codes RCF, RFF and RAE will be merged into RCF and RF4, R1H into R1H.
#' MRSA_trust <- list(list(c("RCF", "RFF", "RAE"), "RCF"), list(c("RF4", "R1H"), "R1H"))
#'
#' CCG codes 07L, 07M and 07N will be merged into 07L.
#' MRSA_ccg <- list(list(c("07L", "07M", "07N"), "07L"))
#'
#' Execute function by, remember to choose the right file type:
#' merge_everything(MRSA_file, 2, MRSA_trust, MRSA_ccg)
#' @import assertthat
#' @import crayon
#' @import openxlsx
#' @export

#Main function.
merge_everything <- function (merge_file, file_type, trust_list = NULL,
                              ccg_list = NULL, result_file = "Result.xlsx") {
  result_file <<- result_file

  #subfunction, previously main function.
  try_merge <- function (merge_file, file_type, trust_list, ccg_list) {

  #Body of the try_merge function.

  #Collect information with Trust/CCG codes
  #Check that codes are provided
  trust_null <- is.null(trust_list)
  ccg_null <- is.null(ccg_list)

  #Perform merges with Trust codes

  #If no trust code provided ignore this part and go to ccg codes
  if (trust_null == FALSE){

    #Numner of Trust merges
    t_length <- length(trust_list)

    #Extract all Trust merges
    for (i in 1:t_length) {
      assign(paste0("trust", i), trust_list[[i]])
      }

    #Merge all worksheets using 1 trust code, it will create "Results.xlsx" file
    merge_all(merge_file, file_type, trust_code = trust1, ccg_code = NULL)


    #Merge remaining Trusts codes.
    if (t_length > 1) {
      for (i in 2:t_length) {
        merge_all(result_file, file_type, trust_code = get(paste0("trust", i)),
                  ccg_code = NULL)
        }
      }
    }

  #Perform merges with CCG codes

  #if no ccg codes provided ignore this part and function stops
  if (ccg_null == FALSE) {

    #Number of CCG merges
    c_length <- length (ccg_list)

    #Extract all CCG merges
    for (i in 1:c_length) {
      assign(paste0("ccg", i), ccg_list[[i]])
    }

    #Check for "Results.xlsx" file
    if (trust_null == TRUE) {

      #Merge all worksheets using 1 CCG code, it will create "Results.xlsx" file
      merge_all(merge_file, file_type, trust_code = NULL, ccg_code = ccg1)
    }

    #"Results.xlsx" saved, overwrite it.
    else {
      merge_all(result_file, file_type, trust_code = NULL, ccg_code = ccg1)
    }

    #Merge remaining CCG codes.
    if (c_length > 1) {
      for (i in 2:c_length) {
      merge_all(result_file, file_type, trust_code = NULL,
                ccg_code = get(paste0("ccg", i)))
      }
    }
  }
  }

  #Body of the merge_everything function - it only checks function for errors
  if (assertthat::is.error(try(try_merge(merge_file, file_type, trust_list,
                                         ccg_list)))==T){
    cat(crayon::red$bold("\nERROR\n"),
        crayon::red("\nPlease ensure you have provided the right input parameters.\n\"replacement has length zero\" - make sure your lists match corresponding function arguments.\n\"object ... not found\" - check your input file"
                                      ))

    #Error massages

    #Not allowed file_type used
    allowed_ftypes <- c(1, 2, "other", "mrsa")

    if (!file_type %in% allowed_ftypes) {
      cat((crayon::red("\nYou have entered the wrong file type. \nFor non-MRSA file please type 1 no quotes or other with quotes. \nFor MRSA file lease type 2 no quotes or mrsa with quotes")))}
  }

  #Miracle - function works; display information
  else cat(
    crayon::magenta$bold("\nDONE!. \nYou have successfully used file:",
                         crayon::blue(merge_file), "using parameters:",
                         if(file_type == 1 | file_type == "other")
                           crayon::blue("\nNon-MRSA file type.")
                         else if (file_type == 2 | file_type == "mrsa")
                           crayon::blue("\nMRSA file type.")
                         else crayon::red("\nYou haven't provided the right file type. Function hasn't done anything!"),
                         if (is.null(trust_list) & is.null(ccg_list))
                           crayon::red("\nYou haven't provided Trust and CCG codes. Function hasn't done anything!")
                         else if(is.null(trust_list)) crayon::red("\nYou haven't provided Trust codes!")
                         else if (is.null(ccg_list)) crayon::red("\nYou haven't provided CCG codes!")
                         else (crayon::blue("\nYou have used Trust and CCG codes.")),

  "\nSuccessfully merged file can be found in", crayon::blue(getwd()),
  "folder.\nFile name used:", crayon::blue(result_file), "."))

  rm(result_file, envir = .GlobalEnv)
}

#' Essential function for merge_everything.
#'
#' Operates on worksheets, returns worksheet.
#'
#' @export

#Subfunction - it performs merges in one worksheet using one code. Return: merged worksheet.
merge_one <- function (worksheet_tbm, codes, is_SSRSS = FALSE) {

  #Gather necessary information@

  #Rows that will be merged, store as a vector
  rows_number <- 0
  for (i in 1:length(codes[[1]])) {
    x <- grep(codes[[1]][i], worksheet_tbm[,1])
    rows_number[i] <- x
  }

  #Search row where data starts
  code_row <- (grep("Trust code|CCG code", worksheet_tbm[,1], ignore.case = T))

  #Rows above and below, important references to the data structure
  code_down <- code_row + 1
  code_up <- code_row - 1

  #Search column where data starts
  name_row <- (grep("Trust Name|CCG Name", worksheet_tbm[code_row,],
                    ignore.case = T))

  #Unique cells with "totals" string
  search_totals <- grep("Total", worksheet_tbm[code_down, ])
  last_total <- length(search_totals)

  #Columns with data
  start_column <- name_row[1]+1
  end_column <- dim(worksheet_tbm)[2]-1
  column_range <- c(start_column:end_column)

  #Check if data splits, if so define regions of split
  if (sum(!is.na(worksheet_tbm[code_up, ]==T)) == 0) {
    is_splitted <- FALSE
  }
  else {
    split_column <- min(which(!is.na(worksheet_tbm[code_up, ]==T)))
    split_range1 <- c(1:(split_column-start_column))
    split_range2 <- c(-1:((split_column-start_column)*-1))
    is_splitted <- (split_column != start_column)
    period_count <- length(split_range1)
  }

  #Final row that other will be merged
  merged_row <- grep(codes[[2]], worksheet_tbm[,1])

  #Create data frame with data.
  df_char <- worksheet_tbm[rows_number,column_range]
  df_num <-  suppressWarnings(as.data.frame(sapply(df_char, as.numeric)))

  #Perform merges according to data type and structure
  #Merge when data splits
  if (is_splitted == T) {
    df_num1 <- df_num[, split_range1, drop = FALSE]
    df_num2 <- df_num[, split_range2]
    df_num1 <- colSums(df_num1)
    df_num2 <- colSums(df_num2)
    num2_vec <- NULL

    #Most of the structures that split
    if (is_SSRSS == F) {
      for (i in 0:(period_count-1)) {
        x = (5*i)+2
        y = (5*i)+4
        num2_vec <- append(num2_vec, x)
        num2_vec <- append(num2_vec, y)
      }
      num1_vec <- rep(df_num1, each = 2)
      for (i in 1:length(num2_vec)) {
        df_num2[num2_vec[i]] <- df_num2[(num2_vec[i])-1] / num1_vec[i] * 100000
      }
    }
    else {

      #MRSA worksheet 4 and 5 splits using this formula
      for (i in 0:(period_count-1)) {
        x = (5*i)+3
        num2_vec <- append(num2_vec, x)
      }
      num1_vec <- rep(df_num1, each = 2)
      for (i in 1:length(num2_vec)) {
        df_num2[num2_vec[i]] <- df_num2[(num2_vec[i])-1] / num1_vec[i] * 100000
      }
    }
    final_row <- append(df_num1,df_num2)
  }

  #Merge when data is not splitted
  else {
    final_row <- colSums(df_num)
  }

  #Replece merged row with final row
  worksheet_tbm[merged_row, column_range] <- final_row

  #Remove other rows that are redundant and return merged worksheet
  remove_rows <- rows_number[!rows_number %in% merged_row]
  worksheet_tbm <- worksheet_tbm[-remove_rows, ]
  return(worksheet_tbm)
}


#' Essential function for merge_everything.
#'
#' Operates on files, returns file.
#'
#' @export

#Subfunction, it performs merges on all worksheets with trust and/or ccg code.
#Returns merged 1 file with codes used (maximum 1 pair of codes).
merge_all <- function (merge_file, file_type, trust_code, ccg_code) {

  # Create worksheets and workbook
  sheet_number <- c(2:length(openxlsx::getSheetNames(merge_file)))
  for (i in sheet_number) {
    assign (paste("worksheet", i, sep = ""),
            openxlsx::read.xlsx(xlsxFile = merge_file, sheet = i, startRow = 1,
                                colNames = TRUE,rowNames = FALSE,
                                skipEmptyRows = FALSE, skipEmptyCols = FALSE))
  }
  temp_workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(temp_workbook, sheetName = "empty")

  #Flow control for different file types. "is.null..." skips merging when no codes are provided.
  switch(file_type,

         #Process non-MRSA file when main function argument "file_type" is "1" or "other".
         other = {
           for (i in sheet_number) {
             addWorksheet(temp_workbook, i)
           }
           if (!is.null(trust_code)) {
             worksheet2 <- merge_one(worksheet_tbm = worksheet2,
                                     codes = trust_code, is_SSRSS = F)
             worksheet3 <- merge_one(worksheet_tbm = worksheet3,
                                     codes = trust_code, is_SSRSS = F)
           }
           if(!is.null(ccg_code)) {
             worksheet4 <- merge_one(worksheet_tbm = worksheet4,
                                     codes = ccg_code, is_SSRSS = F)
             worksheet5 <- merge_one(worksheet_tbm = worksheet5,
                                     codes = ccg_code, is_SSRSS = F)
           }
           for (i in sheet_number) {
             writeData(temp_workbook, i, get(paste0("worksheet", i)))
           }
           saveWorkbook(temp_workbook, result_file, overwrite = T)
           return(result_file)
         },

         #Process MRSA file when main function argument "file_type" is "2" or "mrsa".
         mrsa = {
           for (i in sheet_number) {
             addWorksheet(temp_workbook, i)
           }
           if (!is.null(trust_code)) {
             worksheet2 <- merge_one(worksheet_tbm = worksheet2,
                                     codes = trust_code, is_SSRSS = F)
             worksheet4 <- merge_one(worksheet_tbm = worksheet4,
                                     codes = trust_code, is_SSRSS = T)
             worksheet6 <- merge_one(worksheet_tbm = worksheet6,
                                     codes = trust_code, is_SSRSS = F)
             worksheet7 <- merge_one(worksheet_tbm = worksheet7,
                                     codes = trust_code, is_SSRSS = F)
           }
           if(!is.null(ccg_code)) {

             worksheet3 <- merge_one(worksheet_tbm = worksheet3,
                                     codes = ccg_code, is_SSRSS = F)
             worksheet5 <- merge_one(worksheet_tbm = worksheet5,
                                     codes = ccg_code, is_SSRSS = T)
             worksheet8 <- merge_one(worksheet_tbm = worksheet8,
                                     codes = ccg_code, is_SSRSS = F)
             worksheet9 <- merge_one(worksheet_tbm = worksheet9,
                                     codes = ccg_code, is_SSRSS = F)
           }

           #Saves R objcts and wirte xlsx file
           for (i in sheet_number) {
             openxlsx::writeData(temp_workbook, i, get(paste0("worksheet", i)))
           }
           openxlsx::saveWorkbook(temp_workbook, result_file, overwrite = T)
         }
  )
}
