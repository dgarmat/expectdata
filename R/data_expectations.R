

### data expectation functions -----

#' Check if a data frame has duplicates in a given column. If a vector is given, check for duplicates in the vector.
#'
#' @param df data frame to check
#' @param test_columns character vector name of column expecting no duplicates, if number vector given, column numbers
#' @param stop_if_fail T/F for whether to consider failure an error
#' @param report_duplicates T/F for whether to return a partial list of the top duplicates if failure
#' @param return_df T/F whether to end function with dataframe input (as in if a check in part of a pipe)
#'
#' @return several options depending on whether it fails or succeeeds
#' @export
#' @importFrom dplyr group_by_
#' @importFrom dplyr count
#' @importFrom dplyr filter
#'
#' @examples
#' expect_no_duplicates(mtcars, "cyl")
#' # [1] "top duplicates..."
#' # A tibble: 3 x 2
#' # Groups:   cyl [3]
#' #cyl     n
#' #<dbl> <int>
#' #1     4    11
#' #2     6     7
#' #3     8    14
#' # Error in ifelse(stop_if_fail, stop(paste0("Duplicates detected in column: ",  :
#'                                                 Duplicates detected in column: cyl
#'
#' expect_no_duplicates(rownames(mtcars))
#' # [1] "no vector duplicates...OK"
expect_no_duplicates <- function(df, test_columns = NA, stop_if_fail = TRUE, report_duplicates = TRUE, return_df = TRUE){
  if(return_df){
    df_copy_for_later <- df
  }

  if (!("data.frame" %in% class(df)) & is.vector(df)){
    df <- data.frame("vector" = df)
    test_columns <- "vector"
  }

  # no test column given, assume testing all for duplicates
  if (is.na(test_columns)){
    test_columns <- names(df)
  }

  # if number given, resave as field name with that number
  if (is.numeric(test_columns)){
    test_columns <- names(df)[test_columns]
  }

  if (sum(colnames(df) == as.name(test_columns)) == 0) {
    stop(paste0("No column named: ", test_columns))
  }
  if (sum(colnames(df) == as.name(test_columns)) > 1) {
    stop(paste0("Expected only one, but multiple columns named: ", test_columns))
  }

  for (column in test_columns){
    dft <- group_by_(df, as.name(column))
    dft <- count(dft)
    dft <- filter(dft, n > 1)
    if(nrow(dft) == 0){
      print(paste0("no ", column, " duplicates...OK"))
    } else if(nrow(dft) > 0){
      if(report_duplicates){
        print("top duplicates...")
        dft <- arrange(dft, desc(n))
        print(dft)
      }
      ifelse(stop_if_fail,
             stop(paste0("Duplicates detected in column: ", column)),
             warning(paste0("Duplicates detected in column: ", column)))
    }
  }

  if(return_df){
    df_copy_for_later
  }

}


#' Check if a dataframe has the same number of rows as another, or else 0 rows. If vectors are given the lengths of the vectors are compared.
#'
#' @param df1 dataframe or vector to check (required)
#' @param df2 optional second dataframe or vector to compare (if not given, defaults to zero row data frame)
#' @param stop_if_fail T/F for whether to consider failure an error
#' @param report_rowcount T/F for whether to return the number of rows
#' @param return_df T/F whether to end function with dataframe 1 input (as in if a check in part of a pipe)
#'
#' @return several options depending on whether it fails or succeeeds
#' @export
#'
#' @examples
#' expect_same_number_of_rows(mtcars, mtcars)
#' # [1] "Same number of rows...OK"
#'
#' expect_same_number_of_rows(mtcars, iris)
#' # Error in ifelse(stop_if_fail, stop(paste0("Different number of rows: ",  :
#' #    Different number of rows: 32 vs: 150
#'
#' expect_same_number_of_rows(mtcars)
#' # Error in ifelse(stop_if_fail, stop(paste0("Different number of rows: ",  :
#' #    Different number of rows: 32 vs: 0
expect_same_number_of_rows <- function(df1, df2 = data.frame(), stop_if_fail = TRUE, report_rowcount = FALSE, return_df = TRUE){
  # df2 = data.frame() default means if df2 is not specified, it checks if df1 has zero rows
  if(return_df){
    df_copy_for_later <- df1
  }

  # if df is a vector not a df, make it into a df
  if (!("data.frame" %in% class(df1)) & is.vector(df1)){
    df1 <- data.frame(df1)
  }
  if (!("data.frame" %in% class(df2)) & is.vector(df2)){
    df2 <- data.frame(df2)
  }
  if((!("data.frame" %in% class(df1)) & !is.vector(df1)) |
     (!("data.frame" %in% class(df2)) & !is.vector(df2))){
    print(class(df1))
    print(typeof(df1))
    print(class(df2))
    print(typeof(df2))
    stop("One of the inputs is neither a data frame nor vector")
  }

  if (nrow(df1) == nrow(df2)){
    if(nrow(df2) == 0){
      print(paste0("No rows found as expected...OK"))
    } else{
      print(paste0("Same number of rows", ifelse(report_rowcount, paste0(": ", nrow(df1)), ""), "...OK"))
    }
  } else{
    ifelse(stop_if_fail,
           stop(paste0("Different number of rows: ", nrow(df1), " vs: ", nrow(df2))),
           warning(paste0("Different number of rows: ", nrow(df1), " vs: ", nrow(df2))))
  }

  if(return_df){
    df_copy_for_later
  }
}

#' Check if the column names you expect to be in the df, are indeed in there
#'
#' @param df
#' @param colums_expected a character vector
#' @param return_df T/F whether to end function with dataframe input (as in if a check in part of a pipe)
#'
#' @return
#' @export
#'
#' @examples
#' expect_column_names_somewhere_in_data_frame(mtcars, c("mpg", "cyl"))
#' # [1] "all columns found...OK"
#' expect_column_names_somewhere_in_data_frame(mtcars, c("mpg", "cyl", "car_name"))
#' # Error in expect_column_names_somewhere_in_data_frame(mtcars, c("mpg",  :
#' #   car_name column not found
expect_column_names_somewhere_in_data_frame <- function(df, colums_expected, return_df = TRUE){
  if(return_df){
    df_copy_for_later <- df
  }

  if(sum(names(df) %in% colums_expected) == length(colums_expected)){
    print("all columns found...OK")
  } else{
    cols_not_found <- colums_expected[!(colums_expected %in% names(df))]
    stop(paste0(paste0(cols_not_found, collapse = ", "), " column",
                ifelse(length(cols_not_found) > 1, "s", ""),
                " not found"))
  }

  if(return_df){
    df_copy_for_later
  }
}


#' Check that values in a discrete or categorical vector are within a set of acceptable values
#'
#' @param test_vector vector to test
#' @param correct_vector vector of all acceptable values
#'
#' @return
#' @export
#'
#' @examples
#' expect_values_only_in(mtcars$cyl, c(2, 4, 6))
#' # Error in expect_values_only_in(mtcars$cyl, c(2, 4, 6)) :
#' #   8 value not found in list given
#' expect_values_only_in(mtcars$cyl, c(2, 4, 6, 8))
#' # [1] "all values expected...OK"
expect_values_only_in <- function(test_vector, correct_vector){
  if(typeof(test_vector) != typeof(correct_vector) |
     class(test_vector) != class(correct_vector)){
    stop(paste0("typeof() or class() of test_vector does not match correct_vector"))
  }

  if(sum(!(unique(test_vector) %in% correct_vector)) == 0 ){
    print("all values expected...OK")
  } else{
    vals_not_found <- unique(test_vector)[!(unique(test_vector) %in% correct_vector)]
    stop(paste0(paste0(vals_not_found, collapse = ", "), " value",
                ifelse(length(vals_not_found) > 1, "s", ""),
                " not found in list given"))
  }
}


#' Check if there are any NA values in a data frame, or specified column, within a tolerance
#'
#' @param df
#' @param test_column character string for column to test - optional
#' @param na_tolerance number of NA allowed before failure, default is zero
#' @param return_df T/F whether to end function with dataframe input (as in if a check in part of a pipe)
#'
#' @return
#' @export
#' @importFrom dplyr select_
#'
#' @examples
#' expect_no_nas(mtcars, "cyl")
#' # [1] "Detected 0 NAs...OK"
#' expect_no_nas(mtcars)
#' # [1] "Detected 0 NAs...OK"
#' expect_no_nas(c(0, 3, 4, 5))
#' # [1] "Detected 0 NAs...OK"
#' expect_no_nas(c(0, 3, NA, 5))
#' # Error in expect_no_nas(c(0, 3, NA, 5)) : Detected 1 NAs
expect_no_nas <- function(df, test_column = NA, na_tolerance = 0, return_df = TRUE){
  if(return_df){
    df_copy_for_later <- df
  }

  if(!is.na(test_column)){
    # -i to handle multiple columns, need to iterate, maybe create or find a function that will make mult names
    df <- select_(df, as.name(test_column))
  }

  na_sum <- sum(is.na(df))
  if(na_sum > na_tolerance){
    stop(paste0("Detected ", na_sum, " NAs"))
  } else{
    print(paste0("Detected ", na_sum, " NAs...OK"))
  }

  if(return_df){
    df_copy_for_later
  }
}




#' Check if a set of columns are dates. Useful for debugging PowerBI R scripts with dates.
#'
#' @param df
#' @param cols character vector of columns names to check if dates
#' @param stop_if_fail T/F for whether to consider failure an error
#' @param return_df T/F whether to end function with dataframe input (as in if a check in part of a pipe)
#'
#' @return
#' @export
#'
#' @examples
expect_date <- function(df, cols, stop_if_fail = TRUE, return_df = TRUE){
  if(return_df){
    df_copy_for_later <- df
  }

  if (!("data.frame" %in% class(df)) & is.vector(df)){
    df <- data.frame("vector" = df)
    cols <- "vector"
  }

  # if any not found, then return error
  for(co in cols){
    if (sum(colnames(df) %in% co) == 0) {
      # -i: would like to return all the columns not found, not just the first
      stop(paste0("No column named: ", co))
    }
  }

  dfdates <- sapply(df[cols], function(x) is.Date(x))
  if(sum(dfdates) == length(cols)){
    print(paste0("all columns are dates...OK"))
  } else if(sum(dfdates) < length(cols)){
    ifelse(stop_if_fail,
           stop(paste0("Column is not a date: ", names(which(!dfdates)))),
           warning(paste0("Column is not a date: ", names(which(!dfdates)))))
  }

  if(return_df){
    df_copy_for_later
  }
}


#' Check number of rows and columns matches expectation of range
#'
#' @param df data frame to check. If a vector is given, it converts it into a one column dataframe to check length as rows
#' @param min_nrow Integer of inclusive minimum OK
#' @param max_nrow Integer of inclusive maximum OK. Default is same as minimum
#' @param stop_if_fail T/F for whether to consider failure an error
#' @param return_df T/F whether to end function with dataframe input (as in if a check in part of a pipe)
#'
#' @return
#' @export
#'
#' @examples
expect_dimensions_between <- function(df, min_nrow = 0, max_nrow = Inf, min_cols = 0, max_cols = Inf,
                                      stop_if_fail = TRUE, return_df = TRUE){
  if(return_df){
    df_copy_for_later <- df
  }

  if (!("data.frame" %in% class(df)) & is.vector(df)){
    df <- data.frame("vector" = df)
    cols <- "vector"
  }

  nrows <- nrow(df)
  ncols <- ncol(df)

  if(min_nrow == 0 & max_nrow == Inf & min_cols == 0 & max_cols == Inf){
    # check that at least one dimension is requested
    stop("No test dimensions given")
  } else if(min_cols == 0 & max_cols == Inf){
    # check rows only
    if(nrows >= min_nrow & nrows <= max_nrow){
      print(paste0("Number of rows is ", nrows, "...OK"))
    } else {
      ifelse(stop_if_fail,
             stop(paste0("Number of rows is ", nrows, " not in range given of [", min_nrow, ", ", max_nrow, "]")),
             warning(paste0("Number of rows is ", nrows, " not in range given of [", min_nrow, ", ", max_nrow, "]")))
    }
  } else if(min_nrow == 0 & max_nrow == Inf){
    #check columns only
    if(ncols >= min_cols & ncols <= max_cols){
      print(paste0("Number of cols is ", ncols, "...OK"))
    } else {
      ifelse(stop_if_fail,
             stop(paste0("Number of cols is ", ncols, " not in range given of [", min_cols, ", ", max_cols, "]")),
             warning(paste0("Number of cols is ", ncols, " not in range given of [", min_cols, ", ", max_cols, "]")))
    }
  } else {
    #check both rows and cols
    if(nrows >= min_nrow & nrows <= max_nrow & ncols >= min_cols & ncols <= max_cols){
      print(paste0("Number of rows is ", nrows, "and number of cols is ", ncols, "...OK"))
    } else {
      ifelse(stop_if_fail,
             stop(paste0("Number of rows is ", nrows, "and number of cols is ", ncols,
                         " one is not in range given of rows: [", min_nrow, ", ", max_nrow, "]",
                         "cols: [", min_cols, ", ", max_cols, "]")),
             warning(paste0("Number of rows is ", nrows, "and number of cols is ", ncols,
                            " one is not in range given of rows: [", min_nrow, ", ", max_nrow, "]",
                            "cols: [", min_cols, ", ", max_cols, "]")))
    }
  }

  if(return_df){
    df_copy_for_later
  }

}
