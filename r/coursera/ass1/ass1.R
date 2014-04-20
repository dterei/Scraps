pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  # create an empty vector to store all values from the specified column. This
  # isn't very efficient, we should be able to calculate the mean incrementally
  # but I don't know how NA values are meant to be handled...
  all <- c()

  # loop over all specified file ID's.
  for (file in id) {
    # turn a file ID (i.e., 2) into a file name (002.csv)
    file <- sprintf("%03d.csv", file)

    # turn a file name (i.e., 002.csv) into a file path (specdata/002.csv)
    path <- file.path(directory, file)

    # read the file as a CSV
    data <- read.csv(path)

    # extract out the column we want from the data. The format is
    # `data[<row>,<column>]` and if we only specify one of <row> or <column>
    # then we grab all data associated with that row or column. So here we only
    # specify the column so we get all fields in that column
    column <- data[,pollutant]

    # combine this files data with previous files data
    all  <- c(all,column)
  }

  # compute the mean across all data ignoring NA (not-a-number [i.e., 1/0 =
  # NA]) values. Return it as our result.
  mean(all, na.rm = TRUE)
}

