complete <- function(directory, id = 1:332) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  files_full <- list.files(directory, full.names=TRUE)  #creates a list of files
  dat <- data.frame()  #creates an empty dataframe for csv files
  dat2 <- data.frame()  #creates an empty dataframe for current csv file
  nobs <- data.frame()  #creates an empty dataframe for all complete observations
  nobs2 <- data.frame()  #creates an empty dataframe for current complete observations
  for (i in id) {
    #loops trhough the files, rbinding them together
    dat2 <-  read.csv(files_full[i]) #reads the csv with the current value in "id"
    dat <- rbind(dat, dat2) #rbinds the current csv with the previous csv
    removeNA <- dat2[complete.cases(dat2),] #removes rows with NA
    completenobs <- nrow(removeNA) # counts the rows of complete observations
    nobs2 <- c(i, completenobs) # fills the variable with current csv complete observations
    nobs <- rbind(nobs, nobs2) #adds current csv's observation with previous csv's observations
  }
  names(nobs) <- c("ID", "nobs") #names the columns
  print(nobs) #prints the results
}
