# This function is called from determineAcceptableRangesForAttributes.Rmd and
# from isSubmissionAcceptable.
# It returns a dataframe of the number of non NAs for each attribute.
processASubmissionFile = function(DBTableName) {
  #Read some data from database.
  
  tryCatch({
    message("Read a submission table from database.  Takes about 90 seconds per table")
    
    attribData = dbReadTable(con, DBI::Id(schema = "terminal", table = DBTableName))
    message("Reading data from DB.")
  }, error=function(cond) {
    message("Reading data failed.")
    message("Code stopped.  Does the table exist in SQL?")
    message(cond)
    return(NA)
  }, finally={
    head(attribData)
    message(paste("Data table has", nrow(attribData), "rows and", ncol(attribData), "columns"))
  })
  
  #df <- data.frame(matrix(ncol = 3, nrow = 0))
  #x <- c("name", "age", "gender")
  #colnames(df) <- x
  
  # We count the number of entries in each column that are not null.
  message("Determining the number of entries for each attribute.  Takes about 60 seconds per DB table.")
  countNonNAs = (apply(attribData, 2, function(x) length(which(!is.na(x))))) # Takes 90 seconds
  #countNonNAs_df = data.frame(matrix(ncol=2, nrow=length(countNonNAs)))
  #n = length(countNonNAs)
  #countNonNAs_df = data.frame(Attributes = character(), temp = integer(), stringsAsFactors = FALSE)
  countNonNAs_df = data.frame(matrix(ncol=2, nrow=length(countNonNAs)))
  x = c("Attributes", DBTableName)
  colnames(countNonNAs_df) <- x
  countNonNAs_df$Attributes = names(countNonNAs)
  countNonNAs_df[DBTableName] = (countNonNAs)
  #countNonNAs_df[DBTableName] = (apply(attribData, 2, function(x) length(which(!is.na(x))))) # Takes 90 seconds
  # Add the number of rows
  #countNonNAs_df = rbind(countNonNAs_df, c("TOTAL_ROWS", nrow(attribData)))
  
  # Return a data frame with two columns.  First is "Attributes" and second is a count of the number of non-NAs.
  return(countNonNAs_df)
}