# File contains proprietary functions for app.


#---------------------------
# ReadData Function
#---------------------------

# Data Reading Function; Determines file extension and reads based on extension 
# (currently only csv, xpt, xlsx, sas7bdat)
ReadData <- function(dataFile, header, sheet){
  if (tools::file_ext(dataFile) == "csv") {
    data <- read_csv(dataFile,
             col_names = header)
  } 
  else if (tools::file_ext(dataFile) == "xpt") {
    data <- read_xpt(dataFile)
  } 
  else if (tools::file_ext(dataFile) == "sas7bdat") {
    data <- read_sas(dataFile)
  } 
  else if (tools::file_ext(dataFile) == "xlsx") {
    data <- read_xlsx(dataFile,
              sheet = sheet,
              col_names = header)
  }
  return(data)
}

# Returns all cols (variables) in data with more then x% missing-ness
NA_cols <- function(df, na_percent) {
  df <- df %>%
    summarise_all(~ mean(is.na(.))) %>%
    gather(key = "variable", value = "na_percentage") %>%
    filter(na_percentage > na_percent) %>%
    pull(variable)
  return(df)
}


# Returns all rows (observations) in data with more then x% missing-ness
NA_rows <- function(df, na_percent) {
  df %>%
    filter(rowSums(is.na(df)) / ncol(df) > na_percent)
}


#---------------------------
# NAOptions Function
#---------------------------

# Function handles NA's based on user preference without all the extra knowledge

NAOptions <- function(Data, 
                      Method = c("Ignore", "Complete_Cases", "NaiveMean", "Group_Mean", "Multiple_Imputation"), 
                      Group = NA,
                      impute_method = "pmm") {
  
  # Match the selected method to one of the valid choices
  Method <- match.arg(Method)
  
  if (Method == "Ignore") {
    return(Data)
    
  } else if (Method == "Complete_Cases") {
    # Keep only rows without any NA values
    Data <- Data %>% 
      filter(complete.cases(.))
    
  } else if (Method == "NaiveMean") {
    # For each numeric column, replace NAs with the overall mean of that column
    numeric_cols <- names(dplyr::select_if(Data, is.numeric))
    Data <- Data %>%
      mutate(across(all_of(numeric_cols), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
    
  } else if (Method == "Group_Mean") {
    # Group by the specified grouping variable and fill NAs with group means for numeric columns
    numeric_cols <- names(dplyr::select_if(Data, is.numeric))
    Data <- Data %>%
      group_by(.data[[Group]]) %>%
      mutate(across(all_of(numeric_cols), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
      ungroup()
    
  } else if (Method == "Multiple_Imputation") {
    # Perform multiple imputation using the mice package
    # By default, we can do a simple example with predictive mean matching (pmm)
    imputed_data <- mice(Data, m = 1, maxit = 1, method = impute_method, seed = 91)
    
    # Return one completed dataset (e.g., the first one)
    Data <- complete(imputed_data, 1)
  }
  
  return(Data)
}


#---------------------------
# ChangeVarType Function
#---------------------------

# Changes the type or format of a variable (as....) but has
# additional params and allows simpler and cleaner usage

ChangeVarType <- function(Data, 
                          Var, 
                          Type = c("Chr", "Num", "Fac", "Date", "NA"), 
                          Labels = NA,
                          DateFormat = "%Y-%m-%d") {
  
  # Convert based on the chosen Type
  if (Type == "Chr") {
    
    Data <- Data %>%
      mutate("{Var}" := as.character(.data[[Var]]))
    
  } else if (Type == "Num") {
    # Convert to numeric
    Data <- Data %>%
      mutate("{Var}" := as.numeric(as.character(.data[[Var]])))
    
  } else if (Type == "Fac") {
    # Convert to factor, using specified Levels if given
    if (!all(is.na(Labels))) {
      Data <- Data %>%
        mutate("{Var}" := factor(.data[[Var]], labels = Labels))
    } else {
      Data <- Data %>%
        mutate("{Var}" := as.factor(.data[[Var]]))
    }
    
  } else if (Type == "Date") {
    # Convert to Date (with a user-specified format)
    Data <- Data %>%
      mutate("{Var}" := as.Date(.data[[Var]], format = DateFormat))
  }
  
  return(Data)
}






