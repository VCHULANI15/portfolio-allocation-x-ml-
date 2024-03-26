require(quantmod)
require(googledrive)

# Ensure "PortfolioOptimization" folder exists in Google Drive
ensure_drive_folder_exists <- function(folder_name) {
  folder <- drive_ls(pattern = folder_name, type = "folder")
  if (length(folder$name) == 0) {
    # Folder does not exist, create it
    drive_mkdir(folder_name)
    cat("Folder '", folder_name, "' created in Google Drive.\n")
  } else {
    cat("Folder '", folder_name, "' already exists in Google Drive.\n")
  }
}

fetch_stock_data <- function(stock_list, start_date, end_date, verbose = FALSE) {
  master_df <- NULL
  for (idx in seq(length(stock_list))) {
    stock_index = stock_list[idx]
    if (verbose) {
      cat("Fetching data for:", stock_index, "\n")
    }
    # Error handling to continue in case of an error
    tryCatch({
      getSymbols(stock_index, verbose = verbose, src = "yahoo", from = start_date, to = end_date)
      temp_df = as.data.frame(get(stock_index))
      temp_df$Date = row.names(temp_df)
      temp_df$Index = stock_index
      row.names(temp_df) = NULL
      colnames(temp_df) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Date", "Index")
      temp_df = temp_df[c("Date", "Index", "Open", "High", "Low", "Close", "Volume", "Adjusted")]
      master_df <- rbind(master_df, temp_df)
    }, error = function(e) {
      if (verbose) {
        cat("Failed to fetch data for:", stock_index, "\n")
      }
    })
  }
  return(master_df)
}

save_to_csv_google_drive <- function(df, file_name) {
  # Ensure the "PortfolioOptimization" folder exists
  ensure_drive_folder_exists("PortfolioOptimization")
  
  # Define the path in Google Drive
  drive_path <- paste0("PortfolioOptimization/", file_name)
  temp_file <- tempfile(fileext = ".csv")
  write.csv(df, temp_file, row.names = FALSE)
  
  # Upload the file to Google Drive
  drive_upload(temp_file, path = drive_path, overwrite = TRUE)
  cat("Data saved to Google Drive:", drive_path, "\n")
}


run_stock_data_fetch <- function(stock_list, start_date, end_date, save_csv = TRUE, verbose = FALSE) {
  master_df <- fetch_stock_data(stock_list, start_date, end_date, verbose)
  if (save_csv) {
    file_name <- paste0("StockDataa_", Sys.Date(), ".csv")
    save_to_csv_google_drive(master_df, file_name)
  }
  return(master_df)
}


stock_list <- c("AMZN", "TSLA", "NFLX", "ADDYY", "NKE", "PG", "KO", "PEP", "WMT", "MCD", "NEE", "ENPH", "SEDG", "FSLR", "PLUG", "XOM", "CVX", 
                "BP", "RDS.A", "TTE", "AAPL", "GOOGL", "META", "CRM", "NFLX", "IBM", "CSCO", "VZ", "T", "CMCSA", "MRNA", "BNTX", "ILMN", "VRTX", 
                "REGN", "JNJ", "PFE", "MRK", "NVS", "BMY", "SQ", "PYPL", "GS", "MS", "SCHW", "JPM", "BAC", "WFC", "C", "HSBC")
start_date <- Sys.Date() - (365*10) 
end_date <- Sys.Date()

stock_data <- run_stock_data_fetch(stock_list, start_date, end_date, save_csv = TRUE, verbose = TRUE)