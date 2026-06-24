# Load necessary library
  library(gmailr)
  library(tidyverse)
  library(purrr)
  library(magrittr)
  library(base64enc)
  library(base64url)
  library(rvest)
  library(xml2)
  library(stringr)
  library(stringdist)
  library(ausPH) 
  library(dplyr)
  library(stringi)
  library(gargle)
  library(googleCloudStorageR)
  
  ############# AUTHENTICATION ##################################
  
  #Gmail
  path_gm_json <- Sys.getenv("GM_CLIENT_JSON")
  gm_auth_configure(path = path_gm_json)
  gm_auth(email = "email@gmail.com")
  
  # Google Storage
  path_json <- Sys.getenv("GAR_CLIENT_JSON")
  gcs_auth(path_json)
  
  
  ############# FUNCTIONS ##################################
  
  ####### Function to decode base64 URL safe encoding
  decode_base64_url <- function(encoded_string) {
    # Convert the URL-safe base64 string to standard base64
    encoded_string <- gsub("-", "+", encoded_string)
    encoded_string <- gsub("_", "/", encoded_string)
    # Add necessary padding
    encoded_string <- paste0(encoded_string, strrep("=", (4 - nchar(encoded_string) %% 4) %% 4))
    # Decode the base64 string
    rawToChar(base64decode(encoded_string))
  }
  
  ####### Save the HTML content to a file
  save_html <- function(content,message_id){
    
    # Create a temporary file to hold the content before uploading
    temp_file <- tempfile(fileext = ".html")
    writeLines(content, con = temp_file)
    
    # Define the object path in the bucket
    object_name <- paste0(message_id, ".html")
    
    # Upload the file to Google Cloud Storage
    gcs_upload(temp_file, 
               bucket = "canberra-inbox", 
               name = object_name,
               predefinedAcl='publicRead'
               )
    
    return(content)
  }
  
  # Function to save data frame as CSV to Google Cloud Storage
  save_csv_to_gcs <- function(df, name_file_csv){
    # Create a temporary file to hold the CSV
    temp_file <- tempfile(fileext = ".csv")
    write.csv(df, temp_file, row.names = FALSE, fileEncoding = "UTF-8")
    
    # Upload the CSV to Google Cloud Storage
    gcs_upload(temp_file, bucket = "inbox-other", name = name_file_csv)
  }
  
 # Function to clean HTML content
  cleaned_email_html_vectorized <- function(html_content) {
    # Convert encoding
    html_content <- iconv(html_content, from = "UTF-8", to = "UTF-8")
    # Add space after HTML tags
    html_string <- gsub("(<[^>]+>)", "\\1 ", html_content)
    # Remove <style> tags and their content
    html_content_cleaned <- gsub("<style\\b[^<]*</style>", "", html_string, ignore.case = TRUE)
    # Parse HTML content
    html_content_modified <- read_html(html_content_cleaned) %>%
      html_nodes("body") %>% 
      html_text(trim = TRUE)
    # Remove specific Unicode characters
    html_content_modified <- gsub("\u200C|\U0001F423|\u034F|\U0001F49C|\u274C|\u2714|\uFE0F|\U0001F447", "", html_content_modified)
    # Remove multiple spaces
    html_content_modified <- str_squish(html_content_modified)
    
    return(html_content_modified)
  }
  
  # Function to fin html
  find_html_body <- function(parts) {
    for (part in parts) {
      if (!is.null(part$mimeType) && part$mimeType == "text/html") {
        return(part$body$data)
      } else if (!is.null(part$parts)) {
        html_body <- find_html_body(part$parts)
        if (!is.null(html_body)) {return(html_body)}
      }
    }
    return("NoHTML")
  }
  
  find_plan_body <- function(parts) {
    for (part in parts) {
      if (!is.null(part$mimeType) && part$mimeType == "text/plain") {
        return(part$body$data)
      } else if (!is.null(part$parts)) {
        plan_body <- find_plan_body(part$parts)
        if (!is.null(plan_body)) {return(plan_body)}
      }
    }
    return("NoPlain")
  }
  
  # Initialize an empty data frame to store the message details
  message_details <- data.frame(
    message_id = character(),
    mesage_date = character(),
    message_hour = character(),
    message_sender = character(),
    message_subject = character(),
    message_body = character(),
    message_html_link = character(),
    massage_image_directory = character(),
    stringsAsFactors = FALSE
  )
  
  ################################## MAIN RETRIEVE THE EMAILS (SAVE HTML)####################################################
  # Search criteria (excluding both Discarded and Retrieved labels)
  #search_query <- 'in:inbox -label:DISCARDED -label:RETRIEVED'
  #search_query <- 'in:inbox -label:DISCARDED'
  #search_query <- 'in:inbox -label:DISCARDED -label:RETRIEVED after:2024/09/29 before:2024/10/02'
  search_query <- 'in:inbox -label:DISCARDED -label:RETRIEVED'
  # Get messages (maximum 500)
  messages <- gm_messages(num_results = 500, search = search_query)
  #messages <- gm_messages(num_results = 500, search = 'in:inbox -label:DISCARDED')
  
  for (j in 1:length(messages)){
    for (i in seq_along(messages[[j]]$messages)) {
      # Get the message ID
      m <-messages[[j]]$messages[[i]]
      message_id <- m$id
      cat("message_id: ",message_id, "\n")
      # Get the full message
      full_message <- gm_message(message_id, format = "full")
      
      # Extract headers
      headers <- full_message$payload$headers
      date_time <- NULL
      sender <- NULL
      subject <- NULL
      
      for (header in headers) {
        if (header$name == "Date") {
          date_time <- header$value
        } else if (header$name == "From") {
          sender <- header$value
        } else if (header$name == "Subject") {
          subject <- header$value
        }
      }
      
      # Split the date and time
      date <- format(as.POSIXct(date_time, format="%a, %d %b %Y %H:%M:%S %z"), "%d/%m/%Y")
      hour <- format(as.POSIXct(date_time, format="%a, %d %b %Y %H:%M:%S %z"), "%H:%M:%S")
      cleaned_text<- ""
      
      # Extract the parts of the message
      parts <- full_message$payload$parts
      
      # Find the part with MIME type
      if (!is.null(parts)){
        html_content <- find_html_body(parts)
        html_content <- decode_base64_url(html_content)
        html_content <- save_html(html_content,message_id)
        cleaned_text <- html_content
        in_parts <- FALSE
        if (cleaned_text=="NoHTML"){
          plan_content<- find_plan_body(parts)
          email_content <- rawToChar(base64decode(plan_content))
          html_content_decoded <- paste("<html><body><pre>", email_content, "</pre></body></html>")
          save_html(html_content_decoded,message_id)
          cleaned_text <- html_content_decoded
        }
      }else {
        if (full_message$payload$mimeType == "text/html") {
          html_content <- full_message$payload$body$data
          html_content_decoded <- decode_base64_url(html_content)
          html_content_decoded <- save_html(html_content_decoded, message_id)
          cleaned_text <- html_content_decoded
        }else if ((full_message$payload$mimeType == "text/plain")){
          email_content <- rawToChar(base64decode(full_message$payload$body$data))
          html_content_decoded <- paste("<html><body><pre>", email_content, "</pre></body></html>")
          save_html(html_content_decoded, message_id)
          cleaned_text <- html_content_decoded
        }else{
          cleaned_text <- "No find"
        }
      }
      
      html_file_path <- paste0(message_id, ".html")
      image_directory <- paste0("images/", message_id)
      
      # Append the details to the data frame
      message_details <- rbind(message_details, data.frame(
        message_id = message_id,
        message_date = date,
        message_hour = hour,
        message_sender = sender,
        message_subject = subject,
        message_body = cleaned_text,
        message_html_link = html_file_path,
        massage_image_directory = image_directory,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Extract names and emails
  message_details$senderName <- sub("^(.*)\\s*<.*>$", "\\1", message_details$message_sender)
  message_details$email <- sub("^.*<([^>]*)>.*$", "\\1", message_details$message_sender)
  
  # Clean up quotes in names
  message_details$senderName <- gsub('\"', '', message_details$senderName)
  
  # Apply the function to the entire column using vectorized operations
  message_details <- message_details %>%
    mutate(message_body = sapply(message_body, cleaned_email_html_vectorized))
  
  
##########___ GET THE PARLAMENTARIANS FROM getParlService AND EDIT FOR COMPARISON _________#############

current_parlamentarians <- subset(getParlService(chamber = "all"), is.na(DateEnd))

current_parlamentarians_party <- subset(getPartyService(), is.na(DateEnd))

parlamentarians_p <- current_parlamentarians_party %>%
  left_join(current_parlamentarians, by = "PHID") %>%
  select(PHID,DisplayName.x,MemberOrSenator,Electorate,DateStart.x,Description,Party,PartyColour,Description)

# Perform the join operation
parlamentarians <- current_parlamentarians %>%
  left_join(current_parlamentarians_party, by = "PHID") %>%
  select(PHID,DisplayName.x,MemberOrSenator,Electorate,DateStart.x,Description,Party,PartyColour)

parlamentarians <- parlamentarians %>% rename(SurnameName = DisplayName.x,
                                              DateStart = DateStart.x)


# Function to split and merge the DisplayName in reverse order
split_and_merge <- function(name) {
  parts <- strsplit(name, ",\\s*", fixed = FALSE)[[1]]
  if (length(parts) == 2) {
    reversed_name <- paste(parts[2], parts[1])
  } else {
    reversed_name <- name
  }
  return(trimws(reversed_name))
}

# Apply the function to create a new column and trim whitespace
parlamentarians$Name <- sapply(parlamentarians$SurnameName, split_and_merge)

# Function to modify the Full_Name
modify_names <- function(name) {
  if (grepl("\\(.*\\)", name)) {
    # Extract the part before the brackets and the part after the brackets
    part_after_brackets <- sub(".*\\)", "", name)
    
    # Extract the name inside the brackets
    name_inside_brackets <- gsub(".*\\(([^)]+)\\).*", "\\1", name)
    
    # Combine the name inside brackets with the part after the brackets
    modified_name <- paste(name_inside_brackets, part_after_brackets)
    
    # Remove any extra spaces
    modified_name <- gsub("\\s+", " ", modified_name) # Ensure only one whitespace between words
    return(modified_name)
  }else{
    return(name)
  }
}

# Apply the function to the Full_Name column
parlamentarians <- parlamentarians %>%
  mutate(Name = sapply(Name, modify_names))

# As the scope does not consider the dataset parlament, add for default this add the current parlament
parlamentarians <- parlamentarians %>%
  mutate(parliament = 48)
#note: I moved to 48 for all e-newsletters from the day after the election (i.e. from 4 May 2025)

save_csv_to_gcs(parlamentarians, "parlamentarians_dataset.csv")

###############_______________ MERGIN THE THE DATASET ____________________##############################

clean_for_comparison <- function(column_name, df) {
  df[[column_name]] <- tolower(df[[column_name]])  # Convert all names in specified column to lowercase
  common_words <- "\\b(senator|mp|prime minister|office|no reply|australia|the hon|CSC|federal|member|for|team|dr)\\b"
  df[[column_name]] <- gsub("[,\\.()\"]", "", df[[column_name]])
  df[[column_name]] <- gsub(common_words, "", df[[column_name]], ignore.case = TRUE)
  df[[column_name]] <- trimws(df[[column_name]])
  return(df)
}

# Use parlamentarians dataset from the previous process
parlamentarians_dataset<- parlamentarians
parlamentarians_dataset<-clean_for_comparison("Name", parlamentarians_dataset) 

messages_retrived <- message_details
messages_retrived <- clean_for_comparison("senderName", messages_retrived)

#### Manage special cases:
messages_retrived <- messages_retrived %>%
  mutate(senderName = case_when(
    senderName == "david smith" ~ "david philip benedict smith",
    senderName == "andrew wilkie" ~ "andrew damien wilkie",
    senderName == "pat conroy" ~ "patrick martin conroy",
    senderName == "linda reynolds" ~ "reynolds linda karen",
    senderName == "zoe mckenzie" ~ "zoe anne mckenzie",
    senderName == "garland carina" ~ "carina garland",
    senderName == "zali steggall" ~ "steggall zali oam",
    senderName == "let's talk about it - babet" ~ "babet ralph",
    senderName == "dean smith" ~ "smith dean anthony",
    TRUE ~ senderName  # Retain original value if no match
  ))


#################______ PAIR COMPARISON USING Jaro-Winkler _________________##########

find_best_match <- function(name, df1) {
  similarities <- sapply(df1$Name, function(df1_name) stringdist::stringdist(df1_name, name, method = "jw"))
  max_index <- which.min(similarities)  # Jaro-Winkler similarity is a distance metric, so lower is better
  return(list(PHID = df1$PHID[max_index], similarity = 1 - similarities[max_index], p_Name = df1$Name[max_index]))
}

# Apply the function to each row in message_details
matches <- lapply(messages_retrived$senderName, find_best_match, df1 = parlamentarians_dataset)

# Extract phid, similarity, and corresponding_name into separate vectors
messages_retrived$PHID <- sapply(matches, function(match) match$PHID)
messages_retrived$similarity <- sapply(matches, function(match) match$similarity)
messages_retrived$Name <- sapply(matches, function(match) match$p_Name)
messages_retrived$Ground_truth <- ""

# Save the data frame MESSAAGE_DETAIL_TEMP to a CSV file FINAL OUTPUT in Google Storage
save_csv_to_gcs(messages_retrived, "CBR_Inbox_TEM.csv")
