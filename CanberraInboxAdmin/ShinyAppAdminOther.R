library(shiny)
library(shinythemes)
library(shinyjs)
library(DT)                    
library(dplyr)                 
library(readr)                
library(stringr) 
library(quanteda)
library(gargle)
library(gmailr)
library(googleCloudStorageR)
library(shinycssloaders)

############# AUTHENTICATION ##################################

#App authentication
valid_username <- Sys.getenv("USER")
valid_password <- Sys.getenv("PASSWORD")
# Authenticated status
is_authenticated <- reactiveVal(FALSE)
is_inboxChose <- reactiveVal(FALSE)




#################_____ FUNCTIONS ________###################
truncate_text <- function(text, word_limit = 10) {
  words <- str_split(text, "\\s+")[[1]]  # Split the text into words
  truncated <- paste(head(words, word_limit), collapse = " ")  # Take the first 'word_limit' words
  return(truncated)
}

# Update the email's label to downloaded
update_labels <- function(message_ids){
  for (ms_id in message_ids) {
    #cat("id",ms_id)
    gmailr::gm_modify_message( id = ms_id, add_labels = "Label_4013903200620411993" )
    
  }
}

# update_labels <- function(message_ids) {
#   lapply(message_ids, function(ms_id) {
#     gm_modify_message(id = ms_id, add_labels = "Label_4013903200620411993")#"Label_4013903200620411993"
#   })
# }

# Download files from Google Cloud Storage
download_file_from_gcs <- function(bucket_name, object_name, dest_path) {
  gcs_get_object(object_name, bucket = bucket_name, saveToDisk = dest_path, overwrite = TRUE)
}


downloadInboxFile  <- function(inboxVariables, file_suffix, destination_folder, extension) {
  if (!is.null(inboxVariables)) {
    # Construct dynamic arguments
    bucket_name <- paste0(inboxVariables$iinbox_name, "-inbox-other")
    source_file <- paste0(inboxVariables$prefix_upper, "_", file_suffix, extension)
    destination_path <- paste0(destination_folder, "/", inboxVariables$prefix_upper, "_", file_suffix, extension)
    
    # Call the download function
    download_file_from_gcs(bucket_name, source_file, destination_path)
  } else {
    showModal(modalDialog(
      title = "Error",
      "No inbox selected. Please choose an inbox first.",
      easyClose = TRUE
    ))
  }
}

# Upload files to Google Cloud Storage
upload_file_to_gcs <- function(bucket_name, local_path, object_name) {
  gcs_upload(local_path, bucket = bucket_name, name = object_name)
}

# Excel download handler

xlsx.writeMultipleData <- function(file, ...) {
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1) {
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    } else {
      write.xlsx(objects[[i]], file, sheetName = objnames[i], append = TRUE)
    }
  }
}

# Metadata columns for the corpus
metadata_columns <- c("PHID", "SurnameName", "MemberOrSenator", "Electorate",
                      "Party", "parliament", "message_id", "message_date",
                      "message_hour", "message_sender", "message_subject",
                      "message_body", "message_html_link", "massage_image_directory")
metadata_columns_w <- c("MemberID", "SurnameName", "Electorate",
                      "Party", "parliament", "message_id", "message_date",
                      "message_hour", "message_sender", "message_subject",
                      "message_body", "message_html_link", "massage_image_directory")
metadata_columns_o <- c("person_id", "SurnameName", "MemberOrSenator", "Riding_Senatorial_Division",
                      "Party", "parliament", "message_id", "message_date",
                      "message_hour", "message_sender", "message_subject",
                      "message_body", "message_html_link", "massage_image_directory")

#############_____________APP ________#####################

ui <- fluidPage(
  useShinyjs(),  # Use shinyjs for enabling/disabling buttons
  tags$head(
    tags$script(HTML("function scrollToTop() {
      window.scrollTo(0, 0);
    }"))
  ),
  titlePanel("CanberraInbox Admin"),
  theme = shinytheme("cerulean"),  # Apply Cerulean theme
  
  # Login Panel
  conditionalPanel(
    condition = "output.isLoggedIn == false",
    fluidPage(
      h2("Login"),
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      actionButton("login", "Login"),
      uiOutput("login_status")
    )
  ),
  
  
  # Choose Inbox Panel
  conditionalPanel(
    condition = "output.isLoggedIn == true && !output.inboxSelected",
    fluidPage(
      h2("Choose Inbox"),
      actionButton("canberra", "Canberra"),
      actionButton("ottawa", "Ottawa"),
      actionButton("wellington", "Wellington")
    )
  ),
  
  # Main App Panel
  conditionalPanel(
    condition = "output.isLoggedIn == true && output.inboxSelected",
    tabPanel("Admin",
           fluidPage(
             fluidRow(
               column(3, actionButton("retrieve", "Retrieve Newsletters")),
               column(3, uiOutput("status"), downloadButton("download_csv", "Download CSV"),),
               column(3, uiOutput("status"), downloadButton("download_excel", "Download Excel"),),
               column(3, actionButton("create_update_corpus", "Update Corpus")),
               column(12, actionButton("logout", "Logout", class = "btn btn-danger"))
             ),
             br(),
             
             fluidRow(
               column(12,withSpinner(DTOutput("email_table"))),
               column(12,actionButton("approve", "Approve data"))
             ),
            br()
           )
    )
  ),
  
  # Output for login status
  tags$head(
    tags$script(HTML("function scrollToTop() {
      window.scrollTo(0, 0);
    }"))
  )
  
)

# try(
#   gm_auth(token = gm_token_read(
#     ".secrets/gmailr-token.rds",
#     key = Sys.getenv("GMAILR_KEY")
#   ))
# )


server <- function(input, output, session) {
  # Reactive values to track state
  rv <- reactiveValues(
    isLoggedIn = FALSE,
    inboxChosen = NULL,
    inboxVariables = list() # To store the variables dynamically
  )
  
  # Define output for login status
  output$isLoggedIn <- reactive({ is_authenticated() })
  outputOptions(output, "isLoggedIn", suspendWhenHidden = FALSE)
  
  # Reactive Output for inboxSelected
  output$inboxSelected <- reactive({
    !is.null(rv$inboxChosen)  # TRUE if an inbox has been selected
  })
  outputOptions(output, "inboxSelected", suspendWhenHidden = FALSE)
  
  # Inbox settings table
  inbox_settings <- list(
    Canberra = list(
      iinbox_name = "canberra",
      prefix_lower = "cbr",
      prefix_upper = "CBR",
      id_label_inbox = "Label_4013903200620411993",
      id_label_retrieved = "Label_3037043592402205091",
      id_label_discard ="Label_5527042881718374213"
    ),
    Ottawa = list(
      iinbox_name = "ottawa",
      prefix_lower = "otw",
      prefix_upper = "OTW",
      id_label_inbox = "Label_1486984644930214210",
      id_label_retrieved = "Label_3010547458625304725",
      id_label_discard ="Label_8600256955940380876"
    ),
    Wellington = list(
      iinbox_name = "wellingtonnz",
      prefix_lower = "wlg",
      prefix_upper = "WLG",
      id_label_inbox = "Label_5009743614736854962",
      id_label_retrieved = "Label_7990110882756903129",
      id_label_discard ="Label_3093819538298751041"
    )
  )
  
  # Login button functionality
  observeEvent(input$login, {
    if (input$username == valid_username && input$password == valid_password) {
      is_authenticated(TRUE)
      output$login_status <- renderUI({ HTML("<strong style='color: green;'>Login successful!</strong>") })
      shinyjs::hide("username")
      shinyjs::hide("password")
      shinyjs::hide("login")
    } else {
      output$login_status <- renderUI({ HTML("<strong style='color: red;'>Invalid username or password.</strong>") })
    }
  })
  
  
  # Inbox selection logic
  observeEvent(input$canberra, {
    update_inbox_variables("Canberra")
  })
  
  observeEvent(input$ottawa, {
    update_inbox_variables("Ottawa")
  })
  
  observeEvent(input$wellington, {
    update_inbox_variables("Wellington")
  })
  
  # Function to update variables dynamically
  update_inbox_variables <- function(inbox) {
    rv$inboxChosen <- inbox
    rv$inboxVariables <- inbox_settings[[inbox]]
    authenticate_gmail_and_storage(inbox)
    
    showModal(modalDialog(
      title = paste("Selected Inbox:", rv$inboxVariables$iinbox_name),
      paste("Authentication Successful: You are now authenticated for the inbox.",rv$inboxVariables$iinbox_name),
      easyClose = TRUE
    ))
  }
  
  # Gmail and Google Storage Authentication
  authenticate_gmail_and_storage <- function(inbox) {
    if (inbox == "Canberra") {
      gm_auth_configure(path = "./credentials_gmail_desktop.json")
      options(
        gargle_verbosity = "debug",
        gargle_oauth_cache = ".secret",
        gargle_oauth_email = "canberra.inbox@gmail.com"
      )
      gm_auth(email = "canberra.inbox@gmail.com")
      path_json <- Sys.getenv("GAR_CLIENT_JSON")
      gcs_auth(path_json)
    } else if (inbox == "Ottawa") {
      gm_auth_configure(path = "./ottawa_credentials_gmail.json")
      options(
        gargle_verbosity = "debug",
        gargle_oauth_cache = ".secretottawa",
        gargle_oauth_email = "inbox.ottawa@gmail.com"
      )
      gm_auth(email = "inbox.ottawa@gmail.com")
      path_json <- Sys.getenv("GAR_CLIENT_JSON_OTTAWA")
      gcs_auth(path_json)
    } else if (inbox == "Wellington") {
      gm_auth_configure(path = "./wellingtonnz_credentials_gmail.json")
      options(
        gargle_verbosity = "debug",
        gargle_oauth_cache = ".secretWellingtonnz",
        gargle_oauth_email = "wellingtonnz.inbox@gmail.com"
      )
      gm_auth(email = "wellingtonnz.inbox@gmail.com")
      path_json <- Sys.getenv("GAR_CLIENT_JSON_WELLINGTONNZ")
      gcs_auth(path_json)
    }
    showModal(modalDialog(
      title = paste(inbox, "Authentication Successful"),
      "You are now authenticated for the selected inbox.",
      easyClose = TRUE
    ))
  }

  
  # Disable the buttons at the start
  shinyjs::disable("approve")
  shinyjs::disable("create_update_corpus")
  shinyjs::disable("download_csv")
  shinyjs::disable("download_excel")
  
  # Reactive value to store the dataset
  email_data <- reactiveVal(data.frame())
  
  # Track if approval is completed
  approval_completed <- reactiveVal(FALSE)
  
  observeEvent(input$retrieve, {
    output$status <- renderText("Retrieving newsletters... Please wait.")
    
    # Source the external file to retrieve newsletters
    withProgress(message = 'Retrieving newsletters...', value = 0, {
      # Source the external file to retrieve newsletters
      source_name <-paste0(rv$inboxVariables$iinbox_name, "_Rosa_Main_retrieve_email.R")
      source(source_name, local = TRUE)
      
      # Optionally update progress during retrieval
      incProgress(1, detail = "Processing complete")
    })
    
    # Update status
    output$status <- renderUI({
      HTML("<strong style='color: blue;'>", "Newsletters retrieved successfully.", "</strong>")
    })
    
    # Enable the Approve button after retrieving newsletters
    shinyjs::enable("approve")
    shinyjs::disable("retrieve")
    
    # Download CBR_Inbox_TEM.csv from Google Cloud Storage
    #download_file_from_gcs("canberra-inbox-other","CBR_Inbox_TEM.csv", "data/CBR_Inbox_TEM.csv")
    
    downloadInboxFile(rv$inboxVariables, "Inbox_TEM", "data", ".csv")
    
    # Read the CSV into a dataframe
    source_file <- paste0("data/",rv$inboxVariables$prefix_upper, "_Inbox_TEM.csv")
    cbr_inbox_temp <- read_csv(source_file)
    
    # Identify the column name dynamically
    id_column <- intersect(c("PHID", "MemberID", "person_id"), names(cbr_inbox_temp))
    
    if (length(id_column) != 1) {
      stop("None or multiple ID columns detected. Ensure one of 'PHID', 'MemberID', or 'person_id' is present.")
    }
    
    # Create a new column with truncated message body (first 10 words)
    cbr_inbox_temp <- cbr_inbox_temp %>%
      mutate(
        truncated_message_body = sapply(message_body, truncate_text),
        Ground_truth = ifelse(is.na(Ground_truth), "No", Ground_truth),  # Default 'No' if missing
        !!id_column := as.character(.data[[id_column]])  # Ensure PHID is character type
      ) 
    
    # Store the updated data in the reactive variable
    email_data(cbr_inbox_temp)
    
    # Render the table with editable columns: PHID and Ground_truth
    output$email_table <- renderDT({
      datatable(
        email_data() %>%
          select(message_id, message_date, message_subject, 
                 truncated_message_body, senderName, email, !!id_column, Name, Ground_truth),
        selection = "none", 
        editable = list(target = "cell", 
                        columns = c(6, 8)), 
        rownames = FALSE,
        options = list(
          pageLength = -1,  # Set to -1 to show all rows by default
          lengthMenu = list(c(-1, 10, 25, 50), c("All", "10", "25", "50")),
          columnDefs = list(
            list(
              targets = 6,   # Dynamically adjust for ID column
              render = JS(
                "function(data, type, row, meta) {",
                "  return '<input type=\"text\" onchange=\"Shiny.setInputValue(\\'email_table_cell_edit\\', {row: ' + (meta.row + 1) + ', col: 11, value: this.value}, {priority: \\'event\\'});\" value=\"' + data + '\">';",
                "}"
              )
            ),
            list(
              targets = 8,  # Ground_truth column
              render = JS(
                "function(data, type, row, meta) {",
                "  return '<select onchange=\"Shiny.setInputValue(\\'email_table_cell_edit\\', {row: ' + (meta.row + 1) + ', col: 14, value: this.value}, {priority: \\'event\\'});\">' +",
                "  '<option value=\"Yes\"' + (data == 'No' ? ' selected' : '') + '>No</option>' +",
                "  '<option value=\"Yes\"' + (data == 'Yes' ? ' selected' : '') + '>Yes</option>' +",
                "  '<option value=\"Discard\"' + (data == 'Discard' ? ' selected' : '') + '>Discard</option></select>';",
                "}"
              )
            )
          )
        )
    ) %>%
        formatStyle('Ground_truth', target = 'cell', 
                    color = styleEqual(c("Yes", "No", "Discard"), c("green", "red","red"))) 
    }, server = FALSE)
    
    
  })
  
  # Observe edits in the email_table
  observeEvent(input$email_table_cell_edit, {
    # Get the edited info from the table
    info <- input$email_table_cell_edit
    #str(info)  # Debugging: View the structure of the input
    
    # Update the reactive email_data with the new values
    updated_data <- email_data()
    
    updated_data[info$row, info$col] <- info$value
    email_data(updated_data)
  })
  
  # Approve button functionality
  observeEvent(input$approve, {
    # Retrieve current data from the table, including any changes made to PHID and Ground_truth
    updated_data <- email_data()
    
    # Identify the column name dynamically
    id_column <- intersect(c("PHID", "MemberID", "person_id"), names(updated_data))
    
    if (length(id_column) != 1) {
      stop("None or multiple ID columns detected. Ensure one of 'PHID', 'MemberID', or 'person_id' is present.")
    }
    
    filtered_updated_data_discard <- updated_data[updated_data$Ground_truth == "Discard", ]
    print(filtered_updated_data_discard)
      
    # Save the updated dataset
    email_data_cleaned <- updated_data %>%
      mutate(!!id_column := as.character(.data[[id_column]]))
    
    #Update the discard label
    for (ms_id in filtered_updated_data_discard$message_id) {
      #print(ms_id)
      gmailr::gm_modify_message(id = ms_id, add_labels = rv$inboxVariables$id_label_discard)
    }
    
    # Download parlamentarians_dataset.csv from Google Cloud Storage
    bucket_name_c = paste0(rv$inboxVariables$iinbox_name, "-inbox-other")
    download_file_from_gcs(bucket_name_c,"parlamentarians_dataset.csv", "data/parlamentarians_dataset.csv")
    
    # Read the parliamentarians dataset
    parlamentarians_dataset <- read_csv("data/parlamentarians_dataset.csv")
    
    # Detect the dynamic ID column
    id_column <- intersect(c("PHID", "MemberID", "person_id"), names(parlamentarians_dataset))
    if (length(id_column) != 1) {
      stop("None or multiple ID columns detected. Ensure one of 'PHID', 'MemberID', or 'person_id' is present.")
    }
    
    
    parlamentarians_dataset <- parlamentarians_dataset %>%
      mutate(!!id_column := as.character(.data[[id_column]]))
    
    # Perform the join operation using updated PHID and Ground_truth
    cbr_inbox_main <- parlamentarians_dataset %>%
      right_join(
        dplyr::filter(email_data_cleaned, Ground_truth == "Yes"), 
        by = id_column
      )
      
    # Save the complete cbr_inbox_main to a CSV file
    name_file_to_upload =  paste0(rv$inboxVariables$prefix_upper, "_Inbox_MAIN.csv")
    write_csv(cbr_inbox_main, name_file_to_upload)
    temp_file <- tempfile(fileext = ".csv")
    
    # Upload the resulting CBR_Inbox_MAIN.csv to Google Cloud Storage
    gcs_upload(name_file_to_upload, name = name_file_to_upload, bucket = bucket_name_c)
      
    output$status <- renderUI({
      HTML("<strong style='color: blue;'>", "Data approved and saved to Google Cloud Storage.", "</strong>")
    })
    
    # Call the JavaScript function to scroll to the top
    runjs("scrollToTop();")  # This will scroll to the top of the page
      
    # Set approval completed to TRUE to activate corpus creation button
    approval_completed(TRUE)
      
    
    # Enable 
    shinyjs::enable("create_update_corpus")
    shinyjs::disable("approve")
  })
  
  
  #
  safe_download_file_from_gcs <- function(bucket, file_name, dest_path) {
    tryCatch(
      {
        download_file_from_gcs(bucket, file_name, dest_path)
        TRUE
      },
      error = function(e) {
        if (grepl("http_404", e$message)) {
          warning(paste("File", file_name, "not found in bucket", bucket))
        } else {
          warning(paste("Failed to download", file_name, "from bucket", bucket, ":", e$message))
        }
        FALSE
      }
    )
  }
  
  # Create/Update Corpus button functionality
  observeEvent(input$create_update_corpus, {
    if (approval_completed()) {
      #Define paths 
      bucket_name_c = paste0(rv$inboxVariables$iinbox_name, "-inbox-other")
      corpus_path <- paste0("data/",rv$inboxVariables$prefix_lower, "_inbox_corpus.rds")
      csv_path <- paste0("data/",rv$inboxVariables$prefix_upper, "_Inbox_MAIN.csv")
      corpus_name <- paste0(rv$inboxVariables$prefix_lower, "_inbox_corpus.rds")
      csv_name <- paste0(rv$inboxVariables$prefix_upper, "_Inbox_MAIN.csv")

      # Attempt to download files
      corpus_downloaded <- safe_download_file_from_gcs(bucket_name_c, corpus_name, corpus_path)
      csv_downloaded <- safe_download_file_from_gcs(bucket_name_c, csv_name, csv_path)
      
      if (!corpus_downloaded) {
        metadata_mapping <- list(
          "canberra" = metadata_columns,
          "wellingtonnz" = metadata_columns_w,
          "ottawa" = metadata_columns_o
        )
        
        cbr_inbox_main <- read.csv(csv_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
        print("cbr_inbox_main")
        print(cbr_inbox_main)
        # Create the corpus with metadata
        cbr_inbox_corpus <- corpus(cbr_inbox_main$message_body, docnames = cbr_inbox_main$message_id)
        print("cbr_inbox_corpus")
        print(cbr_inbox_corpus)        
        #docvars(cbr_inbox_corpus) <- cbr_inbox_main[, metadata_columns]
        print("inbox name")
        print(rv$inboxVariables$iinbox_name)
        print(metadata_mapping[[rv$inboxVariables$iinbox_name]])
        docvars(cbr_inbox_corpus) <- cbr_inbox_main[, metadata_mapping[[rv$inboxVariables$iinbox_name]]]
        print(colnames(docvars(cbr_inbox_corpus)))
        # Save the corpus
        saveRDS(cbr_inbox_corpus, file = corpus_path)
        
        # Upload the new corpus to Google Cloud Storage
        
        gcs_upload(corpus_path, 
                     bucket = bucket_name_c, 
                     name = corpus_name,
                    predefinedAcl = "bucketLevel"
                     )
        
        # Update email labels
        for (ms_id in cbr_inbox_main$message_id) {
          print(ms_id)
          gmailr::gm_modify_message(id = ms_id, add_labels = rv$inboxVariables$id_label_inbox )
          gmailr::gm_modify_message(id = ms_id, add_labels = rv$inboxVariables$id_label_retrieved )
        }
        
        output$status <- renderUI({
          HTML("<strong style='color: blue;'>", "Corpus updated and uploaded to Google Cloud Storage.", "</strong>")
        })
        
        shinyjs::disable("create_update_corpus")
        shinyjs::enable("download_csv")
        shinyjs::enable("download_excel")
        
      } else {
        # Function to add new data to the existing corpus
        add_to_corpus <- function(existing_corpus_path, new_data_csv) {
          # Read the existing corpus
          existing_corpus <- readRDS(existing_corpus_path)
          # Read new data
          new_data <- read.csv(new_data_csv, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
          # Remove duplicates from new data
          existing_doc_ids <- docnames(existing_corpus)
          
          # Ensure 'message_id' exists in new_data
          if ("message_id" %in% colnames(new_data)) {
            new_data1 <- new_data[!new_data$message_id %in% existing_doc_ids, ]
            
            # If new data exists, create and combine the corpus
            if (nrow(new_data1) > 0) {
              metadata_mapping <- list(
                "canberra" = metadata_columns,
                "wellingtonnz" = metadata_columns_w,
                "ottawa" = metadata_columns_o
              )
              
              new_corpus <- corpus(new_data1$message_body, docnames = new_data1$message_id)
              
              #docvars(new_corpus) <- new_data1[, metadata_columns]
              docvars(new_corpus) <- new_data1[, metadata_mapping[[rv$inboxVariables$iinbox_name]]]
              
              # Combine with existing corpus
              combined_corpus <- c(existing_corpus, new_corpus)
              
              # Save updated corpus
              saveRDS(combined_corpus, file = existing_corpus_path)
              
              temp_file <- tempfile(fileext = ".rds")
              saveRDS(combined_corpus, temp_file)
              
              # Upload updated corpus to Google Cloud Storage
              gcs_upload(temp_file, 
                         bucket = bucket_name_c, 
                         name = corpus_name,
                         predefinedAcl = "bucketLevel"
              )
             
              # Update email labels for new messages
              for (ms_id in new_data1$message_id) {
                print(ms_id)
                gmailr::gm_modify_message(id = ms_id, add_labels = rv$inboxVariables$id_label_inbox )
                gmailr::gm_modify_message(id = ms_id, add_labels = rv$inboxVariables$id_label_retrieved )
              }
            }
          } else {
            stop("Column 'message_id' not found in new_data")
          }
        }
        
        # Define new data CSV path
        new_data_csv <- csv_path
        add_to_corpus(corpus_path, new_data_csv)
        
        # Uncomment to send email notification
        # sendEmail()
        
        output$status <- renderUI({
          HTML("<strong style='color: blue;'>", "Corpus updated and uploaded to Google Cloud Storage.", "</strong>")
        })
        
        shinyjs::disable("create_update_corpus")
        shinyjs::enable("download_csv")
        shinyjs::enable("download_excel")
      }
    }
  })
  
  
  #Download the updated CSV after modification
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("corpus_newsletters_csv", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      corpus_path <- paste0("data/",rv$inboxVariables$prefix_lower, "_inbox_corpus.rds")
      corpus_updated <- readRDS(corpus_path)
      csv_corpus_df <- quanteda::convert(corpus_updated, to = "data.frame")
      
      if (all(c("doc_id", "text") %in% colnames(csv_corpus_df))) {
        csv_corpus_df <- csv_corpus_df %>%
          select(-doc_id, -text)
      }
      write.csv(csv_corpus_df, file(file, encoding = "UTF-8"), row.names = FALSE, quote = TRUE)
      #readr::write_excel_csv(csv_corpus_df, file, na = "", delim = ",")
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("corpus_newsletters_excel", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      corpus_path <- paste0("data/",rv$inboxVariables$prefix_lower, "_inbox_corpus.rds")
      corpus_updated <- readRDS(corpus_path)
      csv_corpus_df <- quanteda::convert(corpus_updated, to = "data.frame")
      
      if (all(c("doc_id", "text") %in% colnames(csv_corpus_df))) {
        csv_corpus_df <- csv_corpus_df %>%
          select(-doc_id, -text)
      }
      
      xlsx.writeMultipleData(file, csv_corpus_df)
    }
  )
  
  # Logout button functionality
  observeEvent(input$logout, {
    # Reset the authentication
    is_authenticated(FALSE)
    
    # Show login fields again
    shinyjs::show("username")
    shinyjs::show("password")
    shinyjs::show("login")
    
    # Optionally reset the input fields
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    
    # Clear any status messages
    output$login_status <- renderUI({ HTML("") })
    
    # Reset reactive values related to the inbox
    rv$inboxChosen <- NULL  # Set inboxChosen to NULL
    rv$inboxVariables <- list()  # Clear dynamic inbox variables
    #Reset Table
    email_data(data.frame())
    shinyjs::enable("retrieve")
  })
}

shinyApp(ui = ui, server = server)