library(shiny)
library(shinythemes)
library(quanteda)
library(DT)
library(wordcloud)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gargle)
library(googleCloudStorageR)
library(openxlsx)
library(xlsx)

# Authenticate with Google Cloud
path_json <- Sys.getenv("GAR_CLIENT_JSON")
gcs_auth(path_json)

# Download the corpus from Google Cloud Storage
gcs_get_object("cbr_inbox_corpus.rds", bucket = "canberra-inbox-other", saveToDisk = "data/cbr_inbox_corpus.rds", overwrite = TRUE)


# Load the corpus once at startup
canberra_inbox_corpus <- readRDS("data/cbr_inbox_corpus.rds")
canberra_inbox_corpus <- quanteda::convert(canberra_inbox_corpus, to = "data.frame")


# Convert message_date from "day/month/year" to Date format
canberra_inbox_corpus$message_date <- lubridate::dmy(canberra_inbox_corpus$message_date)

# Get the oldest and most recent dates
min_date <- min(canberra_inbox_corpus$message_date, na.rm = TRUE)
max_date <- max(canberra_inbox_corpus$message_date, na.rm = TRUE)

###########___________Funtions_______________#####
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
# Fetch the log file
fetch_logs <- function() {
  gcs_get_object("log.csv", saveToDisk = "data/local_log.csv", bucket = "canberra-inbox-other", overwrite = TRUE )
  read.csv("data/local_log.csv", stringsAsFactors = FALSE)
}

# Update the log file
update_logs <- function(csv_increment_f = 0, excel_increment_f = 0, csv_increment_h = 0, excel_increment_h = 0) {
  logs <- fetch_logs()
  
  # Increment download counts
  logs$csv_downloads_f <- logs$csv_downloads_f + csv_increment_f
  logs$excel_downloads_f <- logs$excel_downloads_f + excel_increment_f
  logs$csv_downloads_h <- logs$csv_downloads_h + csv_increment_h
  logs$excel_downloads_h <- logs$excel_downloads_h + excel_increment_h
  
  # Save the updated log locally
  write.csv(logs, "data/local_log.csv", row.names = FALSE)
  
  # Upload the updated log to Google Cloud Storage
  gcs_upload("data/local_log.csv", name = "log.csv", bucket = "canberra-inbox-other")
}

############__________APP________####
ui <- navbarPage(
  title = "CanberraInbox",
  theme = shinytheme("cerulean"),  # Apply Cerulean theme
  
  tabPanel("Home",
           fluidPage(
             fluidRow(
               column(3,  # Left side column (Sidebar for filters)
                      wellPanel(
                        h4("Filters"),
                        selectInput("partyFilter", "Filter by Party:", 
                                    choices = c("All", unique(canberra_inbox_corpus$Party)), 
                                    selected = "All"),
                        selectInput("nameFilter", "Filter by Name:", 
                                    choices = c("All", unique(canberra_inbox_corpus$SurnameName)), 
                                    selected = "All"),
                        dateRangeInput("dateFilter", "Filter by Date:",
                                       start = min_date, end = max_date,
                                       min = min_date, max = max_date)
                      )
               ),
               column(9,  # Main content column
                      fluidRow(
                        column(12, 
                               textInput("searchTerm", "Enter search term:", "", width = "50%")
                        )
                      ),
                      fluidRow(
                        column(4, downloadButton("downloadCSV", "Download CSV")),
                        column(4, downloadButton("downloadExcel", "Download Excel")),
                        column(4, actionButton("plotButton", "Graph", class = "btn-primary"))  # Moved the "Graph" button here
                      ), br(),
                      fluidRow(
                        column(12,
                               plotOutput("filteredPlot")  # Placeholder for the plot
                        )
                      ),
                      br(),
                      fluidRow(
                        column(12,
                               DTOutput("filteredResults")
                        )
                      )
                      
               )
             )
           )
  )
  ,
  
  tabPanel("About & Citation",
           fluidPage(
             h2("About & Citation"),
             span("This application collects and all e-newsletters sent by Australian MPs, to allow researchers to explore the messages conveyed by various parties and representatives.
                    Inspired by initiatives like",
                  a(href = "https://www.dcinbox.com/", "DCInbox", target = "_blank"), 
                  ", and lead by Dr. Daniel Casey, this research project aims to enable users to explore trends and patterns in political communication, contributing to a deeper understanding of political messaging and engagement.",
                  br(), br(),
                    "As at ", format(Sys.Date(), "%d-%m-%Y"), "there are", nrow(canberra_inbox_corpus), "e-newsletters in the dataset, and growing every week.",
                  br(),br(),
                  "For academic and non-profit uses: These data have been made freely and publicly available. Please feel welcome to download and use these data in your own research. We only ask that you kindly cite the CanberraInbox as a data source. 
                  The suggested citation is: ", a(href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4997848", 
                                            "Casey, Daniel, CanberraInbox: Political communication, the personal vote and representation styles -studying legislators' enewsletters in Australia (October 19, 2024). Available at SSRN: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4997848"),
                  target = "_blank"),
                  br(),br(),
             fluidRow(
               column(4, 
                      img(src = "Casey_Daniel_21_0.jpg", 
                          width = "60%", height = "auto", alt = "Dr. Daniel Casey")
               ),
               column(8, 
                      h3("Dr. Daniel Casey"),
                      p("School and/or Centres: ANU School of Politics & International Relations"),
                      p("Email: ", a(href = "mailto:daniel.casey@anu.edu.au", "daniel.casey@anu.edu.au", target = "_blank")),
                      p("Website: ", a(href = "http://www.daniel-casey.org/", "www.daniel-casey.org", target = "_blank")),
                      p("Google Scholar: ", a(href = "https://scholar.google.com/citations?user=B0JzF2sAAAAJ&hl=en", "daniel.scholar.google.com", target = "_blank"))
               )
             ),
             br(),
             fluidRow(
               column(4, 
                      img(src = "Rosa_Soto_Ruidias.png", 
                          width = "60%", height = "auto", alt = "Rosa Soto")
               ),
               column(8, 
                      h3("MComp. Rosa R. Soto Rudias "),
                      p("School and/or Centres: ANU School of Politics & International Relations"),
                      p("Email: ", a(href = "mailto:rosarosmery.sotoruidias@anu.edu.au", "rosarosmery.sotoruidias@anu.edu.au", target = "_blank")),
                      p("Linkedin: ", a(href = "www.linkedin.com/in/rosa-soto-ruidias", "www.linkedin.com/in/rosa-soto-ruidias", target = "_blank")),
                      p("Google Scholar: ", a(href = "https://scholar.google.com/citations?user=n3Ap1oMAAAAJ&hl=en&authuser=1", "rosa.scholar.google.com", target = "_blank"))
               )
             ),br()
           )
  ),
  
  tabPanel("Descriptive Statistics",
           fluidPage(
             h2("Descriptive Statistics"),
             p("This page offers an overview of newsletters send by the Members of Parliament. Explore the weekly fluctuations in e-newsletters, 
                 compare the communication strategies of different parties and MPs in each chamber. 
                 You can customize the analysis by selecting specific date ranges to focus on periods of interest.
                 As at ", format(Sys.Date(), "%d-%m-%Y"), "there are", nrow(canberra_inbox_corpus), "e-newsletters in the dataset, and growing every week."),
             
             # Date range input to allow selecting start and end dates
             dateRangeInput("date_range", 
                            label = "Select Date Range:",
                            start = min_date,  # Default to min_date
                            end = max_date,    # Default to max_date
                            min = min_date,    # Set the minimum selectable date
                            max = max_date     # Set the maximum selectable date
             ),
             
             plotOutput("lineChart"),  # Output for the line chart
             
             h3("Table 1: Number and Percentage of Newsletters by Party"),
             tableOutput("partyTable"),  # Output for the first table
             
             h3("Table 2: Number and Percentage of Newsletters by Chamber"),
             tableOutput("memberTable"),  # Output for the second table
           ),br()
  ),
  
  tabPanel("Wordclouds",
           fluidPage(
             fluidRow(
               column(4, dateInput("startDate", "Start Date", 
                                   value = min_date, 
                                   min = min_date, 
                                   max = max_date)),
               column(4, dateInput("endDate", "End Date", 
                                   value = max_date, 
                                   min = min_date, 
                                   max = max_date)),
               column(4, actionButton("generate", "Generate Wordclouds"))
             ),
             
             fluidRow(
               column(6, div(style = "border: 1px solid #ccc;", plotOutput("wordcloudALP"))),
               column(6, div(style = "border: 1px solid #ccc;", plotOutput("wordcloudLPA")))
             ),
             fluidRow(
               column(6, div(style = "border: 1px solid #ccc;", plotOutput("wordcloudGreens"))),
               column(6, div(style = "border: 1px solid #ccc;", plotOutput("wordcloudOthers")))
             ),br()
           )
  ),
  
  tabPanel("Full Dataset",
           fluidPage(
             h2("Full Dataset"),
             downloadButton("downloadData", "Download CSV"),
             downloadButton("downloadDataExcel", "Download Excel"),
             p("You can download the full dataset in either CSV or Excel. We have noticed that some the length of the contents of some of the e-newsletters has created issues with the CSV when opened with Excel. 
               Those issues do not appear to impact GoogleSheets. We are aware of error messages when opening the XLS file, but it does not appear to impact the data. "),
             
              )
  )
)

server <- function(input, output) {
  
  # Reactive expression to filter data based on search term
  filteredData <- reactive({
    data <- canberra_inbox_corpus
    
    # Apply Party filter
    if (input$partyFilter != "All") {
      data <- data[data$Party == input$partyFilter, ]
    }
    
    # Apply Name filter
    if (input$nameFilter != "All") {
      data <- data[data$SurnameName == input$nameFilter, ]
    }
    
    # Apply Date filter
    if (!is.null(input$dateFilter)) {
      data <- data[data$message_date >= input$dateFilter[1] & data$message_date <= input$dateFilter[2], ]
    }
    
    # Apply search term filter
    if (input$searchTerm != "") {
      data <- data[grepl(input$searchTerm, data$message_subject, ignore.case = TRUE) |
                     grepl(input$searchTerm, data$message_body, ignore.case = TRUE), ]
    }
    
    return(data)
  })
  
  
  # Render filtered data in a DataTable with custom formatting
  output$filteredResults <- renderDT({
    data <- filteredData()
    
    if (nrow(data) == 0) {
      return(data.frame(Result = "No results found."))
    }

    # Ensure date and hour columns are sorted properly
    data <- data %>%
      arrange(desc(as.Date(message_date, format = "%Y-%m-%d")), 
              desc(as.POSIXct(message_hour, format = "%H:%M:%S")))
    
    data <- data.frame(
      Message = sprintf(
        '<div>
            <h3>%s</h3>
            <p><b>Author: </b>%s</p>
            <p><b>Party: </b>%s</p>
            <p><b>Chamber: </b>%s</p>
            <p><b>Date: </b>%s</p>
            <p><b>Hour: </b>%s</p>
            <a href="https://storage.googleapis.com/canberra-inbox-html-other/%s" target="_blank">View Original HTML</a>
            <p>%s</p>
            <hr>
           </div>',
        data$message_subject, data$SurnameName, data$Party, data$MemberOrSenator, 
        data$message_date, data$message_hour, data$message_html_link, data$message_body
      )
    )
    
    datatable(data, escape = FALSE,
              options = list(
                dom = 'i<"top"l>rt<"bottom"p><"clear">',  # Updated to control the layout
                pageLength = 15,
                lengthMenu = c(15, 30, 50),
                autoWidth = TRUE
              ),
              rownames = FALSE)
  })
  
  
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      # Sanitize the search term
      sanitized_search_term <- gsub("[^A-Za-z0-9]", "_", input$searchTerm) 
      paste("canberrainbox_", sanitized_search_term, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      update_logs(csv_increment_h = 1)
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
  
  # New event observer for the "Graph" button
  observeEvent(input$plotButton, {
    data <- filteredData()
    
    if (nrow(data) == 0) {
      showNotification("No data available for the selected filters.", type = "error")
      return()
    }
    
    # Generate a bar chart
    output$filteredPlot <- renderPlot({
      data %>%
        mutate(week_start = floor_date(message_date, unit = "week", week_start = 1),
               Party = case_when(
                 Party == "Australian Labor Party" ~ "Australian Labor Party",
                 Party %in% c("Liberal Party of Australia", "The Nationals") ~ "Liberal/National Coalition",
                 Party == "Australian Greens" ~ "Australian Greens",
                 TRUE ~ "Others")) %>%
        group_by(week_start, Party) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        ggplot(aes(x = week_start, y = Count, fill = Party)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Number of Observations per Week by Party",
             x = "Week", y = "Number of Observations") +
        theme_minimal() +
        scale_fill_manual(values = c("Australian Labor Party" = "#BB1312",
                                     "Liberal/National Coalition" = "#004694",
                                     "Australian Greens" = "#07A800",
                                     "Others" = "black")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
  
  # Download filtered data as Excel
  output$downloadExcel <- downloadHandler(
    filename = function() {
      # Sanitize the search term
      sanitized_search_term <- gsub("[^A-Za-z0-9]", "_", input$searchTerm) # Replace non-alphanumeric characters with underscores
      paste("canberrainbox_", sanitized_search_term, "_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      update_logs(excel_increment_h = 1)
      xlsx.writeMultipleData(file, filteredData())
    }
  )
  
  # Helper function to generate wordclouds
  generate_wordcloud <- function(data, title) {
    tokens_data <- tokens(data$message_body, remove_punct = TRUE)
    dfm_data <- dfm(tokens_data) %>%
      dfm_remove(stopwords("en")) %>%
      dfm_trim(min_termfreq = 1) %>%
      dfm_keep(pattern = "\\b\\w{4,}\\b", valuetype = "regex")
    
    # Calculate word frequency
    freq <- colSums(as.matrix(dfm_data))
    top_words <- sort(freq, decreasing = TRUE)[1:10]
    
    # Plot wordcloud
    wordcloud(words = names(top_words), freq = top_words,
              scale = c(3, 0.5), max.words = 10, 
              colors = brewer.pal(5, "Dark2"))
    title(main = title, cex.main = 1.5)
  }
  
  # Initial rendering of wordclouds based on default date range
  observe({
    filtered_corpus <- canberra_inbox_corpus %>%
      filter(message_date >= min(canberra_inbox_corpus$message_date) & 
               message_date <= max(canberra_inbox_corpus$message_date))
    
    output$wordcloudALP <- renderPlot({
      generate_wordcloud(filtered_corpus %>% filter(Party == "Australian Labor Party"), 
                         "Australian Labor Party")
    })
    
    output$wordcloudLPA <- renderPlot({
      generate_wordcloud(filtered_corpus %>% filter(Party %in% c("Liberal Party of Australia", "The Nationals")), 
                         "Liberal/National Coalition")
    })
    
    output$wordcloudGreens <- renderPlot({
      generate_wordcloud(filtered_corpus %>% filter(Party == "Australian Greens"), 
                         "Australian Greens")
    })
    
    output$wordcloudOthers <- renderPlot({
      generate_wordcloud(filtered_corpus %>% filter(!(Party %in% c("Australian Labor Party", "Liberal Party of Australia", "Australian Greens"))), 
                         "Others")
    })
  })
  
  # Update wordclouds based on selected date range when "Generate" is clicked
  observeEvent(input$generate, {
    filtered_corpus <- canberra_inbox_corpus %>%
      filter(message_date >= input$startDate & message_date <= input$endDate)
    
    output$wordcloudALP <- renderPlot({
      generate_wordcloud(filtered_corpus %>% filter(Party == "Australian Labor Party"), 
                         "Australian Labor Party")
    })
    
    output$wordcloudLPA <- renderPlot({
      generate_wordcloud(filtered_corpus %>% filter(Party %in% c("Liberal Party of Australia", "The Nationals")), 
                         "Liberal/National Coalition")
    })
    
    output$wordcloudGreens <- renderPlot({
      generate_wordcloud(filtered_corpus %>% filter(Party == "Australian Greens"), 
                         "Australian Greens")
    })
    
    output$wordcloudOthers <- renderPlot({
      generate_wordcloud(filtered_corpus %>% filter(!(Party %in% c("Australian Labor Party", "Liberal Party of Australia", "Australian Greens"))), 
                         "Others")
    })
  })
  
  # Reactive expression to filter and group data by week
  party_newsletters_per_week <- reactive({
    # Use selected date range or default to min_date and max_date
    selected_dates <- input$date_range
    
    canberra_inbox_corpus %>%
      filter(message_date >= selected_dates[1] & message_date <= selected_dates[2]) %>%
      mutate(Party = case_when(
        Party == "Australian Labor Party" ~ "Australian Labor Party",
        Party %in% c("Liberal Party of Australia", "The Nationals") ~ "Liberal/National Coalition",
        Party == "Australian Greens" ~ "Australian Greens",
        TRUE ~ "Others"
      )) %>%
      mutate(message_date = as.Date(message_date, format = "%Y%m%d"), # If the format is "20231001"
             week_start = floor_date(message_date, unit = "week", week_start = 1)) %>%
      #mutate(week_start = floor_date(message_date, unit = "week", week_start = 1)) %>% # Group by week starting on Monday
      group_by(week_start, Party) %>%
      summarise(Count = n()) %>%
      ungroup()
  })
  
  # Table 1: Number and percentage of newsletters by party
  party_summary <- reactive({
    selected_dates <- input$date_range
    
    # Group by party and count
    party_counts <- canberra_inbox_corpus %>%
      filter(message_date >= selected_dates[1] & message_date <= selected_dates[2]) %>%
      mutate(Party = case_when(
        Party == "Australian Labor Party" ~ "Australian Labor Party",
        Party %in% c("Liberal Party of Australia", "The Nationals") ~ "Liberal/National Coalition",
        Party == "Australian Greens" ~ "Australian Greens",
        TRUE ~ "Others"
      )) %>%
      group_by(Party) %>%
      summarise(Count = n()) %>%
      ungroup()
    
    # Calculate percentages
    total <- sum(party_counts$Count)
    party_counts <- party_counts %>%
      mutate(Percentage = (Count / total) * 100)
    
    # Add total row
    party_counts <- bind_rows(party_counts, 
                              tibble(Party = "Total", Count = total, Percentage = 100))
    
    return(party_counts)
  })
  
  # Table 2: Number and percentage of newsletters by chamber
  member_summary <- reactive({
    selected_dates <- input$date_range
    
    # Group by MemberOrSenator and count
    member_counts <- canberra_inbox_corpus %>%
      filter(message_date >= selected_dates[1] & message_date <= selected_dates[2]) %>%
      group_by(MemberOrSenator) %>%
      summarise(Count = n()) %>%
      ungroup()
    
    # Calculate percentages
    total <- sum(member_counts$Count)
    member_counts <- member_counts %>%
      mutate(Percentage = (Count / total) * 100)
    
    # Add total row
    member_counts <- bind_rows(member_counts, 
                               tibble(MemberOrSenator = "Total", Count = total, Percentage = 100))
    
    return(member_counts)
  })
  
  # Render the line chart for newsletters by party per week
  output$lineChart <- renderPlot({
    data <- party_newsletters_per_week() %>%
      filter(!is.na(week_start) & !is.na(Count))
    
    ggplot(data, aes(x = week_start, y = Count, color = Party)) +
      geom_line(linewidth = 0.5, linetype = "dotted") +
      geom_point(size = 3) +
      geom_smooth(span = 0.5, method = "loess", na.rm = TRUE, se= F) +  # Increased span for better smoothing
      labs(title = "Newsletters per week by party",
           x = "Weeks", y = "Number of Newsletters") +
      scale_color_manual(values = c("Australian Labor Party" = "#BB1312",
                                    "Liberal/National Coalition" = "#004694",
                                    "Australian Greens" = "#07A800",
                                    "Others" = "black")) +
      ylim(0, max(data$Count, na.rm = TRUE)) +  # Ensures NA values don't affect the y-axis
      theme_minimal() +
      theme(
        legend.position = "right",
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),   # Remove minor grid lines
        axis.text = element_text(family = "sans-serif"),
        plot.title = element_text(hjust = 0.5),
      )
  })
  
  # Render the first table for newsletters by party
  output$partyTable <- renderTable({
    party_summary()
  })
  
  # Render the second table for newsletters by Chamber
  output$memberTable <- renderTable({
    member_summary()
  })  
  
  # Download handler for CSV download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("canberrainbox_newsletters_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      update_logs(csv_increment_f = 1)
      canberra_inbox_dw <- readRDS("data/cbr_inbox_corpus.rds")
      canberra_inbox_df <- quanteda::convert(canberra_inbox_dw, to = "data.frame")# Convert to data frame
      canberra_inbox_corpus_download <- canberra_inbox_df %>%
        select(-massage_image_directory, -doc_id, -text)
      write.csv(canberra_inbox_corpus_download, file, row.names = FALSE, quote = TRUE)
    }
  )
  
  
  
  output$downloadDataExcel <- downloadHandler(
    filename = function() {
      paste("canberra_inbox_corpus-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      update_logs(excel_increment_f = 1)
      canberra_inbox_dw <- readRDS("data/cbr_inbox_corpus.rds")
      canberra_inbox_df <- quanteda::convert(canberra_inbox_dw, to = "data.frame") # Convert to data frame
      canberra_inbox_corpus_download <- canberra_inbox_df %>%
        select(-massage_image_directory, -doc_id, -text)
      xlsx.writeMultipleData(file, canberra_inbox_corpus_download)
    }
  )
  
}

shinyApp(ui = ui, server = server)
