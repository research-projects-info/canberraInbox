# canberraInbox
A Shiny-based research platform for collecting, processing, and exploring Australian parliamentary e-newsletters. The project automates email retrieval from a Gmail inbox, matches newsletters to Members of Parliament and Senators, stores processed content in Google Cloud Storage, and provides a searchable web interface for analysis and download.

## Features

* Automated retrieval of newsletters from Gmail.
* Extraction and cleaning of HTML and plain-text email content.
* Storage of newsletter content and metadata in Google Cloud Storage.
* Automatic matching of senders to Australian parliamentarians using fuzzy name matching.
* Searchable Shiny web application with filters for party, member, date, and keywords.
* Download filtered or complete datasets in CSV and Excel formats.
* Descriptive statistics and visualizations.
* Party-specific word cloud generation.

## Project Structure

```text
.
├── canberra_main_retrieve_email.R
├── ShinyAppSearch_CBR_Inbox.R
├── ShinyAppAdmin.R
└── README.md
```

### Scripts

| File                                  | Description                                                                                                              |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------ |
| `canberra_main_retrieve_email.R` | Retrieves emails, extracts content, uploads HTML files to Google Cloud Storage, and matches senders to parliamentarians. |
| `ShinyAppSearch_CBR_Inbox.R`   | Main Shiny application used to search, analyse, visualize, and download newsletter data.                                 |
| `ShinyAppAdmin.R`                | Administrative application and maintenance utilities.                                                                    |

## Requirements

### R Packages

* shiny
* shinythemes
* gmailr
* tidyverse
* quanteda
* DT
* lubridate
* ggplot2
* gargle
* googleCloudStorageR
* openxlsx
* xlsx
* stringdist
* rvest
* xml2
* ausPH

### External Services

* Gmail API
* Google Cloud Storage
* Google OAuth credentials

Required environment variables:

```r
GM_CLIENT_JSON
GAR_CLIENT_JSON
```

## Workflow

### 1. Email Retrieval

The retrieval script:

* Connects to Gmail.
* Downloads newsletter emails.
* Extracts HTML or plain-text content.
* Stores original HTML files in Google Cloud Storage.
* Cleans and standardizes email content.
* Matches senders to Australian parliamentarians.
* Generates datasets for downstream analysis.

### 2. Data Processing

The project enriches newsletter data using:

* Australian parliamentary member data (`ausPH`)
* Party affiliation information
* Fuzzy matching using Jaro-Winkler distance

### 3. Shiny Application

The web application provides:

* Full-text newsletter search
* Filtering by party, MP, and date range
* Weekly communication trends
* Party-level descriptive statistics
* Word cloud visualizations
* Dataset downloads (CSV and Excel)

## Running the Project

Install dependencies:

```r
install.packages(c(
  "shiny",
  "tidyverse",
  "gmailr",
  "googleCloudStorageR",
  "quanteda",
  "DT",
  "lubridate",
  "ggplot2"
))
```

Run the email retrieval process:

```r
source("canberra_main_retrieve_email.R")
```

Launch the Shiny application:

```r
shiny::runApp("ShinyAppSearch_CBR_Inbox.R")
```

## Citation

### Data

For academic and non-profit uses, these data have been made freely and publicly available. Please feel welcome to download and use these data in your own research. We kindly ask that you cite CanberraInbox as a data source:

> Casey, Daniel (2025). *CanberraInbox: Political communication, the personal vote and representation styles—studying legislators' e-newsletters in Australia*. *Legislative Studies Quarterly*, 50(3), e70004. https://doi.org/10.1111/lsq.70004

### Software

If you use, modify, or build upon the Canberra Inbox software, please cite:

> Casey, Daniel, and Soto Ruidias, Rosa Rosmery (2026). *Canberra Inbox: Software for Collecting and Analysing Australian Parliamentary E-Newsletters* (Version 1.0) [Computer software]. GitHub repository.

