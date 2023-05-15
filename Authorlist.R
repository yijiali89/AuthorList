library(shiny)
library(tidyverse)
library(readxl)
library(common)

ui <- fluidPage(
  titlePanel("Author and Affiliation List"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fileInput("file", "Choose Excel File", accept = c(".xls", ".xlsx")), 
      print('Sample excel sheet: https://github.com/yijiali89/AuthorList/blob/main/Sample.xlsx')
    ), 
    mainPanel = mainPanel(
      h4("Author List:"),
      verbatimTextOutput("authorOutput"),
      h4("Affiliation List:"),
      verbatimTextOutput("affiliationOutput")
    )
))

server <- function(input, output) {
  output$authorOutput <- renderPrint({
    req(input$file)
    data <- read_excel(input$file$datapath)
    
    affiliation_list <- data%>%
      select(starts_with("Affiliation")) %>%
      pivot_longer(cols = starts_with("Affiliation"), 
                   names_to = "Affiliation_Number", values_to = "Affiliation") %>%
      distinct(Affiliation) %>%
      na.exclude()%>%
      mutate(Affiliation_Number=seq(1:nrow(.)))
    
    author_list <-data%>%
      select(Author, starts_with("Affiliation")) %>%
      pivot_longer(cols = starts_with("Affiliation"), names_to = "Affiliation_Number", values_to = "Affiliation") %>%
      select(-Affiliation_Number)%>%
      left_join(., affiliation_list, by='Affiliation')%>%filter(Affiliation_Number>0)%>%
      group_by(Author) %>%
      summarize(Affiliations = paste(Affiliation_Number[!is.na(Affiliation)], collapse = " ")) %>%
      ungroup()%>%
      arrange(factor(Author, levels=data$Author)) 
    
    cat("Author List:\n")
    for (i in seq_along(author_list$Author)) {
      cat(author_list$Author[i] %p% supsc(author_list$Affiliations[i]), ", ")
    }
  })
  
  output$affiliationOutput <- renderPrint({
    req(input$file)
    data <- read_excel(input$file$datapath) 
    
    affiliation_list <- data%>%
      select(starts_with("Affiliation")) %>%
      pivot_longer(cols = starts_with("Affiliation"), 
                   names_to = "Affiliation_Number", values_to = "Affiliation") %>%
      distinct(Affiliation) %>%
      na.exclude()%>%
      mutate(Affiliation_Number=seq(1:nrow(.)))
    
    cat("\nAffiliation List:\n")
    for (i in seq_along(affiliation_list$Affiliation)) {
      
      cat(paste0(i, ". ", affiliation_list$Affiliation[i]), '\n')
      
    }
  })
}

shinyApp(ui, server)
