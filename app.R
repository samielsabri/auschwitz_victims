library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

# Assuming auschwitz_victims_final.csv is in the working directory
# Load the dataset
auschwitz_victims_final <- read.csv("auschwitz_victims_final.csv")

# Ensure 'Country' column exists
if (!"Country" %in% colnames(auschwitz_victims_final)) {
  stop("Column 'Country' not found in the dataset.")
}

# Define UI
ui <- fluidPage(
  titlePanel("Holocaust Victims at Auschwitz"),
  sidebarLayout(
    sidebarPanel(
      actionButton("selectAll", "Select/Deselect All"),
      checkboxGroupInput("selectedCountries", "Select Countries:",
                         choices = unique(auschwitz_victims_final$Country)),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graph", plotOutput("countryPlot")),
        tabPanel("Table", DTOutput("countryTable"))
      ),
      width = 9
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Track the state of the "Select All" action
  selectAllState <- reactiveValues(selected = FALSE)
  
  observeEvent(input$selectAll, {
    selectAllState$selected <- !selectAllState$selected
    
    if (selectAllState$selected) {
      updateCheckboxGroupInput(session, "selectedCountries",
                               selected = unique(auschwitz_victims_final$Country))
    } else {
      updateCheckboxGroupInput(session, "selectedCountries", selected = character(0))
    }
  })
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    if (is.null(input$selectedCountries)) {
      return(auschwitz_victims_final)
    }
    auschwitz_victims_final %>%
      filter(Country %in% input$selectedCountries)
  })
  
  # Render the graph
  output$countryPlot <- renderPlot({
    ggplot(filteredData(), aes(x = Country, fill = Country)) +
      geom_bar() +
      theme_minimal() +
      labs(title = "Number of Holocaust Victims by Country",
           x = "Country", y = "Number of Victims") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the table
  output$countryTable <- renderDT({
    filteredData()
  }, options = list(pageLength = 10))
}

# Run the app
shinyApp(ui = ui, server = server)
