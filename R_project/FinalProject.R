library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(DBI)
library(RMySQL)
library(forecast)
library(rugarch)
library(shinyjs)
library(bcrypt)

# Hashed password (usually comes from your user database)
password_hash <- hashpw('pass')

# Random session ID (you can generate a more secure one)
sessionid <- "OQGYIrpOvV3KnOpBSPgOhqGxz2dE5A9IpKhP6Dy2kd7xIQhLjwYzskn9mIhRAVHo"

# Define the UI for the login page
login_ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
  body {
  background-image: url('1.jpg');
  background-repeat: no-repeat;
  background-size: cover;
}
    .login-container {
    text-align: center;
      width: 300px;
      margin: 150px auto;
      padding: 20px;
      background-color: #ffffff;
      border-radius: 5px;
      box-shadow: 0px 0px 10px 0px #888888;
    }
    .login-button {
      background-color: #007bff;
      color: #ffffff;
      border: none;
      width: 100%;
      padding: 10px;
      border-radius: 5px;
      font-size: 16px;
    }
    .line {
      width: 100%;
      border-bottom: 1px solid #c3c3c3;
      margin: 20px 0;
    }
    .forgot-password {
      text-align: center;
      color: #007bff;
      font-size: 14px;
    }
    .input-group {
      width: 100%;
      margin-bottom: 10px;
    }
    ")
    )
  ),
  
  # Login container
  div(class = "login-container",
      h2("Login"),
      br(),
      div(class = "input-group",
          span(class = "input-group-addon", shiny::icon("user")),
          shiny::tags$input(type = "text", class = "form-control", id = "username", placeholder = "Username")
      ),
      div(class = "input-group",
          span(class = "input-group-addon", shiny::icon("lock")),
          shiny::tags$input(type = "password", class = "form-control", id = "password", placeholder = "Password")
      ),
      actionButton("login", "Sign In", class = "login-button"),
      div(class = "line"),
      tags$a(href = "#", shiny::icon("question-circle"), "Forgot your password?", class = "forgot-password")
  ),
  tags$footer(
    style = "text-align: center; font-style: italic;",
    "Authorized users olny"
  ), br(),
  
)

# Define the UI
dashboard_ui <- dashboardPage(
  dashboardHeader(title = "UNZA HOSPITAL DATA VISUALIZATION AND ANALYSIS SYSTEM"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Upload Data", tabName = "upload_data", icon = icon("upload")),
      menuItem("Visualize and Analyze Data", tabName = "patient_data", icon = icon("table")),
      menuItem("Predict Future Occurrences", tabName = "predict", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Logout", icon = icon("sign-out"), tabName = "logout")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML("
        body {
            font-family: Arial, Helvetica, sans-serif;
            background-image: url('2.jpg');
            background-size: cover;
            background-color: #e9ddd8;
            background-blend-mode: multiply;

        }
        ")
      )
    ),
    tabItems(
      
      tabItem(tabName = "home",
              h2("Welcome to the UNZA Hospital Data Visualization and Analysis System"),
              p("This system is designed to provide healthcare professionals and researchers with powerful tools for visualizing, analyzing, and forecasting patient data from UNZA Hospital."),
              p("Key Features:"),
              p("- Upload Data: Easily upload patient data from Excel files or MySQL databases."),
              p("- Visualize and Analyze: Explore data patterns, trends, and relationships using a variety of visualization techniques such as pie charts, scatterplots, histograms, line graphs, and bar graphs."),
              p("- Predict Future Occurrences: Forecast future events based on historical data using advanced time series forecasting methods."),
              p("To get started, navigate to the Upload Data tab to upload your data, or select Visualize and Analyze Data to explore existing datasets. You can also use the Predict Future Occurrences tab to make forecasts based on your data."),
              p("For assistance or inquiries, please refer to the About tab."),
      ),
      tabItem(tabName = "upload_data",
              h2("Upload Data"),
              sidebarLayout(
                sidebarPanel(
                  radioButtons("source", "Data source:",
                               choices = list("Excel File" = "excel", "MySQL Database" = "mysql"),
                               selected = "excel"),
                  conditionalPanel(
                    condition = "input.source == 'excel'",
                    fileInput("file", "Upload Excel File", accept = c(".xlsx")),
                    actionButton("analyze", "Visualize and analyze Data")
                  ),
                  conditionalPanel(
                    condition = "input.source == 'mysql'",
                    textInput("dbname", "Database name"),
                    textInput("host", "Host"),
                    textInput("user", "User"),
                    passwordInput("password", "Password"),
                    actionButton("connect", "Connect"),
                    selectInput("table", "Select Table", choices = NULL), # Add this line
                    actionButton("analyze_db", "Analyze Data") # Add this line
                  ),
                  br(), # some spacing
                  tags$hr() # a horizontal line
                ),
                mainPanel(
                  p("")
                )
              )
      ),
      tabItem(tabName = "patient_data",
              h2("Visualize and Analyze Patient Data"),
              sidebarLayout(
                sidebarPanel(
                  uiOutput("columnSelect"),
                  tags$hr() # a horizontal line
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("View Data", tableOutput("viewData")),
                    tabPanel("Pie Chart", plotOutput("pieChart", height = "530px")),
                    tabPanel("Scatterplot", plotOutput("scatterplot", height = "550px")),
                    tabPanel("Histogram", plotOutput("histogram", height = "550px")),
                    tabPanel("Line Graph", plotOutput("lineGraph", height = "550px")),
                    tabPanel("Bar Graph", plotOutput("barGraph", height = "550px")),
                    tabPanel("Density Plot(use only one column)", plotOutput("densityPlot", height = "550px"))
                  ),
                  verbatimTextOutput("selectedColumns"),
                  tags$hr(),
                  tags$footer(
                    style = "text-align: center; font-style: italic;",
                    "Data Source: UNZA Clinic Health Center"
                  ),
                  br()
                )
              )
      ),
      tabItem(tabName = "predict",
              h2("Predict Future Occurrences"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("timeColumn", "Select Time Column:", choices = NULL),
                  selectInput("valueColumn", "Select Value Column:", choices = NULL),
                  numericInput("forecastPeriod", "Number of Periods to Forecast:", value = 12, min = 1),
                  selectInput("modelType", "Select Model Type:", 
                              choices = list("ARIMA" = "arima", "ETS" = "ets", "TBATS" = "tbats", "NNAR" = "nnar")),
                  tags$hr(),
                  actionButton("predict", "Predict"),
                  tags$hr()
                ),
                mainPanel(
                  plotOutput("forecastPlot", height = "530px"),
                  verbatimTextOutput("forecastSummary"),
                  tags$hr(),
                  tags$footer(
                    style = "text-align: center; font-style: italic;",
                    "Data Source: UNZA Clinic Health Center"
                  ),
                  br()
                )
              )
      ),
      tabItem(tabName = "about",
              h2("About"),
              p("Welcome to the UNZA Hospital Data Visualization and Analysis System, a comprehensive tool developed by the R programming group."),
              p("The primary objective of this system is to empower healthcare professionals and researchers with the ability to gain valuable insights from patient data."),
              br(),
              p("Features include:"),
              p("1. Data Upload: Upload patient data from Excel files or MySQL databases."),
              p("2. Visualize and Analyze: Explore patterns, trends, and relationships using pie charts, scatterplots, histograms, line graphs, and bar graphs."),
              p("3. Predict Future Occurrences: Forecast future occurrences based on historical data using advanced time series forecasting techniques."),
              br(),
              p("The patient data used in this application is sourced from the UNZA Clinic Health Center, and all data processing and analysis adhere to applicable data protection regulations."),
              p("For inquiries or feedback regarding the UNZA Hospital Data Visualization and Analysis System, please contact us at +260970846745.")
      ),
      tabItem(tabName = "logout",
              p("click the button below to log out."),
              actionButton("login_again", "log out")
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  status <- reactiveVal(value = NULL)
  logged_in <- reactiveVal(FALSE)
  
  observe({
    shinyjs::runjs("Shiny.onInputChange('jscookie', Cookies.get('sessionid'))")
    if (!is.null(input$jscookie) && input$jscookie == sessionid) {
      logged_in(TRUE)
      status(paste0('Logged in with session ID ', input$jscookie))
    } else {
      status('Not logged in')
    }
  })
  
  observeEvent(input$login, {
    if (input$username == 'user' && checkpw(input$password, hash = password_hash)) {
      shinyjs::runjs(sprintf("Cookies.set('sessionid', '%s');", sessionid))
      logged_in(TRUE)
      status('Logged in successfully')
      showNotification("logged in successfully", type = "message")
    } else {
      status('Login failed: Incorrect username or password.')
      showNotification("logged in unsuccessfully", type = "error")
    }
  })
  
  observeEvent(input$login_again, {
    shinyjs::runjs("Cookies.remove('sessionid')")
    logged_in(FALSE)
    status(NULL)
  })
  
  observeEvent(input$logout, {
    shinyjs::runjs("Cookies.remove('sessionid')")
    logged_in(FALSE)
    status('Logged out')
    showNotification("logged out", type = "error")
  })
  
  output$status <- renderText({ status() })
  
  output$main_ui <- renderUI({
    if (logged_in()) {
      dashboard_ui
    } else {
      login_ui
    }
  })
  
  observeEvent(input$connect, {
    con <- tryCatch(
      dbConnect(MySQL(), dbname = input$dbname, host = input$host, user = input$user, password = input$password),
      error = function(e){
        showNotification("Connection failed: Check your input credentials", type = "error")
        return(NULL)
      })
    if(!is.null(con)){
      tables <- dbListTables(con)
      updateSelectInput(session, "table", choices = tables)
    }
  })
  
  data <- eventReactive(c(input$analyze, input$analyze_db), {
    if(input$source == 'mysql' && input$analyze_db > 0) {
      con <- tryCatch(
        dbConnect(MySQL(), dbname = input$dbname, host = input$host, user = input$user, password = input$password),
        error = function(e){
          showNotification("Connection failed: Check your input credentials", type = "error")
          return(NULL)
        })
      if(is.null(con)){
        return(NULL)
      }
      
      df <- dbGetQuery(con, paste0("SELECT * FROM ", input$table))
      dbDisconnect(con)
      return(df)
    } else if(input$source == 'excel' && input$analyze > 0) {
      inFile <- input$file
      if (is.null(inFile)) {
        showNotification("failed to import: please import your excel file", type = "error")
        return(NULL)
      }else{
        showNotification("analyzed successfully", type = "message")
      }
      read_excel(inFile$datapath)
    }
  })
  
  observe({
    df <- data()
    if (!is.null(df)) {
      updateSelectInput(session, "timeColumn", choices = names(df))
      updateSelectInput(session, "valueColumn", choices = names(df))
    }
  })
  
  output$densityPlot <- renderPlot({
    df <- data()
    if (is.null(df) || input$columns == "") {
      return(NULL)
    }
    density_data <- df[[input$columns]]
    ggplot(df, aes(x = density_data)) +
      geom_density(fill = "skyblue", color = "blue") +
      theme_minimal() +
      labs(x = input$columns, y = "Density", title = "Density Plot")
  })
  
  output$viewData <- renderTable({
    df <- data()
    if (is.null(df)) {
      return(NULL)
    }
    df
  })
  
  output$columnSelect <- renderUI({
    df <- data()
    if (is.null(df)) {
      return(NULL)
    }
    selectInput("columns", "Choose column(s) to visualize and analyze:", choices = c("Select Columns" = "", names(df)), multiple = TRUE)
  })
  
  output$viewData <- renderTable({
    df <- data()
    if (is.null(df)) {
      return(NULL)
    }
    df
  })
  
  output$pieChart <- renderPlot({
    df <- data()
    if (is.null(df) || length(input$columns) == 0) {
      return(NULL)
    }
    
    # Calculate percentages
    pie_data <- df[[input$columns[1]]]
    df$percentage <- (pie_data / sum(pie_data)) * 100
    
    # Create pie chart with percentages
    ggplot(df, aes(x = "", y = percentage, fill = row.names(df))) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void() +
      theme(legend.position = "right") +
      labs(fill = "Key", title = "Pie Chart") +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5))
  })
  
  
  
  
  
  output$scatterplot <- renderPlot({
    df <- data()
    if (is.null(df) || length(input$columns) < 2) {
      return(NULL)
    }
    scatter_data_x <- df[[input$columns[1]]]
    scatter_data_y <- df[[input$columns[2]]]
    ggplot(df, aes(x = scatter_data_x, y = scatter_data_y)) +
      geom_point(color = "red") +
      theme_minimal() +
      labs(x = input$columns[1], y = input$columns[2], title = "Scatterplot")
  })
  
  output$histogram <- renderPlot({
    df <- data()
    if (is.null(df) || length(input$columns) == 0) {
      return(NULL)
    }
    hist_data <- df[[input$columns[1]]]
    ggplot(df, aes(x = hist_data)) +
      geom_histogram(binwidth = 10, fill = "blue", color = "green") +
      theme_minimal() +
      labs(x = input$columns[1], y = "Frequency", title = "Histogram")
  })
  
  output$lineGraph <- renderPlot({
    df <- data()
    if (is.null(df) || length(input$columns) < 2) {
      return(NULL)
    }
    line_data_x <- df[[input$columns[1]]]
    line_data_y <- df[[input$columns[2]]]
    ggplot(df, aes(x = line_data_x, y = line_data_y, color = line_data_y)) +
      geom_line() +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal() +
      labs(x = input$columns[1], y = input$columns[2], title = "Line Graph")
  })
  
  output$barGraph <- renderPlot({
    df <- data()
    if (is.null(df) || length(input$columns) < 2) {
      return(NULL)
    }
    bar_data_x <- df[[input$columns[1]]]
    bar_data_y <- df[[input$columns[2]]]
    ggplot(df, aes(x = bar_data_x, y = bar_data_y, fill = bar_data_x)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = input$columns[1], y = input$columns[2], title = "Bar Graph")
  })
  
  output$selectedColumns <- renderPrint({
    input$columns
  })
  
  forecast_data <- eventReactive(input$predict, {
    df <- data()
    if (is.null(df) || input$timeColumn == "" || input$valueColumn == "") {
      return(NULL)
    }
    ts_data <- ts(df[[input$valueColumn]], frequency = 12)
    
    model <- switch(input$modelType,
                    arima = auto.arima(ts_data),
                    ets = ets(ts_data),
                    tbats = tbats(ts_data),
                    nnar = nnetar(ts_data)
    )
    
    forecast(model, h = input$forecastPeriod)
  })
  
  output$forecastPlot <- renderPlot({
    forecast_res <- forecast_data()
    if (is.null(forecast_res)) {
      return(NULL)
    }
    autoplot(forecast_res) +
      labs(title = "Forecasted Data", x = "Time", y = "Value")
  })
  
  output$forecastSummary <- renderPrint({
    forecast_res <- forecast_data()
    if (is.null(forecast_res)) {
      return(NULL)
    }
    summary(forecast_res)
  })
}
ui <- uiOutput("main_ui")
# Run the application
shinyApp(ui = ui, server = server)