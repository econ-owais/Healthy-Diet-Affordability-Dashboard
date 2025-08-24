#########################################################
# Healthy Diet Affordability Dashboard
# Prepared by: Owais Ali Shah
# Empowering Policy Through Data
#########################################################

# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly) # Keeping if future complex plots are needed, though Chart.js is often sufficient for infographics
library(ggplot2) # Keeping for potential future use or local plotting debug
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(highcharter)
library(waiter)
library(shinycssloaders)
library(shinyWidgets)
library(bslib)
library(stringr)
library(shinyjs)

# ======================
# Translation Setup
# ======================
# Create translation dictionary for healthy diet data context
translations <- list(
  en = list(
    dashboard_title = "Healthy Diet Affordability Dashboard",
    overview_details = "Overview & Details",
    trends = "Trend Analysis",
    explorer = "Data Explorer",
    about = "About",
    select_item = "Select Indicator (Item):",
    year_range = "Year Range:",
    total_items = "Total Indicators Tracked",
    time_period = "Time Period Covered",
    data_points = "Total Data Points",
    average_value = "Average Value",
    key_indicators = "Key Indicators Overview",
    quick_insights = "Quick Insights",
    recent_updates = "Recent Data by Item",
    item_trends = "Indicator Trends Over Time",
    trend_options = "Trend Analysis Options",
    statistical_summary = "Statistical Summary",
    indicator_data = "Raw Data Explorer",
    download_data = "Download Data",
    about_title = "About Healthy Diet Affordability Dashboard",
    about_project_details = "This dashboard provides a comprehensive view of the cost and affordability of a healthy diet in Pakistan, enabling analysis of key trends over time.",
    about_author_details = "This dashboard is a data visualization project by Owais Ali Shah, an economics graduate and a data analyst with a passion for using data to understand and solve real-world problems. This dashboard is a reflection of that commitment."
  ),
  ur = list(
    dashboard_title = "صحت مند غذا کی سستی کا ڈیش بورڈ",
    overview_details = "جائزہ اور تفصیلات",
    trends = "رجحان کا تجزیہ",
    explorer = "ڈیٹا ایکسپلورر",
    about = "کے بارے میں",
    select_item = "اشارہ منتخب کریں (آئٹم):",
    year_range = "سال کی رینج:",
    total_items = "ٹریک کردہ کل اشارے",
    time_period = "احاطہ شدہ وقت",
    data_points = "کل ڈیٹا پوائنٹس",
    average_value = "اوسط قدر",
    key_indicators = "اہم اشاروں کا جائزہ",
    quick_insights = "فوری بصیرت",
    recent_updates = "آئٹم کے لحاظ سے حالیہ ڈیٹا",
    item_trends = "وقت کے ساتھ اشاروں کے رجحانات",
    trend_options = "رجحان کے تجزیہ کے اختیارات",
    statistical_summary = "اعداد و شمار کا خلاصہ",
    indicator_data = "خام ڈیٹا ایکسپلورر",
    download_data = "ڈیٹا ڈاؤن لوڈ کریں",
    about_title = "صحت مند غذا کی سستی کے ڈیش بورڈ کے بارے میں",
    about_project_details = "یہ ڈیش بورڈ پاکستان میں صحت مند غذا کی لاگت اور سستی کا ایک جامع جائزہ فراہم کرتا ہے، جو وقت کے ساتھ اہم رجحانات کا تجزیہ کرنے کے قابل بناتا ہے۔",
    about_author_details = "یہ ڈیش بورڈ اویس علی شاہ کا ایک ڈیٹا ویژولائزیشن پراجیکٹ ہے، جو ایک اقتصادیات کا گریجویٹ اور ڈیٹا تجزیہ کار ہے اور حقیقی دنیا کے مسائل کو سمجھنے اور حل کرنے کے لیے ڈیٹا کا استعمال کرنے کا جذبہ رکھتا ہے۔ یہ ڈیش بورڈ اسی عزم کا عکاس ہے۔"
  )
)

# ======================
# Load & Clean Data
# ======================
# Set up waiting screen
waiting_screen <- tagList(
  div(
    style = "text-align: center; color: #fff;",
    div(
      style = "margin-bottom: 20px;",
      img(src = "https://upload.wikimedia.org/wikipedia/commons/3/32/Flag_of_Pakistan.svg",
          height = "60px") # Flag of Pakistan icon
    ),
    h4("Loading Healthy Diet Affordability Database", style = "margin-bottom: 20px;"),
    spin_orbiter(),
    p("Preparing data for analysis...", style = "margin-top: 20px;")
  )
)

# Load the Healthy Diet data from the CSV file
# Using read_csv from readr for better default parsing
data <- read_csv("Cost and Affordability of a Healthy Diet.csv")

# Filter for 'Pakistan' as the Area, if it contains other areas
data <- data %>% filter(Area == "Pakistan")

# Select relevant columns and clean names for easier use
data <- data %>%
  select(Year, Item, Value, Unit) %>%
  rename(Indicator = Item) # Renaming Item to Indicator for consistency with previous app logic

# Get unique indicators and years
available_indicators <- unique(data$Indicator)
available_years <- unique(data$Year)

# ======================
# Custom Theme & Styling - Warm Earth Tones
# ======================
custom_css <- "
/* Main styling */
body {
  font-family: 'Inter', sans-serif;
  background-color: #f5f5dc; /* Light Cream */
  color: #3e2723; /* Dark Brown */
}

/* Header styling with warm gradient */
.skin-blue .main-header .navbar {
  background: linear-gradient(135deg, #795548 0%, #5d4037 100%) !important; /* Brown gradient */
}

.skin-blue .main-header .logo {
  background-color: transparent !important;
  color: #fff !important;
  font-weight: 700;
}

.skin-blue .main-header .logo:hover {
  background-color: transparent !important;
}

/* Professional sidebar */
.skin-blue .main-sidebar {
  background-color: #4e342e !important; /* Darker Brown */
}

.skin-blue .sidebar-menu > li > a {
  border-left: 3px solid transparent;
  color: #ede0d4; /* Off-white for sidebar text */
}

.skin-blue .sidebar-menu > li.active > a, 
.skin-blue .sidebar-menu > li > a:hover {
  border-left-color: #ff9800; /* Orange accent */
  background-color: #6d4c41 !important; /* Medium Brown */
  color: #fff;
}

/* Enhanced boxes with subtle shadows */
.box {
  border-top: 3px solid #ff9800; /* Orange top border */
  border-radius: 8px;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08) !important;
  transition: transform 0.2s, box-shadow 0.2s;
}

.box:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 16px rgba(0, 0, 0, 0.12) !important;
}

.box-header {
  border-bottom: 1px solid #e0e0e0;
  padding: 15px;
  font-weight: 600;
  color: #3e2723; /* Dark Brown for headers */
}

/* Professional value boxes */
.small-box {
  border-radius: 8px;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
  transition: transform 0.2s;
}

.small-box:hover {
  transform: translateY(-3px);
}

.small-box h3 {
  font-size: 24px;
  font-weight: 700;
  color: #3e2723; /* Dark Brown */
}

.small-box .icon {
  font-size: 70px;
  top: -10px;
  color: rgba(62, 39, 35, 0.2); /* Semi-transparent dark brown */
}

/* Enhanced buttons */
.btn {
  border-radius: 4px;
  font-weight: 500;
  transition: all 0.2s;
}

.btn-primary {
  background-color: #ff9800; /* Orange */
  border-color: #ff9800;
  color: #fff;
}

.btn-primary:hover,
.btn-primary:focus {
  background-color: #fb8c00; /* Slightly darker orange */
  border-color: #fb8c00;
  color: #fff;
}

/* Switch styling for language toggle */
.form-switch .form-check-input {
  width: 2.5em;
  height: 1.25em;
  background-color: #ccc;
  border-radius: 1.25em;
  transition: all 0.2s ease-in-out;
  cursor: pointer;
}

.form-switch .form-check-input:checked {
  background-color: #ff9800; /* Orange when checked */
}

/* DT table styling */
.dataTables_wrapper .dataTables_filter input {
  border-radius: 4px;
  border: 1px solid #ccc;
  padding: 6px 12px;
}

/* Custom spinners */
.waiter-overlay-content .fa-spinner {
  font-size: 3em;
  color: #fff;
}

/* Footer styling */
.main-footer {
  background-color: #3e2723; /* Dark Brown */
  color: #ede0d4; /* Off-white */
  padding: 15px;
  text-align: center;
}
"
# ======================
# UI Definition
# ======================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = div(
      style = "text-align: center; padding: 10px;", # Centered the div content
      h4(textOutput("dashboard_title"), style = "color: white; margin: 0; padding: 0;"),
      h6("by ", strong("Owais Ali Shah"), style = "color: #ffcc80; margin: 0; padding: 0; font-size: 1.1em;") # Highlighted name
    ),
    titleWidth = 350,
    tags$li(
      class = "dropdown",
      div(
        style = "margin-top: 10px; margin-right: 15px; display: inline-block;",
        span(style = "color:white;", "EN"),
        switchInput(
          inputId = "lang_toggle",
          value = FALSE,
          size = "mini"
        ),
        span(style = "color:white;", "UR")
      )
    )
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem(textOutput("overview_details"), tabName = "overview_details", icon = icon("info-circle")),
      menuItem(textOutput("trends"), tabName = "trends", icon = icon("chart-line")),
      menuItem(textOutput("explorer"), tabName = "explorer", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css))
    ),
    use_waiter(),
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "overview_details",
        fluidRow(
          valueBoxOutput("total_items_box"),
          valueBoxOutput("time_period_box"),
          valueBoxOutput("data_points_box"),
          valueBoxOutput("avg_value_overview_box")
        ),
        fluidRow(
          box(
            title = textOutput("key_indicators"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            column(
              width = 7,
              withSpinner(highchartOutput("overview_chart", height = "400px"))
            ),
            column(
              width = 5,
              h4(textOutput("quick_insights")),
              htmlOutput("insights_text", class = "insights-box"),
              br(),
              downloadButton("download_overview_data", textOutput("download_data"), class = "btn-primary")
            )
          )
        ),
        fluidRow(
          box(
            title = textOutput("recent_updates"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("latest_data_table")
          )
        ),
        # Merged content from the 'About' tab
        fluidRow(
          box(
            title = textOutput("about_title"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h4(strong("Project Details")),
            HTML(paste0("<h4>", textOutput("about_project_details"), "</h4>")),
            br(),
            h4(strong("About the Author")),
            HTML(paste0("<h4>", textOutput("about_author_details"), "</h4>"))
          )
        )
      ),
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = textOutput("trend_options"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            column(
              width = 4,
              selectInput("item_select", textOutput("select_item"),
                          choices = available_indicators)
            ),
            column(
              width = 4,
              sliderInput("year_range", textOutput("year_range"),
                          min = min(available_years), max = max(available_years),
                          value = range(available_years), sep = "")
            ),
            column(
              width = 4,
              radioButtons("trend_type", "Select Chart Type:",
                           choices = c("Line" = "line", "Area" = "area", "Bar" = "column"),
                           selected = "line", inline = TRUE)
            )
          )
        ),
        fluidRow(
          box(
            title = textOutput("item_trends"),
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            withSpinner(highchartOutput("trend_chart", height = "400px"))
          ),
          box(
            title = textOutput("statistical_summary"),
            status = "info",
            solidHeader = TRUE,
            width = 4,
            verbatimTextOutput("trend_summary")
          )
        )
      ),
      tabItem(
        tabName = "explorer",
        fluidRow(
          box(
            title = textOutput("indicator_data"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            withSpinner(DTOutput("data_table"))
          )
        )
      )
    ),
    # Footer section with your name
    tags$footer(
      tags$div(
        class = "main-footer",
        HTML("© 2025 Owais Ali Shah - All rights reserved.")
      )
    )
  )
)

# ======================
# Server Logic
# ======================
server <- function(input, output, session) {
  
  # Reactive value for current language
  current_language <- reactiveVal("en")
  
  # Initialize waiter
  waiter <- Waiter$new(
    id = c("overview_chart", "trend_chart", "data_table", "latest_data_table"),
    html = waiting_screen,
    color = "#3e2723" # Dark Brown for waiter
  )
  
  # Observe language toggle
  observeEvent(input$lang_toggle, {
    if (input$lang_toggle) {
      current_language("ur")
      showNotification("Language switched to Urdu", type = "message")
    } else {
      current_language("en")
      showNotification("Language switched to English", type = "message")
    }
  })
  
  # Function to get translated text
  t <- function(key) {
    translations[[current_language()]][[key]]
  }
  
  # Reactive UI text elements
  output$dashboard_title <- renderText({ t("dashboard_title") })
  output$trends <- renderText({ t("trends") })
  output$explorer <- renderText({ t("explorer") })
  output$overview_details <- renderText({ t("overview_details") })
  output$about_title <- renderText({ t("about_title") })
  output$about_project_details <- renderText({ t("about_project_details") })
  output$about_author_details <- renderText({ t("about_author_details") })
  output$select_item <- renderText({ t("select_item") })
  output$year_range <- renderText({ t("year_range") })
  output$key_indicators <- renderText({ t("key_indicators") })
  output$quick_insights <- renderText({ t("quick_insights") })
  output$recent_updates <- renderText({ t("recent_updates") })
  output$download_data <- renderText({ t("download_data") })
  output$trend_options <- renderText({ t("trend_options") })
  output$item_trends <- renderText({ t("item_trends") })
  output$statistical_summary <- renderText({ t("statistical_summary") })
  output$indicator_data <- renderText({ t("indicator_data") })
  
  # Reactive data based on filters for Trend Analysis
  filtered_data <- reactive({
    req(input$item_select) # Ensure an item is selected
    data %>%
      filter(
        Indicator == input$item_select,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
  })
  
  # ================= Value Boxes =================
  output$total_items_box <- renderValueBox({
    valueBox(
      value = length(unique(data$Indicator)),
      subtitle = t("total_items"),
      icon = icon("list"),
      color = "orange", # Using a warm tone
      width = 3
    )
  })
  
  output$time_period_box <- renderValueBox({
    valueBox(
      value = paste(min(data$Year), "-", max(data$Year)),
      subtitle = t("time_period"),
      icon = icon("calendar-alt"),
      color = "green", # Using a standard color that blends
      width = 3
    )
  })
  
  output$data_points_box <- renderValueBox({
    valueBox(
      value = format(nrow(data), big.mark = ","),
      subtitle = t("data_points"),
      icon = icon("database"),
      color = "maroon", # Using a warm tone
      width = 3
    )
  })
  
  output$avg_value_overview_box <- renderValueBox({
    # Calculate average of a default indicator for the overview box, e.g., 'Cost of a healthy diet (CoHD)'
    default_indicator_avg <- data %>%
      filter(Indicator == "Cost of a healthy diet (CoHD), LCU per person per day") %>%
      pull(Value) %>%
      mean(na.rm = TRUE)
    
    valueBox(
      value = round(default_indicator_avg, 2),
      subtitle = paste(t("average_value"), "(CoHD)"), # Specify for which indicator
      icon = icon("calculator"),
      color = "olive", # Using a standard color that blends
      width = 3
    )
  })
  
  # ================= Overview Tab Content =================
  
  # Latest data table for overview
  output$latest_data_table <- renderDT({
    waiter$show()
    on.exit(waiter$hide())
    
    # Get the latest year available
    latest_year <- max(data$Year, na.rm = TRUE)
    
    latest_data <- data %>%
      filter(Year == latest_year) %>%
      select(Indicator, Year, Value, Unit) %>%
      arrange(Indicator) %>%
      rename(
        Indicator = Indicator,
        Year = Year,
        `Latest Value` = Value,
        Unit = Unit
      )
    
    datatable(
      latest_data,
      options = list(
        dom = 't', # Only show table
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 'Latest Value', digits = 2)
  })
  
  # Quick Insights text
  output$insights_text <- renderUI({
    # Insights based on 'Cost of a healthy diet (CoHD), LCU per person per day'
    cohd_data <- data %>%
      filter(Indicator == "Cost of a healthy diet (CoHD), LCU per person per day")
    
    if (nrow(cohd_data) == 0) {
      return(HTML("<div style='font-size: 14px;'><p>No data available for Cost of a Healthy Diet.</p></div>"))
    }
    
    avg_cohd <- mean(cohd_data$Value, na.rm = TRUE)
    # Fix: Reference cohd_data instead of cohd
    min_cohd <- min(cohd_data$Value, na.rm = TRUE) 
    max_cohd <- max(cohd_data$Value, na.rm = TRUE) 
    
    # Trend for CoHD
    cohd_by_year <- cohd_data %>%
      group_by(Year) %>%
      summarize(Avg_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(Year)
    
    trend_text <- "stable"
    if(nrow(cohd_by_year) >= 2) {
      if(last(cohd_by_year$Avg_Value) > first(cohd_by_year$Avg_Value)) {
        trend_text <- "increasing"
      } else if (last(cohd_by_year$Avg_Value) < first(cohd_by_year$Avg_Value)) {
        trend_text <- "decreasing"
      }
    }
    
    HTML(paste0(
      "<div style='font-size: 14px; line-height: 1.6'>",
      "<p><strong>Indicator:</strong> Cost of a Healthy Diet (CoHD)</p>",
      "<p><strong>Trend:</strong> <span style='color:", ifelse(trend_text == "increasing", "#b71c1c", "#1b5e20"), "'>", trend_text, "</span></p>", # Red for increasing cost, Green for decreasing
      "<p><strong>Range (LCU/cap/d):</strong> ", round(min_cohd, 2), " to ", round(max_cohd, 2), "</p>",
      "<p><strong>Average (LCU/cap/d):</strong> ", round(avg_cohd, 2), "</p>",
      "<p><strong>Latest Value (", max(cohd_by_year$Year), "):</strong> ",
      round(last(cohd_by_year$Avg_Value), 2),
      " LCU/cap/d</p>",
      "</div>"
    ))
  })
  
  # Overview chart - Average value per indicator
  output$overview_chart <- renderHighchart({
    waiter$show()
    on.exit(waiter$hide())
    
    summary_data <- data %>%
      group_by(Indicator) %>%
      summarize(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Average_Value))
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = t("key_indicators")) %>%
      hc_xAxis(categories = summary_data$Indicator, title = list(text = "Indicator")) %>%
      hc_yAxis(title = list(text = "Average Value")) %>%
      hc_add_series(
        name = "Average Value",
        data = summary_data$Average_Value,
        color = "#ff9800" # Orange accent
      ) %>%
      hc_tooltip(
        valueDecimals = 2,
        shared = TRUE,
        crosshairs = TRUE,
        pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.2f}</b><br/>'
      ) %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = FALSE)))
  })
  
  # Download handler for raw data
  output$download_overview_data <- downloadHandler(
    filename = function() {
      paste0("healthy_diet_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # ================= Trend Analysis Tab Content =================
  
  # Trend chart for selected item
  output$trend_chart <- renderHighchart({
    waiter$show()
    on.exit(waiter$hide())
    
    current_data <- filtered_data()
    
    if (nrow(current_data) == 0) {
      return(highchart() %>% hc_title(text = "No data available for the selected indicator and year range."))
    }
    
    # Aggregate data by year, in case an indicator has multiple entries per year (e.g., from different releases)
    plot_data <- current_data %>%
      group_by(Year) %>%
      summarise(Aggregated_Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
      arrange(Year)
    
    # Determine unit for Y-axis label
    selected_unit <- unique(current_data$Unit)[1]
    
    hc_type <- ifelse(input$trend_type == "line", "line",
                      ifelse(input$trend_type == "area", "area", "column"))
    
    highchart() %>%
      hc_chart(type = hc_type) %>%
      hc_title(text = paste(t("item_trends"), ":", input$item_select)) %>%
      hc_xAxis(categories = plot_data$Year, title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = paste("Value (", selected_unit, ")"))) %>%
      hc_add_series(
        name = input$item_select,
        data = plot_data$Aggregated_Value,
        color = "#a1887f" # Medium Brown
      ) %>%
      hc_tooltip(
        valueDecimals = 2,
        shared = TRUE,
        crosshairs = TRUE,
        # Corrected pointFormat to use paste0
        pointFormat = paste0('<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.2f} ', selected_unit, '</b><br/>')
      ) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # Trend summary for selected item
  output$trend_summary <- renderPrint({
    current_data <- filtered_data()
    summary(current_data$Value)
  })
  
  # ================= Data Explorer Tab Content =================
  
  # Data table for raw data
  output$data_table <- renderDT({
    waiter$show()
    on.exit(waiter$hide())
    
    datatable(
      data,
      extensions = c('Buttons', 'Scroller', 'Responsive'),
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', className = 'btn-sm'),
          list(extend = 'csv', className = 'btn-sm'),
          list(extend = 'excel', className = 'btn-sm'),
          list(extend = 'pdf', className = 'btn-sm')
        ),
        scrollX = TRUE,
        scrollY = "500px",
        scroller = TRUE,
        pageLength = 10,
        responsive = TRUE,
        autoWidth = TRUE,
        language = list(
          search = "Filter:",
          paginate = list(
            `first` = "First", `last` = "Last", `next` = "Next", `previous` = "Previous"
          )
        )
      ),
      class = "cell-border stripe hover",
      rownames = FALSE,
      filter = 'top'
    ) %>%
      formatRound(columns = 'Value', digits = 2)
  })
}

shinyApp(ui, server)
