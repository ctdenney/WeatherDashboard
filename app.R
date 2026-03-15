library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(openair)
library(RMySQL)
library(magick)
library(plotly)

rain_corr = 1.255411 #needs to be re-calculated for new rain gauge

Sys.setenv(tz = 'America/Los_Angeles')

# Database configuration

credentials = read.csv('credentials.csv')
db_user = credentials[credentials$field == 'db_user',2]
db_password = credentials[credentials$field == 'db_password',2]
db_host = credentials[credentials$field == 'db_host',2]

# Current database (new weather station)
current_db_name = 'ADS_WS1'
current_db_table = 'weather_data'

rain_gauge_db = 'rain_gauge_db'
rain_gauge_table = 'rainfall'

# Old database (historical data)
old_db_name = 'SkyWeather2'
old_db_table = 'WeatherData'

# Cutoff date between old and new systems
cutoff_date = as.POSIXct('2024-12-02', tz = 'America/Los_Angeles')

# Helper function to get data from specific database
get_database_data = function(db_name, db_table, start_date = NULL, end_date = NULL, is_old_db = FALSE) {
  mydb = dbConnect(
    MySQL(),
    user = db_user, 
    password = db_password, 
    dbname = db_name, 
    host = db_host
  )
  
  # Build SQL query with date filtering
  base_query = paste0("SELECT * FROM ", db_table)
  
  if (!is.null(start_date) || !is.null(end_date)) {
    where_clauses = c()
    
    if (!is.null(start_date)) {
      # Convert PST to UTC for database query
      start_utc = with_tz(start_date, "UTC")
      where_clauses = c(where_clauses, paste0("TimeStamp >= '", format(start_utc, '%Y-%m-%d %H:%M:%S'), "'"))
    }
    
    if (!is.null(end_date)) {
      # Convert PST to UTC for database query
      end_utc = with_tz(end_date, "UTC")
      where_clauses = c(where_clauses, paste0("TimeStamp <= '", format(end_utc, '%Y-%m-%d %H:%M:%S'), "'"))
    }
    
    if (length(where_clauses) > 0) {
      base_query = paste(base_query, "WHERE", paste(where_clauses, collapse = " AND "))
    }
  }
  
  rs = dbSendQuery(mydb, base_query)
  df = fetch(rs, n = -1)
  dbClearResult(rs)
  dbDisconnect(mydb)
  
  if (nrow(df) == 0) {
    return(data.frame())
  }
  
  # Process data based on database type
  if (is_old_db) {
    # Old database processing (keeping your original logic)
    data = df %>%
      filter(
        ID > 6,
        (SerialNumber %in% c(116, 236, 72, 252) & ID < 166030) |
          (SerialNumber %in% c(181) & ID >= 166030)
      ) %>%
      mutate(
        ts_GMT = force_tz(as_datetime(TimeStamp), tzone = 'GMT'),
        ts_PST = with_tz(ts_GMT, tzone = 'America/Los_Angeles'),
        TotalRain = case_when(
          ID <= 676 ~ 0,
          ID >= 37254 & ID <= 37269 & TotalRain != 355.8 ~ TotalRain + 355.8,
          ID > 37269 & ID < 88267 ~ TotalRain + 355.8,
          ID >= 88267 & ID <= 114820 ~ TotalRain + 596.7,
          ID > 114820 & ID < 166030 ~ TotalRain + 921.9,
          ID >= 166030 ~ TotalRain + 1058.7,
          T ~ TotalRain
        ),
        InstantRain = c(0, diff(TotalRain)),
        SunlightVisible = case_when(
          ts_PST < '2022-02-25' ~ as.numeric(NA),
          SunlightVisible > 200000 ~ as.numeric(NA),
          T ~ SunlightVisible
        ),
        SunlightUVIndex = case_when(
          ts_PST < '2022-02-25' ~ as.numeric(NA),
          T ~ SunlightUVIndex 
        ),
        # Convert old units: centibar to millibar, m/s to kph
        BarometricPressure = BarometricPressure * 10, # centibar to millibar
        WindSpeed = WindSpeed * 3.6, # m/s to kph
        WindGust = WindGust * 3.6 # m/s to kph
      ) %>%
      select(
        ID, TimeStamp, ts_PST, OutdoorTemperature, OutdoorHumidity, 
        IndoorTemperature, IndoorHumidity, TotalRain, InstantRain, 
        SunlightVisible, SunlightUVIndex, WindGust, WindDirection, 
        WindSpeed, BarometricPressure, BarometricPressureSeaLevel, 
        BarometricTemperature, PM_1_0, PM_2_5, PM_10, AQI, AQI24Average, SNR
      ) %>%
      filter(ts_PST > '2022-02-23 17:00:00')
    
  } else {
    # New database processing
    data = df %>%
      mutate(
        ts_GMT = force_tz(as_datetime(TimeStamp), tzone = 'GMT'),
        ts_PST = with_tz(ts_GMT, tzone = 'America/Los_Angeles'),
        InstantRain = TotalRain, # In new system, TotalRain is actually instantaneous
        # Add missing columns as NA for compatibility
        IndoorTemperature = as.numeric(NA),
        IndoorHumidity = as.numeric(NA),
        SunlightVisible = as.numeric(NA),
        SunlightUVIndex = as.numeric(NA),
        BarometricPressureSeaLevel = as.numeric(NA),
        BarometricTemperature = as.numeric(NA),
        PM_1_0 = as.numeric(NA),
        PM_2_5 = as.numeric(NA),
        PM_10 = as.numeric(NA),
        AQI = as.numeric(NA),
        AQI24Average = as.numeric(NA),
        SNR = as.numeric(NA),
        ID = row_number() # Create a dummy ID for compatibility
      ) %>%
      select(
        ID, TimeStamp, ts_PST, OutdoorTemperature, OutdoorHumidity, 
        IndoorTemperature, IndoorHumidity, TotalRain, InstantRain, 
        SunlightVisible, SunlightUVIndex, WindGust, WindDirection, 
        WindSpeed, BarometricPressure, BarometricPressureSeaLevel, 
        BarometricTemperature, PM_1_0, PM_2_5, PM_10, AQI, AQI24Average, SNR
      )
  }
  
  return(data)
}

# Check if user is on home network
is_home_network <- function(session) {
  client_ip <- session$request$HTTP_X_FORWARDED_FOR
  if (is.null(client_ip)) {
    client_ip <- session$request$REMOTE_ADDR
  }
  
  # Check if IP is in your home network range (192.168.0.x based on your server IP)
  grepl("^192\\.168\\.0\\.", client_ip) || 
    client_ip == "127.0.0.1" ||
    client_ip == "::1"
}

# Get rain gauge data
get_rain_gauge_data <- function(start_date = NULL, end_date = NULL) {
  mydb = dbConnect(
    MySQL(),
    user = db_user, 
    password = db_password, 
    dbname = rain_gauge_db, 
    host = db_host
  )
  
  query = "SELECT * FROM rainfall"
  
  if (!is.null(start_date) || !is.null(end_date)) {
    where_clauses = c()
    
    if (!is.null(start_date)) {
      where_clauses = c(where_clauses, paste0("date >= '", format(start_date, '%Y-%m-%d'), "'"))
    }
    
    if (!is.null(end_date)) {
      where_clauses = c(where_clauses, paste0("date <= '", format(end_date, '%Y-%m-%d'), "'"))
    }
    
    if (length(where_clauses) > 0) {
      query = paste(query, "WHERE", paste(where_clauses, collapse = " AND "))
    }
  }
  
  rs = dbSendQuery(mydb, query)
  df = fetch(rs, n = -1)
  dbClearResult(rs)
  dbDisconnect(mydb)
  
  if (nrow(df) > 0) {
    df$date = as.Date(df$date)
  }
  
  return(df)
}

get_water_year <- function(date) {
  # Water year starts October 1st
  # October 2023 is water year 2024
  year(date) + ifelse(month(date) >= 10, 1, 0)
}

get_water_year_month <- function(date) {
  # October = 1, November = 2, ..., September = 12
  ((month(date) + 2) %% 12) + 1
}

water_year_months <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", 
                       "Apr", "May", "Jun", "Jul", "Aug", "Sep")

# Main function to get data across both databases
get_recent_data = function(start_date = NULL, end_date = NULL) {
  if (is.null(start_date)) {
    start_date = as.POSIXct(Sys.Date(), tz = 'America/Los_Angeles') # Today at midnight
  }
  if (is.null(end_date)) {
    end_date = Sys.time() # Current time
  }
  
  # Convert to POSIXct if they're Date objects
  if (class(start_date)[1] == "Date") {
    start_date = as.POSIXct(start_date, tz = 'America/Los_Angeles')
  }
  if (class(end_date)[1] == "Date") {
    end_date = as.POSIXct(end_date, tz = 'America/Los_Angeles')
  }
  
  # Determine which databases to query
  new_data = data.frame()
  old_data = data.frame()
  
  # Query new database if date range extends into new period
  if (end_date >= cutoff_date) {
    new_start = max(start_date, cutoff_date)
    new_data = get_database_data(current_db_name, current_db_table, new_start, end_date, is_old_db = FALSE)
  }
  
  # Query old database if date range extends into old period
  if (start_date < cutoff_date) {
    old_end = min(end_date, cutoff_date - days(1))
    old_data = get_database_data(old_db_name, old_db_table, start_date, old_end, is_old_db = TRUE)
  }
  
  # Combine data
  combined_data = bind_rows(old_data, new_data) %>%
    arrange(ts_PST)
  
  return(combined_data)
}

abs_hum = function(rel_hum, temp_c) {
  SVP = 6.11 * exp(((2500000)/461.52)*(1/273.15 - 1/(temp_c+273.15)))
  WVP2 = rel_hum * SVP
  ah = WVP2/(461.52*(temp_c+273.15))
  return(ah*1000)
}

# UI ####
ui = dashboardPage(
  dashboardHeader(title = 'Cartagena Farm Weather Station'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Current Conditions', tabName = 'conds', icon = icon('wind')),
      menuItem('Detailed Plots', tabName = 'plots', icon = icon('chart-line')),
      menuItem('Long-term Summary', tabName = 'longterm', icon = icon('chart-bar'))
    ),
    dateRangeInput(
      'range','Date Range'#,
      #start = Sys.Date(), 
      #end = Sys.Date()
    ),
    selectInput(
      inputId = 'hourstart',
      label = 'Hour Start',
      choices = seq(0,24,1),
      selected = 0
    ),
    selectInput(
      inputId = 'hourend',
      label = 'Hour End',
      choices = seq(0,24,1),
      selected = 24
    ),
    selectInput(
      inputId = 'resolution',label = "Select Temporal Resolution",
      choices = c('5 minutes','Hourly','Daily','Weekly','Monthly'),
      selected = '5 minutes'
    ),
    checkboxInput('smooth','Show Trend Line?',FALSE),
    sliderInput('smoothness','Level of smoothing',min = 0.02,max = 0.5, value = 0.05),
    checkboxInput('rain_correct', "Correct Rainfall?", FALSE),
    checkboxInput('high_low', 'Show High/Low?', FALSE)
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'conds',
        fluidRow(
          column(
            4,
            div(textOutput('CurrentHeader'),style = 'font-size:200%'),
            div(tableOutput('CurrentConditions1'), style = 'font-size:150%'),
            div(tableOutput('CurrentConditions2'), style = 'font-size:150%'),
            div(tableOutput('CurrentConditions3'), style = 'font-size:150%'),
            div(tableOutput('CurrentConditions4'), style = 'font-size:150%'),
            div(tableOutput('CurrentConditions5'), style = 'font-size:150%')
          ),
          column(
            8,
            imageOutput(outputId = 'farmpic')
          )
        )
      ),
      tabItem(
        tabName = 'plots',
        fluidRow(
          column(
            3,
            plotOutput('TempPlot'),
            conditionalPanel(
              condition = "output.hasAQI",
              plotOutput('AQIPlot')
            )
          ),
          column(
            3,
            plotOutput('RainPlot'),
            plotOutput('PressurePlot')
          ),
          column(
            3,
            plotOutput('HumPlot'),
            plotOutput('WindPlot')
          ),
          column(
            3,
            conditionalPanel(
              condition = "output.hasLight",
              plotOutput('LightPlot')
            ),
            plotOutput('WindRose')
          )
        )
      ),
      # In the longterm tabItem:
      tabItem(
        tabName = 'longterm',
        fluidRow(
          box(
            title = "Annual Rainfall by Water Year",
            width = 12,
            plotOutput("annual_rainfall_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Monthly Rainfall Patterns",
            width = 12,
            plotlyOutput("monthly_rainfall_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Water Year Rainfall Statistics",
            width = 12,
            DT::dataTableOutput("rainfall_statistics")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(session$clientData, {
    updateDateRangeInput(session, 'range',
      start = as.Date(Sys.time(), tz = "America/Los_Angeles"),
      end = as.Date(Sys.time(), tz = "America/Los_Angeles")
    )
  }, once = TRUE, ignoreNULL = FALSE)

  # Reactive data fetching with debugging
  recent_data <- reactive({
    # Get date range from inputs
    start_time = as.POSIXct(as.character(input$range[1]), tz = 'America/Los_Angeles') + 
      3600 * as.numeric(input$hourstart)
    end_time = as.POSIXct(as.character(input$range[2]), tz = 'America/Los_Angeles') + 
      3600 * as.numeric(input$hourend)
    
    # Debug print
    cat("Fetching data from:", as.character(start_time), "to", as.character(end_time), "\n")
    
    data = get_recent_data(start_time, end_time)
    cat("Data rows returned:", nrow(data), "\n")
    
    # Filter data to exact time range in case database returned extra
    if (nrow(data) > 0) {
      data = data %>%
        filter(ts_PST >= start_time, ts_PST <= end_time)
      cat("Data rows after PST filtering:", nrow(data), "\n")
    }
    
    return(data)
  })
  
  # Reactive processed data based on resolution
  processed_data <- reactive({
    data = recent_data()
    resolution = input$resolution
    
    if (nrow(data) == 0) return(data)
    
    cat("Processing data with resolution:", resolution, "\n")
    
    if (resolution == '5 minutes') {
      return(data)
    } else if (resolution == 'Hourly') {
      result = data %>%
        mutate(
          hour = hour(ts_PST), 
          date = date(ts_PST)
        ) %>%
        group_by(date, hour) %>%
        summarise(
          ts_PST = first(ts_PST),
          # Add min/max for high/low display
          min_ot = min(OutdoorTemperature, na.rm = TRUE),
          max_ot = max(OutdoorTemperature, na.rm = TRUE),
          min_it = min(IndoorTemperature, na.rm = TRUE),
          max_it = max(IndoorTemperature, na.rm = TRUE),
          min_oh = min(OutdoorHumidity, na.rm = TRUE),
          max_oh = max(OutdoorHumidity, na.rm = TRUE),
          min_ih = min(IndoorHumidity, na.rm = TRUE),
          max_ih = max(IndoorHumidity, na.rm = TRUE),
          min_bar = min(BarometricPressure, na.rm = TRUE),
          max_bar = max(BarometricPressure, na.rm = TRUE),
          min_ws = min(WindSpeed, na.rm = TRUE),
          max_ws = max(WindSpeed, na.rm = TRUE),
          min_wg = min(WindGust, na.rm = TRUE),
          max_wg = max(WindGust, na.rm = TRUE),
          min_light = min(SunlightVisible, na.rm = TRUE),
          max_light = max(SunlightVisible, na.rm = TRUE),
          min_aqi = min(AQI, na.rm = TRUE),
          max_aqi = max(AQI, na.rm = TRUE),
          OutdoorTemperature = mean(OutdoorTemperature, na.rm = TRUE),
          OutdoorHumidity = mean(OutdoorHumidity, na.rm = TRUE),
          IndoorTemperature = mean(IndoorTemperature, na.rm = TRUE),
          IndoorHumidity = mean(IndoorHumidity, na.rm = TRUE),
          BarometricPressure = mean(BarometricPressure, na.rm = TRUE),
          WindSpeed = mean(WindSpeed, na.rm = TRUE),
          WindGust = mean(WindGust, na.rm = TRUE),
          WindDirection = mean(WindDirection, na.rm = TRUE),
          InstantRain = sum(InstantRain, na.rm = TRUE),
          TotalRain = sum(InstantRain, na.rm = TRUE), # For compatibility
          SunlightVisible = mean(SunlightVisible, na.rm = TRUE),
          SunlightUVIndex = mean(SunlightUVIndex, na.rm = TRUE),
          AQI = mean(AQI, na.rm = TRUE),
          .groups = 'drop'
        )
      # cat("Hourly data processed. Columns:", paste(names(result), collapse = ", "), "\n")
      # cat("Has min_ot column:", 'min_ot' %in% names(result), "\n")
      return(result)
    } else if (resolution == 'Daily') {
      data %>%
        mutate(date = date(ts_PST)) %>%
        group_by(date) %>%
        summarise(
          ts_PST = first(ts_PST),
          # Add min/max for high/low display
          min_ot = min(OutdoorTemperature, na.rm = TRUE),
          max_ot = max(OutdoorTemperature, na.rm = TRUE),
          min_it = min(IndoorTemperature, na.rm = TRUE),
          max_it = max(IndoorTemperature, na.rm = TRUE),
          min_oh = min(OutdoorHumidity, na.rm = TRUE),
          max_oh = max(OutdoorHumidity, na.rm = TRUE),
          min_ih = min(IndoorHumidity, na.rm = TRUE),
          max_ih = max(IndoorHumidity, na.rm = TRUE),
          min_bar = min(BarometricPressure, na.rm = TRUE),
          max_bar = max(BarometricPressure, na.rm = TRUE),
          min_ws = min(WindSpeed, na.rm = TRUE),
          max_ws = max(WindSpeed, na.rm = TRUE),
          min_wg = min(WindGust, na.rm = TRUE),
          max_wg = max(WindGust, na.rm = TRUE),
          min_light = min(SunlightVisible, na.rm = TRUE),
          max_light = max(SunlightVisible, na.rm = TRUE),
          min_aqi = min(AQI, na.rm = TRUE),
          max_aqi = max(AQI, na.rm = TRUE),
          OutdoorTemperature = mean(OutdoorTemperature, na.rm = TRUE),
          OutdoorHumidity = mean(OutdoorHumidity, na.rm = TRUE),
          IndoorTemperature = mean(IndoorTemperature, na.rm = TRUE),
          IndoorHumidity = mean(IndoorHumidity, na.rm = TRUE),
          BarometricPressure = mean(BarometricPressure, na.rm = TRUE),
          WindSpeed = mean(WindSpeed, na.rm = TRUE),
          WindGust = mean(WindGust, na.rm = TRUE),
          WindDirection = mean(WindDirection, na.rm = TRUE),
          InstantRain = sum(InstantRain, na.rm = TRUE),
          TotalRain = sum(InstantRain, na.rm = TRUE),
          SunlightVisible = mean(SunlightVisible, na.rm = TRUE),
          SunlightUVIndex = mean(SunlightUVIndex, na.rm = TRUE),
          AQI = mean(AQI, na.rm = TRUE),
          .groups = 'drop'
        )
    } else if (resolution == 'Weekly') {
      data %>%
        mutate(
          week = week(ts_PST), 
          year = year(ts_PST)
        ) %>%
        group_by(year, week) %>%
        summarise(
          ts_PST = first(ts_PST),
          # Add min/max for high/low display
          min_ot = min(OutdoorTemperature, na.rm = TRUE),
          max_ot = max(OutdoorTemperature, na.rm = TRUE),
          min_it = min(IndoorTemperature, na.rm = TRUE),
          max_it = max(IndoorTemperature, na.rm = TRUE),
          min_oh = min(OutdoorHumidity, na.rm = TRUE),
          max_oh = max(OutdoorHumidity, na.rm = TRUE),
          min_ih = min(IndoorHumidity, na.rm = TRUE),
          max_ih = max(IndoorHumidity, na.rm = TRUE),
          min_bar = min(BarometricPressure, na.rm = TRUE),
          max_bar = max(BarometricPressure, na.rm = TRUE),
          min_ws = min(WindSpeed, na.rm = TRUE),
          max_ws = max(WindSpeed, na.rm = TRUE),
          min_wg = min(WindGust, na.rm = TRUE),
          max_wg = max(WindGust, na.rm = TRUE),
          min_light = min(SunlightVisible, na.rm = TRUE),
          max_light = max(SunlightVisible, na.rm = TRUE),
          min_aqi = min(AQI, na.rm = TRUE),
          max_aqi = max(AQI, na.rm = TRUE),
          OutdoorTemperature = mean(OutdoorTemperature, na.rm = TRUE),
          OutdoorHumidity = mean(OutdoorHumidity, na.rm = TRUE),
          IndoorTemperature = mean(IndoorTemperature, na.rm = TRUE),
          IndoorHumidity = mean(IndoorHumidity, na.rm = TRUE),
          BarometricPressure = mean(BarometricPressure, na.rm = TRUE),
          WindSpeed = mean(WindSpeed, na.rm = TRUE),
          WindGust = mean(WindGust, na.rm = TRUE),
          WindDirection = mean(WindDirection, na.rm = TRUE),
          InstantRain = sum(InstantRain, na.rm = TRUE),
          TotalRain = sum(InstantRain, na.rm = TRUE),
          SunlightVisible = mean(SunlightVisible, na.rm = TRUE),
          SunlightUVIndex = mean(SunlightUVIndex, na.rm = TRUE),
          AQI = mean(AQI, na.rm = TRUE),
          .groups = 'drop'
        )
    } else if (resolution == 'Monthly') {
      data %>%
        mutate(
          month = month(ts_PST), 
          year = year(ts_PST)
        ) %>%
        group_by(year, month) %>%
        summarise(
          ts_PST = first(ts_PST),
          # Add min/max for high/low display
          min_ot = min(OutdoorTemperature, na.rm = TRUE),
          max_ot = max(OutdoorTemperature, na.rm = TRUE),
          min_it = min(IndoorTemperature, na.rm = TRUE),
          max_it = max(IndoorTemperature, na.rm = TRUE),
          min_oh = min(OutdoorHumidity, na.rm = TRUE),
          max_oh = max(OutdoorHumidity, na.rm = TRUE),
          min_ih = min(IndoorHumidity, na.rm = TRUE),
          max_ih = max(IndoorHumidity, na.rm = TRUE),
          min_bar = min(BarometricPressure, na.rm = TRUE),
          max_bar = max(BarometricPressure, na.rm = TRUE),
          min_ws = min(WindSpeed, na.rm = TRUE),
          max_ws = max(WindSpeed, na.rm = TRUE),
          min_wg = min(WindGust, na.rm = TRUE),
          max_wg = max(WindGust, na.rm = TRUE),
          min_light = min(SunlightVisible, na.rm = TRUE),
          max_light = max(SunlightVisible, na.rm = TRUE),
          min_aqi = min(AQI, na.rm = TRUE),
          max_aqi = max(AQI, na.rm = TRUE),
          OutdoorTemperature = mean(OutdoorTemperature, na.rm = TRUE),
          OutdoorHumidity = mean(OutdoorHumidity, na.rm = TRUE),
          IndoorTemperature = mean(IndoorTemperature, na.rm = TRUE),
          IndoorHumidity = mean(IndoorHumidity, na.rm = TRUE),
          BarometricPressure = mean(BarometricPressure, na.rm = TRUE),
          WindSpeed = mean(WindSpeed, na.rm = TRUE),
          WindGust = mean(WindGust, na.rm = TRUE),
          WindDirection = mean(WindDirection, na.rm = TRUE),
          InstantRain = sum(InstantRain, na.rm = TRUE),
          TotalRain = sum(InstantRain, na.rm = TRUE),
          SunlightVisible = mean(SunlightVisible, na.rm = TRUE),
          SunlightUVIndex = mean(SunlightUVIndex, na.rm = TRUE),
          AQI = mean(AQI, na.rm = TRUE),
          .groups = 'drop'
        )
    }
  })
  
  # Check if we have light data
  output$hasLight <- reactive({
    data = recent_data()
    if (nrow(data) == 0) return(FALSE)
    any(!is.na(data$SunlightVisible))
  })
  outputOptions(output, "hasLight", suspendWhenHidden = FALSE)
  
  # Check if we have AQI data  
  output$hasAQI <- reactive({
    data = recent_data()
    if (nrow(data) == 0) return(FALSE)
    any(!is.na(data$AQI))
  })
  outputOptions(output, "hasAQI", suspendWhenHidden = FALSE)
  
  output$annual_rainfall_plot <- renderPlot({
    data = get_rain_gauge_data()
    
    if (nrow(data) > 0) {
      annual_data = data %>%
        mutate(
          water_year = get_water_year(date),
          daily_split_mm = daily_split * 25.4  # Convert inches to mm
        ) %>%
        group_by(water_year) %>%
        summarise(
          total_rainfall = sum(daily_split_mm, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        filter(!is.na(water_year))
      
      ggplot(annual_data, aes(x = factor(water_year), y = total_rainfall)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_minimal() +
        labs(title = "Annual Rainfall by Water Year", 
             x = "Water Year", 
             y = "Total Rainfall (mm)") +
        theme(text = element_text(size = 14))
    }
  })
  
  # Monthly rainfall by water year (interactive plotly) - FIXED
  output$monthly_rainfall_plot <- renderPlotly({
    data = get_rain_gauge_data()
    
    if (nrow(data) > 0) {
      # Convert inches to mm (1 inch = 25.4 mm)
      data$daily_split <- data$daily_split * 25.4
      data$rainfall <- data$rainfall * 25.4
      
      # Get current water year
      current_wy = get_water_year(Sys.Date())
      
      # Calculate monthly totals by water year
      monthly_data = data %>%
        mutate(
          water_year = get_water_year(date),
          wy_month = get_water_year_month(date)
        ) %>%
        group_by(water_year, wy_month) %>%
        summarise(
          monthly_total = sum(daily_split, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        # Ensure all months are represented for each water year
        complete(water_year, wy_month = 1:12, fill = list(monthly_total = 0)) %>%
        arrange(water_year, wy_month)
      
      # Calculate mean by month
      monthly_mean = monthly_data %>%
        group_by(wy_month) %>%
        summarise(
          mean_rainfall = mean(monthly_total, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(wy_month)
      
      # Create plotly figure
      p <- plot_ly()
      
      # Add individual water year lines (grey)
      unique_years = unique(monthly_data$water_year)
      for (wy in unique_years) {
        if (wy != current_wy && !is.na(wy)) {
          wy_data = monthly_data %>% 
            filter(water_year == wy) %>%
            arrange(wy_month)
          
          p <- p %>% add_trace(
            x = 1:12,
            y = wy_data$monthly_total,
            type = 'scatter',
            mode = 'lines',
            name = as.character(wy),
            line = list(color = 'lightgrey', width = 1),
            opacity = 0.5,
            hoverinfo = 'text',
            text = paste('Water Year', wy, '<br>',
                         water_year_months, ': ', round(wy_data$monthly_total, 1), ' mm'),
            showlegend = FALSE
          )
        }
      }
      
      # Add current water year (blue)
      if (current_wy %in% monthly_data$water_year) {
        current_data = monthly_data %>% 
          filter(water_year == current_wy) %>%
          arrange(wy_month)
        
        p <- p %>% add_trace(
          x = 1:12,
          y = current_data$monthly_total,
          type = 'scatter',
          mode = 'lines+markers',
          name = paste('Current WY', current_wy),
          line = list(color = 'blue', width = 3),
          marker = list(color = 'blue', size = 6),
          hoverinfo = 'text',
          text = paste('Water Year', current_wy, '<br>',
                       water_year_months, ': ', round(current_data$monthly_total, 1), ' mm')
        )
      }
      
      # Add mean line (black, thick)
      mean_ordered = monthly_mean %>% arrange(wy_month)
      p <- p %>% add_trace(
        x = 1:12,
        y = mean_ordered$mean_rainfall,
        type = 'scatter',
        mode = 'lines',
        name = 'Mean',
        line = list(color = 'black', width = 4),
        hoverinfo = 'text',
        text = paste('Mean<br>',
                     water_year_months, ': ', round(mean_ordered$mean_rainfall, 1), ' mm')
      )
      
      # Layout
      p <- p %>% layout(
        title = "Monthly Rainfall by Water Year",
        xaxis = list(
          title = "Month",
          tickmode = 'array',
          tickvals = 1:12,
          ticktext = water_year_months
        ),
        yaxis = list(title = "Rainfall (mm)"),
        hovermode = 'closest'  # Changed from 'x unified' to 'closest'
      )
      
      p
    } else {
      plotly_empty()
    }
  })
  
  # Rainfall statistics table
  output$rainfall_statistics <- DT::renderDataTable({
    data = get_rain_gauge_data()
    
    if (nrow(data) > 0) {
      data$daily_split <- data$daily_split * 25.4
      current_wy = get_water_year(Sys.Date())
      
      # Calculate statistics
      monthly_stats = data %>%
        mutate(
          water_year = get_water_year(date),
          wy_month = get_water_year_month(date)
        ) %>%
        group_by(water_year, wy_month) %>%
        summarise(
          monthly_total = sum(daily_split, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # Mean monthly rainfall
      monthly_mean = monthly_stats %>%
        group_by(wy_month) %>%
        summarise(
          mean_monthly = mean(monthly_total, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(wy_month)
      
      # Total mean annual
      total_mean_annual = sum(monthly_mean$mean_monthly)
      
      # Add percentages
      monthly_mean = monthly_mean %>%
        mutate(
          pct_of_annual = (mean_monthly / total_mean_annual) * 100,
          cumulative_pct = cumsum(pct_of_annual)
        )
      
      # Current year data
      current_year_data = monthly_stats %>%
        filter(water_year == current_wy) %>%
        arrange(wy_month) %>%
        select(wy_month, monthly_total)
      
      # Join all together
      result = monthly_mean %>%
        left_join(current_year_data, by = "wy_month") %>%
        mutate(
          # Replace NA with 0 for current year
          monthly_total = ifelse(is.na(monthly_total), 0, monthly_total),
          # Current year cumulative as % of mean annual
          current_cumulative_pct = (cumsum(monthly_total) / total_mean_annual) * 100,
          # Current month as % of mean month
          current_vs_mean_pct = ifelse(mean_monthly > 0, 
                                       (monthly_total / total_mean_annual) * 100, 
                                       NA)
        ) %>%
        mutate(
          Month = water_year_months[wy_month],
          `Mean Monthly (mm)` = round(mean_monthly, 1),
          `% of Annual` = round(pct_of_annual, 1),
          `Cumulative %` = round(cumulative_pct, 1),
          `Current WY (mm)` = round(monthly_total, 1),
          `Current Cumulative %` = round(current_cumulative_pct, 1),
          `Current vs Mean %` = round(current_vs_mean_pct, 1)
        ) %>%
        select(Month, `Mean Monthly (mm)`, `% of Annual`, `Cumulative %`, 
               `Current WY (mm)`, `Current Cumulative %`, `Current vs Mean %`)
      
      result
    }
  }, options = list(pageLength = 12, dom = 't'))
  
  # Current Conditions ####
  output$CurrentHeader = renderText({
    paste0('Current Conditions:')
  })
  
  output$CurrentConditions1 = renderTable({
    data = recent_data()
    if (nrow(data) == 0) return(data.frame())
    
    data.frame(
      `Temp (°C)` = round(tail(data$OutdoorTemperature, 1), 1),
      `Temp (°F)` = round(tail(data$OutdoorTemperature, 1) * 1.8 + 32, 0),
      `Humidity (%)` = round(tail(data$OutdoorHumidity, 1), 1),
      check.names = FALSE
    )
  }, align = 'l')
  
  output$CurrentConditions2 = renderTable({
    data = recent_data()
    if (nrow(data) == 0) return(data.frame())
    
    # Get last hour of data (approximately 12 readings)
    recent_rows = max(1, nrow(data) - 12):nrow(data)
    
    data.frame(
      `Wind Speed (kph)` = round(mean(data$WindSpeed[recent_rows], na.rm = TRUE), 1),
      check.names = FALSE
    )
  }, align = 'l')
  
  output$CurrentConditions3 = renderTable({
    data = recent_data()
    if (nrow(data) == 0) return(data.frame())
    
    recent_rows = max(1, nrow(data) - 12):nrow(data)
    
    data.frame(
      `Wind Gust (kph)` = round(max(data$WindGust[recent_rows], na.rm = TRUE), 1),
      check.names = FALSE
    )
  }, align = 'l')
  
  output$CurrentConditions4 = renderTable({
    data = recent_data()
    if (nrow(data) == 0) return(data.frame())
    
    recent_rows = max(1, nrow(data) - 12):nrow(data)
    
    data.frame(
      `Wind Direction (°)` = round(mean(data$WindDirection[recent_rows], na.rm = TRUE), 0),
      check.names = FALSE
    )
  }, align = 'l')
  
  output$CurrentConditions5 = renderTable({
    data = recent_data()
    if (nrow(data) == 0) return(data.frame())
    
    # Calculate 24-hour rolling rainfall (288 readings at 5-min intervals)
    rainfall_24h = if (nrow(data) >= 288) {
      sum(data$InstantRain[(nrow(data)-287):nrow(data)], na.rm = TRUE)
    } else {
      sum(data$InstantRain, na.rm = TRUE)
    }
    
    if (input$rain_correct) {
      rainfall_24h = rainfall_24h * rain_corr
    }
    
    # AQI (if available)
    recent_rows = max(1, nrow(data) - 12):nrow(data)
    aqi_value = if (any(!is.na(data$AQI[recent_rows]))) {
      round(mean(data$AQI[recent_rows], na.rm = TRUE), 0)
    } else {
      NA
    }
    
    result = data.frame(
      `24h Rainfall (mm)` = round(rainfall_24h, 1),
      check.names = FALSE
    )
    
    if (!is.na(aqi_value)) {
      result$`AQI` = aqi_value
    }
    
    result
  }, align = 'l')
  
  # Picture ####
  output$farmpic = renderImage({
    return(list(
      src = '/srv/shiny-server/WeatherDashboard/LatestImage.jpg', 
      contentType = 'image/jpg',
      width = '90%',
      height = '150%'
    ))
  }, deleteFile = FALSE)
  
  # Updated plots using processed data
  #temperature plot####
  output$TempPlot <- renderPlot({
    data = processed_data()
    if (nrow(data) == 0) {
      plot(1, type="n", main="No Temperature Data Available", xlab="", ylab="")
    } else {
      # cat("TempPlot - High/Low enabled:", input$high_low, "Resolution:", input$resolution, "\n")
      # cat("TempPlot - Has min_ot:", 'min_ot' %in% names(data), "\n")
      
      # Determine plot type based on resolution
      plot_type = if(input$resolution == '5 minutes') 20 else 'b'
      
      par(mar = c(5,4,2,4))
      plot(data$OutdoorTemperature ~ data$ts_PST, 
           main=paste("Temperature"), 
           xlab="Time", ylab="Temperature (°C)", las = 2,
           ylim = if(input$resolution == '5 minutes' | !input$high_low) range(data$OutdoorTemperature) else range(c(data$min_ot, data$max_ot), na.rm = T),
           pch=20, type=if(input$resolution == '5 minutes') 'p' else 'b')
      axis(side = 4, at = (axisTicks(par('usr')[3:4] *(9/5) + 32, log = F) - 32) *(5/9), labels = axisTicks(par('usr')[3:4] *(9/5) + 32, log = F), las = 2)
      mtext(side = 4, text = 'Temperature (°F)', line = 2)
      
      if(input$resolution != '5 minutes') {
        cat(range(c(data$min_ot, data$max_ot, data$min_it, data$max_it), na.rm = T))
      }
      # Add high/low polygons if enabled and not 5-minute data
      if (input$high_low && input$resolution != '5 minutes' && 'min_ot' %in% names(data)) {
        polygon(
          x = c(data$ts_PST, rev(data$ts_PST)),
          y = c(data$min_ot, rev(data$max_ot)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
      }
      
      # Add indoor temperature if available
      if (any(!is.na(data$IndoorTemperature))) {
        points(data$IndoorTemperature ~ data$ts_PST, col='blue', pch=20)
        if(input$resolution != '5 minutes') {
          lines(data$IndoorTemperature ~ data$ts_PST, col='blue')
        }
        
        # Add indoor high/low polygon if enabled
        if (input$high_low && input$resolution != '5 minutes' && 'min_it' %in% names(data)) {
          polygon(
            x = c(data$ts_PST, rev(data$ts_PST)),
            y = c(data$min_it, rev(data$max_it)),
            border = NA, col = adjustcolor('blue', alpha.f = 0.3)
          )
        }
        
        legend('topleft', legend=c('Outdoor', 'Indoor'), col=c('black', 'blue'), pch=20)
      }
      
      # Add smooth line if enabled
      if (input$smooth && nrow(data) > 2) {
        lines(
          smooth.spline(
            x = data$ts_PST,
            y = data$OutdoorTemperature,
            df = max(round(nrow(data) * input$smoothness, 0), 2)
          ),
          col = 'red', lwd = 2
        )
        
        if (any(!is.na(data$IndoorTemperature))) {
          lines(
            smooth.spline(
              x = data$ts_PST,
              y = data$IndoorTemperature,
              df = max(round(nrow(data) * input$smoothness, 0), 2)
            ),
            col = 'red', lwd = 2
          )
        }
      }
    }
  })
  #rain plot ####
  output$RainPlot <- renderPlot({
    data = processed_data()
    if (nrow(data) == 0) {
      plot(1, type="n", main="No Rain Data Available") 
    } else {
      rain_data = data$InstantRain
      if (input$rain_correct) {
        rain_data = rain_data * rain_corr
      }
      
      if(input$resolution == '5 minutes') {
        # Show cumulative for 5-minute data
        par(mar = c(5,4,2,4))
        plot(cumsum(rain_data) ~ data$ts_PST, 
             main=paste("Cumulative Rain"), las = 2,
             xlab="Time", ylab="Rain (mm)", pch=20, type='p')
      } else {
        # Show totals as barplot for aggregated data
        par(mar = c(5,4,2,4))
        barplot(rain_data, las = 2,
                main=paste("Total Rain (", input$resolution, ")"),
                ylab="Rain (mm)")
      }
      
      axis(side = 4, 
           at = axisTicks(par('usr')[3:4] * 0.0393700787, log = FALSE) / 0.0393700787,
           labels = axisTicks(par('usr')[3:4] * 0.0393700787, log = FALSE), 
           las = 2)
      mtext(side = 4, text = 'Rain (in)', line = 3)
    }
  })
  #Pressure plot ####
  output$PressurePlot <- renderPlot({
    data = processed_data()
    if (nrow(data) == 0) {
      plot(1, type="n", main="No Pressure Data Available")
    } else {
      par(mar = c(5,5,2,2))
      plot(data$BarometricPressure ~ data$ts_PST, 
           main=paste("Barometric Pressure"), 
           xlab="Time", ylab="Pressure (mbar)", las = 2, 
           ylim = if(input$resolution == '5 minutes' | !input$high_low) range(data$BarometricPressure) else range(c(data$min_bar, data$max_bar)),
           pch=20, type=if(input$resolution == '5 minutes') 'p' else 'b')
      
      # Add high/low polygons if enabled and not 5-minute data
      if (input$high_low && input$resolution != '5 minutes' && 'min_bar' %in% names(data)) {
        polygon(
          y = c(data$min_bar, rev(data$max_bar)),
          x = c(data$ts_PST, rev(data$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
      }
      
      # Add smooth line if enabled
      if (input$smooth && nrow(data) > 2) {
        lines(
          smooth.spline(
            x = data$ts_PST,
            y = data$BarometricPressure,
            df = max(round(nrow(data) * input$smoothness, 0), 2)
          ),
          col = 'red', lwd = 2
        )
      }
    }
  })
  #humidity plot ####
  output$HumPlot <- renderPlot({
    data = processed_data()
    if (nrow(data) == 0) {
      plot(1, type="n", main="No Humidity Data Available")
    } else {
      plot(data$OutdoorHumidity ~ data$ts_PST, 
           main=paste("Humidity"), las = 2,
           xlab="Time", ylab="Humidity (%)", 
           pch=20, type=if(input$resolution == '5 minutes') 'p' else 'b')
      
      # Add high/low polygons if enabled and not 5-minute data
      if (input$high_low && input$resolution != '5 minutes' && 'min_oh' %in% names(data)) {
        polygon(
          y = c(data$min_oh, rev(data$max_oh)),
          x = c(data$ts_PST, rev(data$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
      }
      
      # Add indoor humidity if available
      if (any(!is.na(data$IndoorHumidity))) {
        points(data$IndoorHumidity ~ data$ts_PST, col='blue', pch=20)
        if(input$resolution != '5 minutes') {
          lines(data$IndoorHumidity ~ data$ts_PST, col='blue')
        }
        
        # Add indoor high/low polygon if enabled
        if (input$high_low && input$resolution != '5 minutes' && 'min_ih' %in% names(data)) {
          polygon(
            y = c(data$min_ih, rev(data$max_ih)),
            x = c(data$ts_PST, rev(data$ts_PST)),
            border = NA, col = adjustcolor('blue', alpha.f = 0.3)
          )
        }
        
        legend('topleft', legend=c('Outdoor', 'Indoor'), col=c('black', 'blue'), pch=20)
      }
      
      # Add smooth line if enabled
      if (input$smooth && nrow(data) > 2) {
        lines(
          smooth.spline(
            x = data$ts_PST,
            y = data$OutdoorHumidity,
            df = max(round(nrow(data) * input$smoothness, 0), 2)
          ),
          col = 'red', lwd = 2
        )
        
        if (any(!is.na(data$IndoorHumidity))) {
          lines(
            smooth.spline(
              x = data$ts_PST,
              y = data$IndoorHumidity,
              df = max(round(nrow(data) * input$smoothness, 0), 2)
            ),
            col = 'red', lwd = 2
          )
        }
      }
    }
  })
  #wind speed plot ####
  output$WindPlot <- renderPlot({
    data = processed_data()
    if (nrow(data) == 0) {
      plot(1, type="n", main="No Wind Data Available")
    } else {
      # Plot wind speed
      par(mar = c(5,4,2,4))
      plot(data$WindSpeed ~ data$ts_PST, 
           main=paste("Wind Speed"),las = 2,
           xlab="Time", ylab="Wind Speed (kph)", 
           ylim = if(input$resolution == '5 minutes' | !input$high_low) range(c(data$WindSpeed, data$WindGust), na.rm = TRUE) else range(c(data$min_ws, data$max_wg)),
           pch=20, type=if(input$resolution == '5 minutes') 'p' else 'b'
      )
      
      # Add high/low polygons if enabled and not 5-minute data
      if (input$high_low && input$resolution != '5 minutes' && 'min_ws' %in% names(data)) {
        polygon(
          y = c(data$min_ws, rev(data$max_ws)),
          x = c(data$ts_PST, rev(data$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
        polygon(
          y = c(data$min_wg, rev(data$max_wg)),
          x = c(data$ts_PST, rev(data$ts_PST)),
          border = NA, col = adjustcolor('blue', alpha.f = 0.3)
        )
      }
      
      # Add wind gusts
      points(data$WindGust ~ data$ts_PST, col='blue', pch=20)
      if(input$resolution != '5 minutes') {
        lines(data$WindGust ~ data$ts_PST, col='blue')
      }
      
      # Add mph axis on right
      axis(4, at=axisTicks(range(c(data$WindSpeed, data$WindGust)*0.621371, na.rm=TRUE), log=FALSE)/.621371, las = 2,
           labels=round(axisTicks(range(c(data$WindSpeed, data$WindGust)*0.621371, na.rm=TRUE), log=FALSE)
                        , 1))
      mtext("Wind Speed (mph)", side=4, line=2)
      
      legend('topleft', legend=c('Speed', 'Gust'), col=c('black', 'blue'), pch=20)
      
      # Add smooth line if enabled
      if (input$smooth && nrow(data) > 2) {
        lines(
          smooth.spline(
            x = data$ts_PST,
            y = data$WindSpeed,
            df = max(round(nrow(data) * input$smoothness, 0), 2)
          ),
          col = 'red', lwd = 2
        )
      }
    }
  })
  #wind rose plot ####
  output$WindRose <- renderPlot({
    data = recent_data() # Use raw data for wind rose
    if (nrow(data) == 0 || all(is.na(data$WindSpeed)) || all(is.na(data$WindDirection))) {
      plot(1, type="n", main="No Wind Data Available")
    } else {
      # Prepare data for openair
      wind_data = data %>%
        select(ts_PST, WindSpeed, WindDirection) %>%
        rename(date = ts_PST, ws = WindSpeed, wd = WindDirection) %>%
        filter(!is.na(ws), !is.na(wd), ws > 0)
      
      if(nrow(wind_data) > 0) {
        polarFreq(wind_data, main = paste("Wind Rose"), wd.nint = 8)
        # plot(1, type="n", main="Wind Rose - Error in plotting")
        # text(1, 1, "Error creating wind rose", cex=1.2)
      } else {
        plot(1, type="n", main="Wind Rose - No valid wind data")
      }
    }
  })
  
  output$LightPlot <- renderPlot({
    data = processed_data()
    if (nrow(data) == 0 || all(is.na(data$SunlightVisible))) {
      plot(1, type="n", main="No Light Data Available")
    } else {
      plot(data$SunlightVisible ~ data$ts_PST, 
           main=paste("Light"), 
           xlab="Time", ylab="Light (Lux)", 
           pch=20, type=if(input$resolution == '5 minutes') 'p' else 'b')
      
      # Add high/low polygons if enabled and not 5-minute data
      if (input$high_low && input$resolution != '5 minutes' && 'min_light' %in% names(data)) {
        polygon(
          y = c(data$min_light, rev(data$max_light)),
          x = c(data$ts_PST, rev(data$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
      }
      
      # Add smooth line if enabled
      if (input$smooth && nrow(data) > 2) {
        lines(
          smooth.spline(
            x = data$ts_PST,
            y = data$SunlightVisible,
            df = max(round(nrow(data) * input$smoothness, 0), 2)
          ),
          col = 'red', lwd = 2
        )
      }
    }
  })
  
  #AQI plot ####
  output$AQIPlot <- renderPlot({
    data = processed_data()
    if (nrow(data) == 0 || all(is.na(data$AQI))) {
      plot(1, type="n", main="No AQI Data Available")
    } else {
      plot(data$AQI ~ data$ts_PST, 
           main=paste("AQI"), 
           xlab="Time", ylab="AQI", 
           pch=20, type=if(input$resolution == '5 minutes') 'p' else 'b')
      
      # Add high/low polygons if enabled and not 5-minute data
      if (input$high_low && input$resolution != '5 minutes' && 'min_aqi' %in% names(data)) {
        polygon(
          y = c(data$min_aqi, rev(data$max_aqi)),
          x = c(data$ts_PST, rev(data$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
      }
      
      # Add smooth line if enabled
      if (input$smooth && nrow(data) > 2) {
        lines(
          smooth.spline(
            x = data$ts_PST,
            y = data$AQI,
            df = max(round(nrow(data) * input$smoothness, 0), 2)
          ),
          col = 'red', lwd = 2
        )
      }
    }
  })
  
}

shinyApp(ui = ui, server = server)
