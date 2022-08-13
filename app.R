library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(openair)
library(RMySQL)
library(magick)

rain_corr = 1.255411

Sys.setenv(tz = 'America/Los_Angeles')
# print(getwd())
credentials = read.csv('credentials.csv')
db_user = credentials[credentials$field == 'db_user',2]
db_password = credentials[credentials$field == 'db_password',2]
db_name = credentials[credentials$field == 'db_name',2]
db_table = credentials[credentials$field == 'db_table',2]
db_host = credentials[credentials$field == 'db_host',2]


get_recent_data = function() {
  mydb = dbConnect(
    MySQL(),
    user = db_user, password = db_password, dbname = db_name, host = db_host
  )
  
  s = paste0("select * from ",db_table)
  rs = dbSendQuery(mydb, s)
  df = fetch(rs, n = -2)
  on.exit(dbDisconnect(mydb))
  
  data = df %>%
    filter(
      ID>6,
      SerialNumber %in% c(116,236)
    ) %>%
    mutate(
      ts_GMT = force_tz(as_datetime(TimeStamp), tzone = 'GMT'),
      ts_PST = with_tz(ts_GMT, tzone = 'America/Los_Angeles'),
      TotalRain = case_when(
        ID <= 676 ~ 0,
        ID >= 37254 & ID <=37269 & TotalRain != 355.8 ~ TotalRain + 355.8,
        ID > 37269 ~ TotalRain + 355.8,
        T ~ TotalRain
      ),
      InstantRain = c(0,diff(TotalRain)),
      SunlightVisible = case_when(
        ts_PST < '2022-02-25' ~ as.numeric(NA),
        SunlightVisible > 200000 ~ as.numeric(NA),
        T ~ SunlightVisible
      ),
      SunlightUVIndex = case_when(
        ts_PST < '2022-02-25' ~ as.numeric(NA),
        T ~ SunlightUVIndex 
      )
    ) %>%
    select(ID,TimeStamp,ts_PST, OutdoorTemperature,OutdoorHumidity,IndoorTemperature,IndoorHumidity,TotalRain,InstantRain,SunlightVisible,SunlightUVIndex,WindGust,
           WindDirection,WindSpeed,BarometricPressure,BarometricPressureSeaLevel,BarometricTemperature,PM_1_0,PM_2_5,PM_10,AQI,AQI24Average,SNR) %>%
    filter(ts_PST > '2022-02-23 17:00:00')
  
  dbClearResult(dbListResults(mydb)[[1]])
  return(data)
}

abs_hum = function(rel_hum,temp_c) {
  SVP = 6.11 * exp(((2500000)/461.52)*(1/273.15 - 1/(temp_c+273.15)))
  WVP2 = rel_hum* SVP
  ah = WVP2/(461.52*(temp_c+273.15))
  return(ah*1000)
}

df_local = get_recent_data()

# UI ####

ui = dashboardPage(
  dashboardHeader(title = 'Cartagena Farm Weather Station'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Current Conditions', tabName = 'conds', icon = icon('wind')),
      menuItem('Detailed Plots', tabName = 'plots', icon = icon('chart-line')),
      menuItem('Time Lapse', tabName = 'images')
    ),
    dateRangeInput(
      'range','Date Range',start = NULL, end = NULL
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
            plotOutput('AQIPlot')
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
            plotOutput('LightPlot'),
            plotOutput('WindRose')
          )
        )
      ),
      tabItem(
        tabName = 'images',
        fluidRow(
          column(
            6,
            actionButton('gif','Make New Timelapse'),
            imageOutput(outputId = 'timelapse'),
            div(),div(),div()
          )
        )
      )
    )
  )
)

server <- function(input, output,session) {
  
  recent_data <- reactivePoll(
    intervalMillis = 75 * 1e3,
    session = session,
    checkFunc = get_recent_data,
    valueFunc = function() {
      raw_df <- get_recent_data()
    }
  )
  
  # Temperature Plot ####
  
  output$TempPlot <- renderPlot({
    
    plot_df = recent_data()[,c('ts_PST','OutdoorTemperature','IndoorTemperature')] %>%
      filter(
        ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
        ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
      ) %>%
      data.frame()
    
    ylim_set = range(plot_df[,c('OutdoorTemperature','IndoorTemperature')]) * c(1,1.1)
    
    par(mar = c(6, 4.1, 4, 4))
    
    # * 5 minutes ####
    
    if(input$resolution == '5 minutes') {
      
      plot(
        plot_df$OutdoorTemperature~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = expression('Temp ('*degree*'C)')
      )
      par(new = T)
      plot(
        plot_df$IndoorTemperature~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        xaxt = 'n', yaxt = 'n', ylab = '', col = 'blue'
      )
      legend('topleft',bty = 'n', pch = 20, col = c('black','blue'), legend = c("Outdoor Temperature",'Indoor Temperature'))
      mtext(side = 3, line = 2, text = 'Temperature', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      
      axis(
        side = 4,
        las = 2,
        at = (axisTicks(ylim_set*1.8+32, log = F)-32)/1.8,
        labels = axisTicks(ylim_set*1.8+32, log = F)
      )
      mtext(side = 4, line = 2, text = expression('Temperature ('*degree*'F)')) 
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$OutdoorTemperature),1), round(last(plot_df$IndoorTemperature),1), sep = ','))
      mtext(side = 3, adj = 1, cex = 1.5,line = 0, text = paste(round(last(plot_df$OutdoorTemperature)*1.8+32,1),round(last(plot_df$IndoorTemperature)*1.8+32,1), sep = ','))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'OutdoorTemperature'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'IndoorTemperature'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      # * Hourly ####
      
    } else if (input$resolution == 'Hourly') {
      
      plot_df = recent_data()[,c('ts_PST','OutdoorTemperature','IndoorTemperature')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          hour = hour(ts_PST), date = date(ts_PST)
        ) %>%
        group_by(date, hour) %>%
        summarise(
          min_ot = min(OutdoorTemperature), max_ot = max(OutdoorTemperature), 
          min_it = min(IndoorTemperature), max_it = max(IndoorTemperature), 
          ts_PST = first(ts_PST), 
          OutdoorTemperature = mean(OutdoorTemperature), IndoorTemperature = mean(IndoorTemperature)
        ) %>%
        data.frame()
      
      plot(
        plot_df$OutdoorTemperature~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = expression('Temp ('*degree*'C)'), type = 'b'
      )
      par(new = T)
      plot(
        plot_df$IndoorTemperature~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        xaxt = 'n', yaxt = 'n', ylab = '', col = 'blue', type = 'b'
      )
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_ot, rev(plot_df$max_ot)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
        polygon(
          y = c(plot_df$min_it, rev(plot_df$max_it)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('blue', alpha.f = 0.3)
        )
      }
      legend('topleft',bty = 'n', pch = 20, col = c('black','blue'), legend = c("Outdoor Temperature",'Indoor Temperature'))
      mtext(side = 3, line = 2, text = 'Mean Hourly Temperature', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2, format = '%a %H')
      
      axis(
        side = 4,
        las = 2,
        at = (axisTicks(ylim_set*1.8+32, log = F, nint = 6)-32)/1.8,
        labels = axisTicks(ylim_set*1.8+32, log = F, nint  = 6)
      )
      mtext(side = 4, line = 2, text = expression('Temperature ('*degree*'F)')) 
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('OT: ',round(last(plot_df$OutdoorTemperature),1),', IT: ', round(last(plot_df$IndoorTemperature),1), sep = ''))
      mtext(side = 3, adj = 1, cex = 1.5,line = 0, text = paste('OT: ',round(last(plot_df$OutdoorTemperature)*1.8+32,1),', IT: ', round(last(plot_df$IndoorTemperature)*1.8+32,1), sep = ''))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'OutdoorTemperature'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'IndoorTemperature'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      
      # * Daily ####
      
    } else if (input$resolution == 'Daily') {
      
      plot_df = recent_data()[,c('ts_PST','OutdoorTemperature','IndoorTemperature')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          date = date(ts_PST)
        ) %>%
        group_by(date) %>%
        summarise(
          min_ot = min(OutdoorTemperature), max_ot = max(OutdoorTemperature), 
          min_it = min(IndoorTemperature), max_it = max(IndoorTemperature), 
          ts_PST = first(ts_PST), 
          OutdoorTemperature = mean(OutdoorTemperature), IndoorTemperature = mean(IndoorTemperature)
        ) %>%
        data.frame()
      
      plot(
        plot_df$OutdoorTemperature~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = expression('Temp ('*degree*'C)'), type = 'b'
      )
      par(new = T)
      plot(
        plot_df$IndoorTemperature~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        xaxt = 'n', yaxt = 'n', ylab = '', col = 'blue', type = 'b'
      )
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_ot, rev(plot_df$max_ot)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
        polygon(
          y = c(plot_df$min_it, rev(plot_df$max_it)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('blue', alpha.f = 0.3)
        )
      }
      legend('topleft',bty = 'n', pch = 20, col = c('black','blue'), legend = c("Outdoor Temperature",'Indoor Temperature'))
      mtext(side = 3, line = 2, text = 'Mean Daily Temperature', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      
      axis(
        side = 4,
        las = 2,
        at = (axisTicks(ylim_set*1.8+32, log = F, nint = 6)-32)/1.8,
        labels = axisTicks(ylim_set*1.8+32, log = F, nint  = 6)
      )
      mtext(side = 4, line = 2, text = expression('Temperature ('*degree*'F)')) 
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('OT: ',round(last(plot_df$OutdoorTemperature),1),', IT: ', round(last(plot_df$IndoorTemperature),1), sep = ''))
      mtext(side = 3, adj = 1, cex = 1.5,line = 0, text = paste('OT: ',round(last(plot_df$OutdoorTemperature)*1.8+32,1),', IT: ', round(last(plot_df$IndoorTemperature)*1.8+32,1), sep = ''))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'date'],
            y = plot_df[,'OutdoorTemperature'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
        lines(
          smooth.spline(
            x = plot_df[,'date'],
            y = plot_df[,'IndoorTemperature'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      
      # * Weekly ####
      
    } else if (input$resolution == 'Weekly') {
      
      plot_df = recent_data()[,c('ts_PST','OutdoorTemperature','IndoorTemperature')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          week = week(ts_PST), year = year(ts_PST)
        ) %>%
        group_by(year, week) %>%
        summarise(
          min_ot = min(OutdoorTemperature), max_ot = max(OutdoorTemperature), 
          min_it = min(IndoorTemperature), max_it = max(IndoorTemperature), 
          ts_PST = first(ts_PST), 
          OutdoorTemperature = mean(OutdoorTemperature), IndoorTemperature = mean(IndoorTemperature)
        ) %>%
        data.frame()
      
      plot(
        plot_df$OutdoorTemperature~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = expression('Temp ('*degree*'C)'), type = 'b'
      )
      par(new = T)
      plot(
        plot_df$IndoorTemperature~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        xaxt = 'n', yaxt = 'n', ylab = '', col = 'blue', type = 'b'
      )
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_ot, rev(plot_df$max_ot)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
        polygon(
          y = c(plot_df$min_it, rev(plot_df$max_it)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('blue', alpha.f = 0.3)
        )
      }
      legend('topleft',bty = 'n', pch = 20, col = c('black','blue'), legend = c("Outdoor Temperature",'Indoor Temperature'))
      mtext(side = 3, line = 2, text = 'Mean Weekly Temperature', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      
      axis(
        side = 4,
        las = 2,
        at = (axisTicks(ylim_set*1.8+32, log = F, nint = 6)-32)/1.8,
        labels = axisTicks(ylim_set*1.8+32, log = F, nint  = 6)
      )
      mtext(side = 4, line = 2, text = expression('Temperature ('*degree*'F)')) 
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('OT: ',round(last(plot_df$OutdoorTemperature),1),', IT: ', round(last(plot_df$IndoorTemperature),1), sep = ''))
      mtext(side = 3, adj = 1, cex = 1.5,line = 0, text = paste('OT: ',round(last(plot_df$OutdoorTemperature)*1.8+32,1),', IT: ', round(last(plot_df$IndoorTemperature)*1.8+32,1), sep = ''))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'OutdoorTemperature'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'IndoorTemperature'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      
      # * Monthly ####
      
    } else if (input$resolution == 'Monthly') {
      
      plot_df = recent_data()[,c('ts_PST','OutdoorTemperature','IndoorTemperature')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          month = month(ts_PST), year = year(ts_PST)
        ) %>%
        group_by(year, month) %>%
        summarise(
          min_ot = min(OutdoorTemperature), max_ot = max(OutdoorTemperature), 
          min_it = min(IndoorTemperature), max_it = max(IndoorTemperature), 
          ts_PST = first(ts_PST), 
          OutdoorTemperature = mean(OutdoorTemperature), IndoorTemperature = mean(IndoorTemperature)
        ) %>%
        data.frame()
      
      plot(
        plot_df$OutdoorTemperature~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = expression('Temp ('*degree*'C)'), type = 'b'
      )
      par(new = T)
      plot(
        plot_df$IndoorTemperature~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        xaxt = 'n', yaxt = 'n', ylab = '', col = 'blue', type = 'b'
      )
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_ot, rev(plot_df$max_ot)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
        polygon(
          y = c(plot_df$min_it, rev(plot_df$max_it)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('blue', alpha.f = 0.3)
        )
      }
      legend('topleft',bty = 'n', pch = 20, col = c('black','blue'), legend = c("Outdoor Temperature",'Indoor Temperature'))
      mtext(side = 3, line = 2, text = 'Mean Monthly Temperature', cex = 1.5)
      axis.POSIXct(x = plot_df$datetime,side = 1, las = 2)
      
      axis(
        side = 4,
        las = 2,
        at = (axisTicks(ylim_set*1.8+32, log = F, nint = 6)-32)/1.8,
        labels = axisTicks(ylim_set*1.8+32, log = F, nint  = 6)
      )
      mtext(side = 4, line = 2, text = expression('Temperature ('*degree*'F)')) 
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('OT: ',round(last(plot_df$OutdoorTemperature),1),', IT: ', round(last(plot_df$IndoorTemperature),1), sep = ''))
      mtext(side = 3, adj = 1, cex = 1.5,line = 0, text = paste('OT: ',round(last(plot_df$OutdoorTemperature)*1.8+32,1),', IT: ', round(last(plot_df$IndoorTemperature)*1.8+32,1), sep = ''))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'OutdoorTemperature'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'IndoorTemperature'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
    }
   
  })
  
  #
  
  # RainPlot ####
  output$RainPlot = renderPlot({
    
    par(mar = c(6, 4.1, 4, 4))
    
    # * 5 minutes ####
    
    if(input$resolution == '5 minutes') {
      plot_df = recent_data()[,c('ts_PST','TotalRain','InstantRain')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        data.frame()
      zero_value = max(c(0, last(filter(recent_data()[,c('ts_PST','TotalRain','InstantRain')], ts_PST < first(plot_df$ts_PST))$TotalRain)), na.rm = T)
      
      if(input$rain_correct == T) {
        plot_df$TotalRain = plot_df$TotalRain * rain_corr
        plot_df$InstantRain = plot_df$InstantRain * rain_corr
        zero_value = zero_value * rain_corr
      }
      
      plot(
        I(plot_df$TotalRain-zero_value)~plot_df$ts_PST,
        las = 2, pch = 20, cex = 1, xlab = '',
        ylab = 'mm', xaxt = 'n'
      )
      mtext(side = 3, line = 2, text = 'Cumulative Rainfall', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,format = '%a %H',side = 1, las = 2)
      
      axis(
        side = 4,
        las = 2,
        at = axisTicks(range((plot_df$TotalRain-zero_value))/25.4, log = F, nint = 6)*25.4,
        labels = axisTicks(range((plot_df$TotalRain-zero_value)/25.4), log = F, nint = 6)
      )
      mtext(side = 4, line = 2, text = 'Inches')
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$TotalRain)-zero_value,1)))
      mtext(side = 3, adj = 1, cex = 1.5,line = 0, text = paste(round((last(plot_df$TotalRain)-zero_value)/25.4,2)))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST']-zero_value,
            y = plot_df[,'TotalRain'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      # * Hourly ####
      
    } else if (input$resolution == 'Hourly') {
      plot_df = recent_data()[,c('ts_PST','InstantRain')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          hour = hour(ts_PST), date = date(ts_PST)
        ) %>%
        group_by(date, hour) %>%
        summarise(across(where(is.numeric), ~sum(.x)), ts_PST =first(ts_PST)) %>%
        mutate(
          datetime = format(ts_PST, format = '%b-%d %H')
        ) %>%
        data.frame()
      
      if(input$rain_correct == T) {
        # plot_df$TotalRain = plot_df$TotalRain * rain_corr
        plot_df$InstantRain = plot_df$InstantRain * rain_corr
        # zero_value = zero_value * rain_corr
      }
      
      b_rain = barplot(plot_df$InstantRain~plot_df$ts_PST, las = 2, ylab = 'mm', xlab = '', xaxt = 'n')
      mtext(side = 3, line = 2, text = 'Total Hourly Rainfall', cex = 1.5)
      axis(at = b_rain, labels = format(plot_df$ts_PST, format = '%a %H'),side = 1, las = 2)
      
      axis(
        side = 4,
        las = 2,
        at = axisTicks(range(c(0,(plot_df$InstantRain))/25.4), log = F, nint = 6)*25.4,
        labels = axisTicks(range(c(0,(plot_df$InstantRain))/25.4), log = F, nint  = 6)
      )
      mtext(side = 4, line = 2, text = 'Inches')
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$InstantRain),1)))
      mtext(side = 3, adj = 1, cex = 1.5,line = 0, text = paste(round(last(plot_df$InstantRain)/25.4,2)))
      # * Daily ####
    } else if (input$resolution == 'Daily') {
      plot_df = recent_data()[,c('ts_PST','InstantRain')] %>%
        filter(
          ts_PST > as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          date = date(ts_PST)
        ) %>%
        group_by(date) %>%
        summarise(across(where(is.numeric), ~sum(.x)), ts_PST = round(first(ts_PST), units = 'days')) %>%
        mutate(
          datetime = format(ts_PST, format = '%b-%d')
        ) %>%
        data.frame()
      
      if(input$rain_correct == T) {
        # plot_df$TotalRain = plot_df$TotalRain * rain_corr
        plot_df$InstantRain = plot_df$InstantRain * rain_corr
        # zero_value = zero_value * rain_corr
      }
      
      b_rain = barplot(plot_df$InstantRain, las = 2, ylab = 'mm', xlab = '')
      mtext(side = 3, line = 2, text = 'Total Daily Rainfall', cex = 1.5)
      axis(at = b_rain, labels = format(plot_df$ts_PST, format = '%b %a'),side = 1, las = 2)
      
      axis(
        side = 4,
        las = 2,
        at = axisTicks(range(c(0,(plot_df$InstantRain))/25.4), log = F, nint = 6)*25.4,
        labels = axisTicks(range(c(0,(plot_df$InstantRain))/25.4), log = F, nint  = 6)
      )
      mtext(side = 4, line = 2, text = 'Inches')
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$InstantRain),1)))
      mtext(side = 3, adj = 1, cex = 1.5,line = 0, text = paste(round(last(plot_df$InstantRain)/25.4,2)))
      # * Weekly ####
    } else if (input$resolution == 'Weekly') {
      plot_df = recent_data()[,c('ts_PST','InstantRain')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          year = year(ts_PST), week = week(ts_PST)
        ) %>%
        group_by(year,week) %>%
        summarise(across(where(is.numeric), ~sum(.x)), ts_PST = first(ts_PST)) %>%
        data.frame()
      
      if(input$rain_correct == T) {
        # plot_df$TotalRain = plot_df$TotalRain * rain_corr
        plot_df$InstantRain = plot_df$InstantRain * rain_corr
        # zero_value = zero_value * rain_corr
      }
      
      b_rain = barplot(plot_df$InstantRain, las = 2, xlab = '', ylab = '', xaxt = 'n')
      mtext(side = 3, line = 2, text = 'Total Weekly Rainfall', cex = 1.5)
      axis(at = b_rain, labels = format(plot_df$ts_PST, format = '%b %a'),side = 1, las = 2)
      
      axis(
        side = 4,
        las = 2,
        at = axisTicks(range(c(0,(plot_df$InstantRain))/25.4), log = F, nint = 6)*25.4,
        labels = axisTicks(range(c(0,(plot_df$InstantRain))/25.4), log = F, nint  = 6)
      )
      mtext(side = 4, line = 2, text = 'Inches')
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$InstantRain),1)))
      mtext(side = 3, adj = 1, cex = 1.5,line = 0, text = paste(round(last(plot_df$InstantRain)/25.4,2)))
      # * Monthly ####
    } else if (input$resolution == 'Monthly') {
      plot_df = recent_data()[,c('ts_PST','InstantRain')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          month = month(ts_PST), year = year(ts_PST)
        ) %>%
        group_by(year, month) %>%
        summarise(across(where(is.numeric), ~sum(.x)), ts_PST = first(ts_PST)) %>%
        mutate(
          date = format(ts_PST, format = '%Y %b')
        ) %>%
        data.frame()
      
      if(input$rain_correct == T) {
        # plot_df$TotalRain = plot_df$TotalRain * rain_corr
        plot_df$InstantRain = plot_df$InstantRain * rain_corr
        # zero_value = zero_value * rain_corr
      }
      
      b_rain = barplot(plot_df$InstantRain, las = 2, xlab = '', ylab = '', xaxt = 'n')
      mtext(side = 3, line = 2, text = 'Total Monthly Rainfall', cex = 1.5)
      axis(at = b_rain, labels = format(plot_df$ts_PST, format = '%Y %b'),side = 1, las = 2)
      
      axis(
        side = 4,
        las = 2,
        at = axisTicks(range(c(0,(plot_df$InstantRain))/25.4), log = F, nint = 6)*25.4,
        labels = axisTicks(range(c(0,(plot_df$InstantRain))/25.4), log = F, nint  = 6)
      )
      mtext(side = 4, line = 2, text = 'Inches')
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$InstantRain),1)))
      mtext(side = 3, adj = 1, cex = 1.5,line = 0, text = paste(round(last(plot_df$InstantRain)/25.4)))
    }
    
    
    
  })
  
  # Humidity Plot ####
  
  output$HumPlot <- renderPlot({
    
    plot_df = recent_data()[,c('ts_PST','OutdoorHumidity','IndoorHumidity')] %>%
      filter(
        ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
        ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
      ) %>%
      data.frame()
    
    ylim_set = range(plot_df[,c('OutdoorHumidity','IndoorHumidity')]) * c(1,1.1)
    
    par(mar = c(6, 4.1, 4, 4))
    
    # * 5 minutes ####
    
    if(input$resolution == '5 minutes') {
      
      plot(
        plot_df$OutdoorHumidity~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = 'Relative %'
      )
      par(new = T)
      plot(
        plot_df$IndoorHumidity~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        xaxt = 'n', yaxt = 'n', ylab = '', col = 'blue'
      )
      legend('topleft',bty = 'n', pch = 20, col = c('black','blue'), legend = c("Outdoor Humidity",'Indoor Humidity'))
      mtext(side = 3, line = 2, text = 'Humidity', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('OH: ',round(last(plot_df$OutdoorHumidity),1),', IH: ', round(last(plot_df$IndoorHumidity),1), sep = ''))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'OutdoorHumidity'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'IndoorHumidity'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      # * Hourly ####
      
    } else if (input$resolution == 'Hourly') {
      
      plot_df = recent_data()[,c('ts_PST','OutdoorHumidity','IndoorHumidity')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          hour = hour(ts_PST), date = date(ts_PST)
        ) %>%
        group_by(date, hour) %>%
        summarise(
          min_ih = min(IndoorHumidity), max_ih = max(IndoorHumidity), 
          min_oh = min(OutdoorHumidity), max_oh = max(OutdoorHumidity),
          IndoorHumidity = mean(IndoorHumidity), OutdoorHumidity = mean(OutdoorHumidity),
          ts_PST = first(ts_PST)
        ) %>%
        mutate(
          datetime = format(ts_PST, format = '%b-%d %H')
        ) %>%
        data.frame()
      
      plot(
        plot_df$OutdoorHumidity~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = 'Relative %', xaxt = 'n', type = 'b'
      )
      par(new = T)
      plot(
        plot_df$IndoorHumidity~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        xaxt = 'n', yaxt = 'n', ylab = '', col = 'blue', type = 'b'
      )
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_oh, rev(plot_df$max_oh)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
        polygon(
          y = c(plot_df$min_ih, rev(plot_df$max_ih)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('blue', alpha.f = 0.3)
        )
      }
      legend('topleft',bty = 'n', pch = 20, col = c('black','blue'), legend = c("Outdoor Humidity",'Indoor Humidity'))
      mtext(side = 3, line = 2, text = 'Hourly Mean Humidity', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2, format = '%a %H')
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('OH: ',round(last(plot_df$OutdoorHumidity),1),', IH: ', round(last(plot_df$IndoorHumidity),1), sep = ''))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'OutdoorHumidity'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'IndoorHumidity'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      
      # * Daily ####
      
    } else if (input$resolution == 'Daily') {
      
      plot_df = recent_data()[,c('ts_PST','OutdoorHumidity','IndoorHumidity')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          date = date(ts_PST)
        ) %>%
        group_by(date) %>%
        summarise(
          min_ih = min(IndoorHumidity), max_ih = max(IndoorHumidity), 
          min_oh = min(OutdoorHumidity), max_oh = max(OutdoorHumidity),
          IndoorHumidity = mean(IndoorHumidity), OutdoorHumidity = mean(OutdoorHumidity),
          ts_PST = first(ts_PST)
        ) %>%
        mutate(
          datetime = format(ts_PST, format = '%b-%d %H')
        ) %>%
        data.frame()
      
      plot(
        plot_df$OutdoorHumidity~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = 'Relative %', xaxt = 'n', type = 'b'
      )
      par(new = T)
      plot(
        plot_df$IndoorHumidity~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        xaxt = 'n', yaxt = 'n', ylab = '', col = 'blue', type = 'b'
      )
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_oh, rev(plot_df$max_oh)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
        polygon(
          y = c(plot_df$min_ih, rev(plot_df$max_ih)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('blue', alpha.f = 0.3)
        )
      }
      legend('topleft',bty = 'n', pch = 20, col = c('black','blue'), legend = c("Outdoor Humidity",'Indoor Humidity'))
      mtext(side = 3, line = 2, text = 'Weekly Mean Humidity', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2, format = '%b %a')
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('OH: ',round(last(plot_df$OutdoorHumidity),1),', IH: ', round(last(plot_df$IndoorHumidity),1), sep = ''))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'date'],
            y = plot_df[,'OutdoorHumidity'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
        lines(
          smooth.spline(
            x = plot_df[,'date'],
            y = plot_df[,'IndoorHumidity'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      
      # * Weekly ####
      
    } else if (input$resolution == 'Weekly') {
      
      plot_df = recent_data()[,c('ts_PST','OutdoorHumidity','IndoorHumidity')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          week = week(ts_PST), year = year(ts_PST)
        ) %>%
        group_by(year, week) %>%
        summarise(
          min_ih = min(IndoorHumidity), max_ih = max(IndoorHumidity), 
          min_oh = min(OutdoorHumidity), max_oh = max(OutdoorHumidity),
          IndoorHumidity = mean(IndoorHumidity), OutdoorHumidity = mean(OutdoorHumidity),
          ts_PST = first(ts_PST)
        ) %>%
        mutate(
          datetime = format(ts_PST, format = '%b-%d %H')
        ) %>%
        data.frame()
      
      plot(
        plot_df$OutdoorHumidity~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = 'Relative %', xaxt = 'n', type = 'b'
      )
      par(new = T)
      plot(
        plot_df$IndoorHumidity~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        xaxt = 'n', yaxt = 'n', ylab = '', col = 'blue', type = 'b'
      )
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_oh, rev(plot_df$max_oh)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
        polygon(
          y = c(plot_df$min_ih, rev(plot_df$max_ih)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('blue', alpha.f = 0.3)
        )
      }
      legend('topleft',bty = 'n', pch = 20, col = c('black','blue'), legend = c("Outdoor Humidity",'Indoor Humidity'))
      mtext(side = 3, line = 2, text = 'Humidity', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('OH: ',round(last(plot_df$OutdoorHumidity),1),', IH: ', round(last(plot_df$IndoorHumidity),1), sep = ''))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'OutdoorHumidity'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'IndoorHumidity'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      
      # * Monthly ####
      
    } else if (input$resolution == 'Monthly') {
      
      plot_df = recent_data()[,c('ts_PST','OutdoorHumidity','IndoorHumidity')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          month = month(ts_PST), year = year(ts_PST)
        ) %>%
        group_by(year, month) %>%
        summarise(
          min_ih = min(IndoorHumidity), max_ih = max(IndoorHumidity), 
          min_oh = min(OutdoorHumidity), max_oh = max(OutdoorHumidity),
          IndoorHumidity = mean(IndoorHumidity), OutdoorHumidity = mean(OutdoorHumidity),
          ts_PST = first(ts_PST)
        ) %>%
        mutate(
          datetime = format(ts_PST, format = '%b-%d %H')
        ) %>%
        data.frame()
      
      plot(
        plot_df$OutdoorHumidity~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = 'Relative %', xaxt = 'n', type = 'b'
      )
      par(new = T)
      plot(
        plot_df$IndoorHumidity~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        xaxt = 'n', yaxt = 'n', ylab = '', col = 'blue', type = 'b'
      )
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_oh, rev(plot_df$max_oh)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
        polygon(
          y = c(plot_df$min_ih, rev(plot_df$max_ih)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('blue', alpha.f = 0.3)
        )
      }
      legend('topleft',bty = 'n', pch = 20, col = c('black','blue'), legend = c("Outdoor Humidity",'Indoor Humidity'))
      mtext(side = 3, line = 2, text = 'Monthly Mean Humidity', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2, format = '%Y %b')
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('OH: ',round(last(plot_df$OutdoorHumidity),1),', IH: ', round(last(plot_df$IndoorHumidity),1), sep = ''))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'datetime'],
            y = plot_df[,'OutdoorHumidity'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
        lines(
          smooth.spline(
            x = plot_df[,'datetime'],
            y = plot_df[,'IndoorHumidity'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
    }
  })
  
  # Light Plot ####
  
  output$LightPlot <- renderPlot({
    
    plot_df = recent_data()[,c('ts_PST','SunlightVisible','SunlightUVIndex')] %>%
      filter(
        ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
        ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
      ) %>%
      filter(!is.na(SunlightVisible)) %>%
      data.frame()
    
    ylim_set = range(plot_df[,c('SunlightVisible','SunlightUVIndex')]) * c(1,1.1)
    
    par(mar = c(6, 4.1, 4, 4))
    
    # * 5 minutes ####
    
    if(input$resolution == '5 minutes') {
      
      plot(
        plot_df$SunlightVisible~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = 'Lux x 1000', yaxt = 'n'
      )
      axis(
        side = 2, las = 2,
        at = axisTicks(ylim_set/1000, log = F)*1000,
        labels = axisTicks(ylim_set/1000, log = F)
      )
      par(new = T)
      plot(
        I(plot_df$SunlightUVIndex)*5000~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        xaxt = 'n', yaxt = 'n', ylab = '', col = 'blue'
      )
      legend('topleft',bty = 'n', pch = 20, col = c('black','blue'), legend = c("Sunlight",'UV Index'))
      mtext(side = 3, line = 2, text = 'Light', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      
      axis(
        side = 4,
        las = 2,
        at = axisTicks(ylim_set/5000, log = F)*5000,
        labels = axisTicks(ylim_set/5000, log = F)
      )
      mtext(side = 4, line = 2, text = 'UV Index')
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('Lux: ',round(last(plot_df$SunlightVisible),1),', UVI: ', round(last(plot_df$SunlightUVIndex),1), sep = ''))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'SunlightVisible'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'SunlightUVIndex']*5000,
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      # * Hourly ####
      
    } else if (input$resolution == 'Hourly') {
      
      plot_df = recent_data()[,c('ts_PST','SunlightVisible','SunlightUVIndex')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          hour = hour(ts_PST), date = date(ts_PST)
        ) %>%
        filter(!is.na(SunlightVisible)) %>%
        group_by(date, hour) %>%
        summarise(across(where(is.numeric), ~mean(.x, na.rm = T)), ts_PST = first(ts_PST)) %>%
        data.frame()
      
      
      b_lux = barplot(plot_df$SunlightVisible, las = 2, xlab = '', ylab = 'Lux', xaxt = 'n')
      axis(at = b_lux, labels = format(plot_df$ts_PST, format = '%a %H'),side = 1, las = 2)
      
      mtext(side = 3, line = 2, text = 'Mean Hourly Sunlight', cex = 1.5)
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$SunlightVisible),1)))
      
      
      # * Daily ####
      
    } else if (input$resolution %in% c('Daily', 'Weekly','Monthly')) {
      
      plot_df = recent_data()[,c('ts_PST','SunlightVisible')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        filter(!is.na(SunlightVisible)) %>%
        filter(SunlightVisible != 0) %>%
        mutate(
          date = date(ts_PST),
          hours = hour(ts_PST)
        ) %>%
        group_by(date) %>%
        summarise(time = length(unique(hours)), dur = max(ts_PST) - min(ts_PST)) %>%
        mutate(
          datetime = format(date, format = '%b-%d')
        ) %>%
        data.frame()
      
      barplot(plot_df$dur~plot_df$date, las = 2, xlab = '', ylab = 'Hours', main = 'Daylight Hours', ylim = c(8, max(plot_df$dur)), xpd = F)
      
    }
  })
  
  
  # Pressure Plot ####
  output$PressurePlot <- renderPlot({
    
    plot_df = recent_data()[,c('ts_PST','BarometricPressure')] %>%
      filter(
        ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
        ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
      ) %>%
      data.frame()
    
    par(mar = c(6, 4.1, 4, 4))
    
    # * 5 minutes ####
    
    if(input$resolution == '5 minutes') {
      
      plot(
        plot_df$BarometricPressure~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '',
        ylab = 'cbar'
      )
      mtext(side = 3, line = 2, text = 'Barometric Pressure', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$BarometricPressure),1)))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'BarometricPressure'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      # * Hourly ####
      
    } else if (input$resolution == 'Hourly') {
      
      plot_df = recent_data()[,c('ts_PST','BarometricPressure')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          hour = hour(ts_PST), date = date(ts_PST)
        ) %>%
        group_by(date, hour) %>%
        summarise(
          min_bar = min(BarometricPressure), max_bar = max(BarometricPressure),
          ts_PST = first(ts_PST), 
          BarometricPressure = mean(BarometricPressure)
        ) %>%
        mutate(
          datetime = format(ts_PST, format = '%b-%d %H')
        ) %>%
        data.frame()
      
      plot(
        plot_df$BarometricPressure~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '',
        ylab = 'cbar', type ='b'
      )
      mtext(side = 3, line = 2, text = 'Barometric Pressure', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$BarometricPressure),1)))
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_bar, rev(plot_df$max_bar)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
      }
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'BarometricPressure'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      
      # * Daily ####
      
    } else if (input$resolution == 'Daily') {
      
      plot_df = recent_data()[,c('ts_PST','BarometricPressure')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          date = date(ts_PST)
        ) %>%
        group_by(date) %>%
        summarise(
          min_bar = min(BarometricPressure), max_bar = max(BarometricPressure),
          ts_PST = first(ts_PST), 
          BarometricPressure = mean(BarometricPressure)
        ) %>%
        mutate(
          datetime = format(date, format = '%b-%d')
        ) %>%
        data.frame()
      
      plot(
        plot_df$BarometricPressure~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '',
        ylab = 'cbar', type = 'b'
      )
      mtext(side = 3, line = 2, text = 'Barometric Pressure', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$BarometricPressure),1)))
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_bar, rev(plot_df$max_bar)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
      }
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'BarometricPressure'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      
      # * Weekly ####
      
    } else if (input$resolution == 'Weekly') {
      
      plot_df = recent_data()[,c('ts_PST','BarometricPressure')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          week = week(ts_PST), year = year(ts_PST)
        ) %>%
        group_by(year, week) %>%
        summarise(
          min_bar = min(BarometricPressure), max_bar = max(BarometricPressure),
          ts_PST = first(ts_PST), 
          BarometricPressure = mean(BarometricPressure)
        ) %>%
        mutate(
          year_week = paste(year, week)
        ) %>%
        data.frame()
      
      plot(
        plot_df$BarometricPressure~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '',
        ylab = 'cbar', type = 'b'
      )
      mtext(side = 3, line = 2, text = 'Barometric Pressure', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$BarometricPressure),1)))
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_bar, rev(plot_df$max_bar)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
      }
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'BarometricPressure'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      
      # * Monthly ####
      
    } else if (input$resolution == 'Monthly') {
      
      plot_df = recent_data()[,c('ts_PST','BarometricPressure')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          month = month(ts_PST), year = year(ts_PST)
        ) %>%
        group_by(year, month) %>%
        summarise(
          min_bar = min(BarometricPressure), max_bar = max(BarometricPressure),
          ts_PST = first(ts_PST), 
          BarometricPressure = mean(BarometricPressure)
        ) %>%
        data.frame()
      
      plot(
        plot_df$BarometricPressure~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '',
        ylab = 'cbar', type = 'b'
      )
      mtext(side = 3, line = 2, text = 'Barometric Pressure', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$BarometricPressure),1)))
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_bar, rev(plot_df$max_bar)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
      }
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'BarometricPressure'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
    }
  })
  
  # Wind Plot ####
  
  output$WindPlot = renderPlot ({
    
    par(mar = c(6, 4.1, 4, 4))
    
    # * 5 minutes ####
    if(input$resolution %in% c('5 minutes')) {
      
      plot_df = recent_data()[,c('ts_PST','WindSpeed', 'WindGust','WindDirection')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        data.frame()
      
      ylim_set = range(plot_df[,c('WindSpeed','WindGust')])
      
      plot(
        plot_df$WindGust~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '',
        ylab = expression('m'^'-s'), ylim = ylim_set,
        col = 'blue'
      )
      points(
        plot_df$WindSpeed~plot_df$ts_PST, pch = 20
      )
      mtext(side = 3, line = 2, text = 'Wind', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      axis(
        side = 4,
        las = 2,
        at = (axisTicks(range(ylim_set*2.2369), log = F)/2.2369),
        labels = axisTicks(ylim_set*2.2369, log = F)
      )
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$WindSpeed),1)))
      mtext(side = 3, adj = 1, cex = 1.5, line = 0, text = paste(round(last(plot_df$WindGust),1)))
      mtext(side = 4, line = 2, text = 'mph')
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'WindSpeed'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      # * Hourly ####
      
    } else if (input$resolution == 'Hourly') {
      
      plot_df = recent_data()[,c('ts_PST','WindSpeed', 'WindGust','WindDirection')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          date = date(ts_PST), hour = hour(ts_PST)
        ) %>%
        group_by(date, hour) %>%
        summarise(across(where(is.numeric), ~mean(.x, na.rm = T)), ts_PST = first(ts_PST)) %>%
        data.frame()
      
      ylim_set = range(plot_df[,c('WindSpeed','WindGust')])
      
      plot(
        plot_df$WindGust~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '',
        ylab = expression('m'^'-s'), ylim = ylim_set,
        col = 'blue'
      )
      points(
        plot_df$WindSpeed~plot_df$ts_PST, pch = 20
      )
      mtext(side = 3, line = 2, text = 'Wind', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      axis(
        side = 4,
        las = 2,
        at = (axisTicks(range(ylim_set*2.2369), log = F)/2.2369),
        labels = axisTicks(ylim_set*2.2369, log = F)
      )
      mtext(side = 4, line = 2, text = 'mph')
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$WindSpeed),1)))
      mtext(side = 3, adj = 1, cex = 1.5, line = 0, text = paste(round(last(plot_df$WindGust),1)))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'WindSpeed'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      # * Daily ####
      
    } else if (input$resolution == 'Daily') {
      
      plot_df = recent_data()[,c('ts_PST','WindSpeed', 'WindGust','WindDirection')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          date = date(ts_PST)
        ) %>%
        group_by(date) %>%
        summarise(across(where(is.numeric), ~mean(.x, na.rm = T)), ts_PST = first(ts_PST)) %>%
        data.frame()
      
      ylim_set = range(plot_df[,c('WindSpeed','WindGust')])
      
      plot(
        plot_df$WindGust~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '',
        ylab = expression('m'^'-s'), ylim = ylim_set,
        col = 'blue', type = 'b'
      )
      lines(
        plot_df$WindSpeed~plot_df$ts_PST, pch = 20, type = 'b'
      )
      mtext(side = 3, line = 2, text = 'Wind', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      
      axis(
        side = 4,
        las = 2,
        at = (axisTicks(range(ylim_set*2.2369), log = F)/2.2369),
        labels = axisTicks(ylim_set*2.2369, log = F)
      )
      mtext(side = 4, line = 2, text = 'mph')
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$WindSpeed),1)))
      mtext(side = 3, adj = 1, cex = 1.5, line = 0, text = paste(round(last(plot_df$WindGust),1)))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'WindSpeed'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      # * Weekly ####
      
    } else if (input$resolution == 'Weekly') {
      
      plot_df = recent_data()[,c('ts_PST','WindSpeed', 'WindGust','WindDirection')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          year = year(ts_PST), week = week(ts_PST)
        ) %>%
        group_by(year, week) %>%
        summarise(across(where(is.numeric), ~mean(.x, na.rm = T)), ts_PST = first(ts_PST)) %>%
        data.frame()
      
      ylim_set = range(plot_df[,c('WindSpeed','WindGust')])
      
      plot(
        plot_df$WindGust~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '',
        ylab = expression('m'^'-s'), ylim = ylim_set,
        col = 'blue', type = 'b'
      )
      lines(
        plot_df$WindSpeed~plot_df$ts_PST, pch = 20, type = 'b'
      )
      mtext(side = 3, line = 2, text = 'Wind', cex = 1.5)
      axis.POSIXct(at = plot_df$ts_PST,labels = paste(plot_df$year, "Wk:",plot_df$week),side = 1, las = 2)
      
      axis(
        side = 4,
        las = 2,
        at = (axisTicks(range(ylim_set*2.2369), log = F)/2.2369),
        labels = axisTicks(ylim_set*2.2369, log = F)
      )
      mtext(side = 4, line = 2, text = 'mph')
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$WindSpeed),1)))
      mtext(side = 3, adj = 1, cex = 1.5, line = 0, text = paste(round(last(plot_df$WindGust),1)))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'WindSpeed'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      # * Monthly ####
      
    } else if (input$resolution == 'Monthly') {
      
      plot_df = recent_data()[,c('ts_PST','WindSpeed', 'WindGust','WindDirection')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = '') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = '') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          year = year(ts_PST), month = month(ts_PST)
        ) %>%
        group_by(year, month) %>%
        summarise(across(where(is.numeric), ~mean(.x, na.rm = T)), ts_PST = first(ts_PST)) %>%
        data.frame()
      
      ylim_set = range(plot_df[,c('WindSpeed','WindGust')])
      
      plot(
        plot_df$WindGust~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '',
        ylab = expression('m'^'-s'), ylim = ylim_set,
        col = 'blue', type = 'b'
      )
      lines(
        plot_df$WindSpeed~plot_df$ts_PST, pch = 20, type = 'b'
      )
      mtext(side = 3, line = 2, text = 'Wind', cex = 1.5)
      axis.POSIXct(at = plot_df$ts_PST, format = '%Y %b',side = 1, las = 2)
      
      axis(
        side = 4,
        las = 2,
        at = (axisTicks(range(ylim_set*2.2369), log = F)/2.2369),
        labels = axisTicks(ylim_set*2.2369, log = F)
      )
      mtext(side = 4, line = 2, text = 'mph')
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$WindSpeed),1)))
      mtext(side = 3, adj = 1, cex = 1.5, line = 0, text = paste(round(last(plot_df$WindGust),1)))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'WindSpeed'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
    }
  })
  
  # WindRose ####
  output$WindRose = renderPlot({
    # * 5 minutes ####
    if(input$resolution == '5 minutes') {
      wind_dat = recent_data()[,c('ts_PST','WindSpeed','WindDirection')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = 'America/Los_Angeles') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = 'America/Los_Angeles') + 3600*as.numeric(input$hourend)) %>%
        rename(date = ts_PST, ws = WindSpeed, wd = WindDirection) %>%
        mutate(ws = case_when(ws == 0 ~ as.numeric(NA), T ~ ws))
      polarFreq(
        wind_dat,
        main = "Time Period Wind Summary",
        wd.nint = 8
      )
      # * Hourly ####
    } else if (input$resolution == 'Hourly') {
      
      wind_dat = recent_data()[,c('ts_PST','WindSpeed','WindDirection')] %>%
        filter(hour(ts_PST) == last(hour(ts_PST))) %>%
        rename(date = ts_PST, ws = WindSpeed, wd = WindDirection)
      polarFreq(
        wind_dat,
        main = "Last hour of wind within time period",
        wd.nint = 8
      )
      # * Daily ####
    } else if (input$resolution == 'Daily') {
      
      wind_dat = recent_data()[,c('ts_PST','WindSpeed','WindDirection')] %>%
        filter(date(ts_PST) == last(date(ts_PST))) %>%
        rename(date = ts_PST, ws = WindSpeed, wd = WindDirection)
      polarFreq(
        wind_dat,
        main = "Last day of wind within time period",
        wd.nint = 8
      )
      # * Weekly ####
    } else if (input$resolution == 'Weekly') {
      
      wind_dat = recent_data()[,c('ts_PST','WindSpeed','WindDirection')] %>%
        filter(week(ts_PST) == last(week(ts_PST))) %>%
        rename(date = ts_PST, ws = WindSpeed, wd = WindDirection)
      polarFreq(
        wind_dat,
        main = "Last week of wind within time period",
        wd.nint = 8
      )
      # * Monthly ####
    } else if (input$resolution == 'Monthly') {
      
      wind_dat = recent_data()[,c('ts_PST','WindSpeed','WindDirection')] %>%
        filter(month(ts_PST) == last(month(ts_PST))) %>%
        rename(date = ts_PST, ws = WindSpeed, wd = WindDirection)
      polarFreq(
        wind_dat,
        main = "Last month of wind within time period",
        wd.nint = 8
      )
      
    }
  })
  
  
  # Absolute Humidity Plot ####
  
  output$AQIPlot <- renderPlot({
    
    plot_df = recent_data()[,c('ts_PST','OutdoorTemperature','IndoorTemperature', 'OutdoorHumidity','IndoorHumidity')] %>%
      filter(
        ts_PST >as.POSIXct(as.character(input$range[1]), tz = 'America/Los_Angeles') + 3600*(as.numeric(input$hourstart)),
        ts_PST < as.POSIXct(as.character(input$range[2]), tz = 'America/Los_Angeles') + 3600*as.numeric(input$hourend)
      ) %>%
      mutate(
        OutdoorAbsHum = abs_hum(OutdoorHumidity,OutdoorTemperature),
        IndoorAbsHum = abs_hum(IndoorHumidity,IndoorTemperature)
      )
    data.frame()
    
    ylim_set = range(plot_df[,c('OutdoorAbsHum','IndoorAbsHum')]) * c(1,1.1)
    
    par(mar = c(6, 4.1, 4, 4))
    
    # * 5 minutes ####
    
    if(input$resolution == '5 minutes') {
      
      plot(
        plot_df$OutdoorAbsHum~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = 'Absolute Humidity (g/m^3)'
      )
      points(
        plot_df$IndoorAbsHum ~ plot_df$ts_PST,
        col = 'blue', pch = 19
      )
      mtext(side = 3, line = 2, text = 'Total Water Moisture', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste(round(last(plot_df$OutdoorAbsHum),1), round(last(plot_df$IndoorAbsHum),1), sep = ','))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'OutdoorAbsHum'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      # * Hourly ####
      
    } else if (input$resolution == 'Hourly') {
      
      plot_df = recent_data()[,c('ts_PST','OutdoorTemperature','IndoorTemperature', 'OutdoorHumidity','IndoorHumidity')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = 'America/Los_Angeles') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = 'America/Los_Angeles') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          hour = hour(ts_PST), date = date(ts_PST),
          OutdoorAbsHum = abs_hum(OutdoorHumidity,OutdoorTemperature),
          IndoorAbsHum = abs_hum(IndoorHumidity,IndoorTemperature),
        ) %>%
        group_by(date, hour) %>%
        summarise(
          min_oah = min(OutdoorAbsHum), max_oah = max(OutdoorAbsHum), 
          min_iah = min(IndoorAbsHum), max_iah = max(IndoorAbsHum), 
          OutdoorAbsHum = mean(OutdoorAbsHum),
          IndoorAbsHum = mean(IndoorAbsHum),
          ts_PST = first(ts_PST)
        ) %>%
        data.frame()
      
      ylim_set = range(plot_df[,c('OutdoorAbsHum','IndoorAbsHum')])
      
      plot(
        plot_df$OutdoorAbsHum~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = 'Absolute Humidity (g/m^3)', type = 'b'
      )
      lines(
        plot_df$IndoorAbsHum ~ plot_df$ts_PST,
        col = 'blue', pch = 19, type = 'b'
      )
      mtext(side = 3, line = 2, text = 'Mean Hourly Absolute Humidity', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2, format = '%a %H')
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_oah, rev(plot_df$max_oah)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
        polygon(
          y = c(plot_df$min_iah, rev(plot_df$max_iah)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('blue', alpha.f = 0.3)
        )
      }
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('OT: ',round(last(plot_df$OutdoorAbsHum),1),', IT: ', round(last(plot_df$IndoorAbsHum),1), sep = ''))
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'OutdoorAbsHum'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      
      # * Daily ####
      
    } else if (input$resolution == 'Daily') {
      
      plot_df = recent_data()[,c('ts_PST','OutdoorTemperature','IndoorTemperature', 'OutdoorHumidity','IndoorHumidity')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = 'America/Los_Angeles') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = 'America/Los_Angeles') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          date = date(ts_PST),
          OutdoorAbsHum = abs_hum(OutdoorHumidity,OutdoorTemperature),
          IndoorAbsHum = abs_hum(IndoorHumidity,IndoorTemperature),
        ) %>%
        group_by(date, hour) %>%
        summarise(
          min_oah = min(OutdoorAbsHum), max_oah = max(OutdoorAbsHum), 
          min_iah = min(IndoorAbsHum), max_iah = max(IndoorAbsHum), 
          OutdoorAbsHum = mean(OutdoorAbsHum),
          IndoorAbsHum = mean(IndoorAbsHum),
          ts_PST = first(ts_PST)
        ) %>%
        data.frame()
      
      ylim_set = range(plot_df[,c('OutdoorAbsHum','IndoorAbsHum')])
      
      plot(
        plot_df$OutdoorAbsHum~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = 'Absolute Humidity (g/m^3)', type = 'b'
      )
      lines(
        plot_df$IndoorAbsHum ~ plot_df$ts_PST,
        col = 'blue', pch = 19, type = 'b'
      )
      mtext(side = 3, line = 2, text = 'Mean Daily Absolute Humidity', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('OT: ',round(last(plot_df$OutdoorAbsHum),1),', IT: ', round(last(plot_df$IndoorAbsHum),1), sep = ''))
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_oah, rev(plot_df$max_oah)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
        polygon(
          y = c(plot_df$min_iah, rev(plot_df$max_iah)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('blue', alpha.f = 0.3)
        )
      }
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'OutdoorAbsHum'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      
      # * Weekly ####
      
    } else if (input$resolution == 'Weekly') {
      
      plot_df = recent_data()[,c('ts_PST','OutdoorTemperature','IndoorTemperature', 'OutdoorHumidity','IndoorHumidity')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = 'America/Los_Angeles') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = 'America/Los_Angeles') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          week = week(ts_PST), year = year(ts_PST),
          OutdoorAbsHum = abs_hum(OutdoorHumidity,OutdoorTemperature),
          IndoorAbsHum = abs_hum(IndoorHumidity,IndoorTemperature),
        ) %>%
        group_by(date, hour) %>%
        summarise(
          min_oah = min(OutdoorAbsHum), max_oah = max(OutdoorAbsHum), 
          min_iah = min(IndoorAbsHum), max_iah = max(IndoorAbsHum), 
          OutdoorAbsHum = mean(OutdoorAbsHum),
          IndoorAbsHum = mean(IndoorAbsHum),
          ts_PST = first(ts_PST)
        ) %>%
        data.frame()
      
      ylim_set = range(plot_df[,c('OutdoorAbsHum','IndoorAbsHum')])
      
      plot(
        plot_df$OutdoorAbsHum~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = 'Absolute Humidity (g/m^3)', type = 'b'
      )
      lines(
        plot_df$IndoorAbsHum ~ plot_df$ts_PST,
        col = 'blue', pch = 19, type = 'b'
      )
      mtext(side = 3, line = 2, text = 'Mean Weekly Absolute Humidity', cex = 1.5)
      axis.POSIXct(x = plot_df$ts_PST,side = 1, las = 2)
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('OT: ',round(last(plot_df$OutdoorAbsHum),1),', IT: ', round(last(plot_df$IndoorAbsHum),1), sep = ''))
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_oah, rev(plot_df$max_oah)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
        polygon(
          y = c(plot_df$min_iah, rev(plot_df$max_iah)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('blue', alpha.f = 0.3)
        )
      }
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'OutdoorAbsHum'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
      
      
      # * Monthly ####
      
    } else if (input$resolution == 'Monthly') {
      
      plot_df = recent_data()[,c('ts_PST','OutdoorTemperature','IndoorTemperature', 'OutdoorHumidity','IndoorHumidity')] %>%
        filter(
          ts_PST >as.POSIXct(as.character(input$range[1]), tz = 'America/Los_Angeles') + 3600*(as.numeric(input$hourstart)),
          ts_PST < as.POSIXct(as.character(input$range[2]), tz = 'America/Los_Angeles') + 3600*as.numeric(input$hourend)
        ) %>%
        mutate(
          month = month(ts_PST), year = year(ts_PST),
          OutdoorAbsHum = abs_hum(OutdoorHumidity,OutdoorTemperature),
          IndoorAbsHum = abs_hum(IndoorHumidity,IndoorTemperature),
        ) %>%
        group_by(date, hour) %>%
        summarise(
          min_oah = min(OutdoorAbsHum), max_oah = max(OutdoorAbsHum), 
          min_iah = min(IndoorAbsHum), max_iah = max(IndoorAbsHum), 
          OutdoorAbsHum = mean(OutdoorAbsHum),
          IndoorAbsHum = mean(IndoorAbsHum),
          ts_PST = first(ts_PST)
        ) %>%
        data.frame()
      
      ylim_set = range(plot_df[,c('OutdoorAbsHum','IndoorAbsHum')])
      plot(
        plot_df$OutdoorAbsHum~plot_df$ts_PST,
        las = 2, xaxt = 'n', pch = 20, cex = 1, xlab = '', ylim = ylim_set,
        ylab = 'Absolute Humidity (g/m^3)', type = 'b'
      )
      lines(
        plot_df$IndoorAbsHum ~ plot_df$ts_PST,
        col = 'blue', pch = 19, type = 'b'
      )
      mtext(side = 3, line = 2, text = 'Mean Monthly Absolute Humidity', cex = 1.5)
      axis.POSIXct(x = plot_df$datetime,side = 1, las = 2)
      
      mtext(side = 3, adj = 0, cex = 1.5,line = 0, text = paste('OT: ',round(last(plot_df$OutdoorAbsHum),1),', IT: ', round(last(plot_df$IndoorAbsHum),1), sep = ''))
      if (input$high_low == TRUE) {
        
        polygon(
          y = c(plot_df$min_oah, rev(plot_df$max_oah)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('grey', alpha.f = 0.3)
        )
        polygon(
          y = c(plot_df$min_iah, rev(plot_df$max_iah)),
          x = c(plot_df$ts_PST, rev(plot_df$ts_PST)),
          border = NA, col = adjustcolor('blue', alpha.f = 0.3)
        )
      }
      
      if(input$smooth == T) {
        lines(
          smooth.spline(
            x = plot_df[,'ts_PST'],
            y = plot_df[,'OutdoorAbsHum'],
            df = max(round(nrow(plot_df)*input$smoothness,0),2)
          ),
          col = 'red', lwd = 2
        )
      }
    }
  })
  
  # Current Conditions ####
  
  # output$CurrentConditions1 = renderTable({
  #   
  #   
  #   currentconditions = recent_data()[,colnames(df_local)] %>%
  #     select(TotalRain,OutdoorTemperature,IndoorTemperature,OutdoorHumidity,IndoorHumidity) %>%
  #     mutate(
  #       InstantRain = c(0,diff(TotalRain)),
  #       OutdoorTemperature = last(OutdoorTemperature),
  #       IndoorTemperature = last(IndoorTemperature),
  #       OutdoorHumidity = last(OutdoorHumidity),
  #       IndoorHumidity = last(IndoorHumidity),
  #       Rain = sum(InstantRain[length(InstantRain)-12:length(InstantRain)])
  #     ) %>%
  #     select(-InstantRain,-TotalRain) %>%
  #     unique()
  # })
  # 
  # output$CurrentConditions2 = renderTable({
  #   
  #   currentconditions = recent_data()[,colnames(df_local)] %>%
  #     select(WindSpeed,WindGust,WindDirection,AQI) %>%
  #     mutate(
  #       Wind = mean(WindSpeed[n()-12:n()], na.rm = T),
  #       WindGusts = max(WindGust[n()-12:n()]),
  #       WindDirection = mean(WindDirection[n()-12:n()], na.rm = T),
  #       AQI = mean(AQI[n()-12:n()], na.rm = T)
  #     ) %>%
  #     select(-WindSpeed,-WindGust) %>%
  #     unique()
  # })
  # 
  # output$CC1 = renderText({
  #   last_temp = recent_data()[,c('ID','OutdoorTemperature')] %>%
  #     filter(ID == last(ID)) 
  #   print(last_temp$OutdoorTemperature)
  #   paste('Current Temperature is ', last_temp$OutdoorTemperature,
  #         ' C/',last_temp$OutdoorTemperature*1.8+32,'F', sep = '')
  #   
  # })
  
  output$CurrentHeader = renderText({
    paste0('Current Conditions:')
  })
  
  output$CurrentConditions1 = renderTable({
    
    currentconditions = recent_data()[,colnames(df_local)] %>%
      select(TotalRain,OutdoorTemperature,IndoorTemperature,OutdoorHumidity,IndoorHumidity) %>%
      mutate(
        InstantRain = c(0,diff(TotalRain)),
        `Temp (C)` = round(last(OutdoorTemperature),1),
        `Temp (F)` = round(last(OutdoorTemperature)*1.8+32,0),
        Humidity = last(OutdoorHumidity)
      ) %>%
      select(`Temp (C)`, `Temp (F)`, Humidity) %>%
      unique()
  }, align = 'l')
  
  output$CurrentConditions2 = renderTable({
    
    currentconditions = recent_data()[,colnames(df_local)] %>%
      select(WindSpeed,WindGust,WindDirection,AQI) %>%
      mutate(
        `Mean Wind Speed of Past Hour` = mean(WindSpeed[(n()-12):n()], na.rm = T),
        `Max Wind Gusts of Past Hour` = max(WindGust[(n()-12):n()], na.rm = T)
      ) %>%
      select(`Mean Wind Speed of Past Hour`) %>%
      unique()
  }, align = 'l')
  
  output$CurrentConditions3 = renderTable({
    
    currentconditions = recent_data()[,colnames(df_local)] %>%
      select(WindSpeed,WindGust,WindDirection,AQI) %>%
      mutate(
        `Max Wind Gusts of Past Hour` = max(WindGust[(n()-12):n()])
      ) %>%
      select(`Max Wind Gusts of Past Hour`) %>%
      unique()
    
  }, align = 'l')
  
  output$CurrentConditions4 = renderTable({
    currentconditions = recent_data()[,colnames(df_local)] %>%
      select(WindDirection) %>%
      mutate(
        `Mean Wind Direction of Past Hour` = round(mean(WindDirection[(n()-12):n()], na.rm = T),0)
      ) %>%
      select(`Mean Wind Direction of Past Hour`) %>%
      unique()
  }, align = 'l')
  
  output$CurrentConditions5 = renderTable({
    
    currentconditions = recent_data()[,colnames(df_local)] %>%
      select(InstantRain, AQI) %>%
      mutate(
        `Last Hour of Rainfall` = sum(InstantRain[(length(InstantRain)-12):length(InstantRain)]),
        # `Last Hour of Rainfall` = case_when(
        #   input$rain_correct == T ~ sum(InstantRain[(length(InstantRain)-12):length(InstantRain)]*rain_corr),
        #   T ~ sum(InstantRain[(length(InstantRain)-12):length(InstantRain)])
        #),
        `Mean AQI of Past Hour` = round(mean(AQI[(n()-12):n()], na.rm = T),0)
      ) %>%
      select(`Last Hour of Rainfall`, `Mean AQI of Past Hour`) %>%
      unique() %>%
      mutate(
        `Last Hour of Rainfall` = case_when(
          input$rain_correct == T ~ `Last Hour of Rainfall`*rain_corr,
          T ~ `Last Hour of Rainfall`
        )
      )
  }, align = 'l')
  
  #picture ####
  output$farmpic = renderImage({
    return(list(src = '/srv/shiny-server/WeatherDashboard/LatestImage.jpg', contentType = 'image/jpg',width = '90%',height = '150%'))
  },deleteFile = F)
  
  gif_generate = eventReactive(input$gif, {
    # print(getwd())
    # print(dir('../'))
    # print(dir('../images'))
    pics = dir('../images', pattern = '*.jpg')
    pics_dir = dir('../images',pattern = '*.jpg', full.names = T)
    # print(pics)
    pic_dates = as.POSIXct(str_sub(pics, start = 12, end = -5L), format = '%Y-%m-%d-%H-%M-%S')
    # print(pic_dates)
    # tlp = which(pic_dates > as.POSIXct('2022-03-20 01:00:00 PDT'))
    tlp = which(pic_dates > as.POSIXct(as.character(input$range[1]), tz = 'America/Los_Angeles') + 3600*(as.numeric(input$hourstart)) & 
                  pic_dates < as.POSIXct(as.character(input$range[2]), tz = 'America/Los_Angeles') + 3600*as.numeric(input$hourend))
    # print(tlp)
    list.files = pics_dir[tlp] %>%
      image_read() %>%
      image_join() %>%
      image_animate(fps = 2) %>%
      image_write('../images/test.gif')
  })
  output$timelapse = renderImage({
    gif_generate()
    return(list(src = '../images/test.gif', width = '100%'))
  }, deleteFile = F)
  
}

shinyApp(ui = ui, server = server)