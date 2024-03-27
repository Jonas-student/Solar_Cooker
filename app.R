############################################################
# R shiny to analyse the performance of the solar cooker   #
# according to the protocol ASAE S580.1 JAN03              #
#                                                          #
# Author: Jonas Meijerink                                  #
# Date: April 2024                                         #
############################################################

#############
# Libraries #
#############

library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)
library(XML)
library(rvest)
library(httr)
library(lubridate)
library(ggplot2)
rm(list = ls())


############
# Shiny UI #
############

ui <- fluidPage(tags$head(
  tags$style(HTML("
            code {
                display:block;
                padding:9.5px;
                margin:0 0 10px;
                margin-top:10px;
                font-size:13px;
                line-height:20px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#F5F5F5;
                border:1px solid rgba(0,0,0,0.15);
                border-radius:4px; 
                font-family:monospace;
            }"))),
  
  navbarPage(
    "Solar Cooker",
    id = "main_navbar",

# First Tabpanel
#-------------------------------------------------------------------------------    
    tabPanel(
      "Upload CSV files",
      # Sidebar layout with input and output definitions ----
      sidebarPanel(
        strong("Test day 1"),
        tags$hr(),
        textInput(
          inputId = "namefile1",
          label = "Enter the name of your dataset bellow",
          placeholder = "Enter text to be used as plot title"
        ),
        
        fileInput("file1", "Choose your teststation data file (.CSV)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        fileInput("file1meta", "Choose the corresponding metadata file (.CSV)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        downloadButton("downloadData1", "Download"),
        tags$hr(),
        strong("Test day 2"),
        tags$hr(),
        textInput(
          inputId = "namefile2",
          label = "Enter the name of your dataset bellow",
          placeholder = "Enter text to be used as plot title"
        ),
        
        fileInput("file2", "Choose your teststation data file (.CSV)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        fileInput("file2meta", "Choose the corresponding metadata file (.CSV)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        downloadButton("downloadData2", "Download"),
        tags$hr(),
        strong("Test day 3"),
        tags$hr(),
        textInput(
          inputId = "namefile3",
          label = "Enter the name of your dataset bellow",
          placeholder = "Enter text to be used as plot title"
        ),
        
        fileInput("file3", "Choose your teststation data file (.CSV)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        fileInput("file3meta", "Choose the corresponding metadata file (.CSV)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        downloadButton("downloadData3", "Download")
      ),
      mainPanel(strong("This script contains an R function to produce a dataset as output containing average measurements of, e.g., the
            ambient temperature (in degrees Celsius) based on repeated measurements within a time period of 10 min.    
            As input, the function requires information regarding    
            the device used (metadata file) and the dataset obtained from the test station."),
                tags$hr(),
                    p("The teststation data file should include the following information in the order bellow:"),
                    column(12, 
                           tags$div(
                             id = "info_list",
                             tags$ul(
                               tags$li("Year: year in which the experiment was performed"),
                               tags$li("Month: month in which the experiment was performed"),
                               tags$li("Day: day at which the experiment was performed"),
                               tags$li("Hour: hour of the day at which the observation was collected"),
                               tags$li("Minute: minute at which the observation was collected"),
                               tags$li("Second: second at which the observation was collected"),
                               tags$li("Outdoor_temp: outdoor temperature (in degrees Celsius)"),
                               tags$li("Wind_speed: wind speed"),
                               tags$li("Air_pressure: air pressure"),
                               tags$li("Rel_hum: relative humidity"),
                               tags$li("Temp_pot1: temperature in pot 1 (in degrees Celsius)"),
                               tags$li("Temp_pot2: temperature in pot 2 (in degrees Celsius)"),
                               tags$li("Temp_pot3: temperature in pot 3 (in degrees Celsius)")
                             )
                           )
          
          
        ),
        tags$hr(),
        p("The metadata file should include the following information in the order bellow:"),
        column(12, 
               tags$div(
                 id = "info_list",
                 tags$ul(
                   tags$li("Date (calendar time): metaData$TestDate"),
                   tags$li("Volume of water: metaData$M"),
                   tags$li("Capacity of water: metaData$Cv"),
                   tags$li("Brand of the cooker: metaData$Cooker"),
                   tags$li("Type of device: metaData$Tdevice"),
                   tags$li("Number of the testing station: metaData$TestStation"),
                   tags$li("Number of the testing probe: metaData$TempSensor"),
                   tags$li("Type of cooking vessel: metaData$Pot"),
                   tags$li("Person performing the experiment: metaData$Operator"),
                   tags$li(" Latitude of the testing site: metaData$Latitude"),
                   tags$li("Longitude of the testing site: metaData$Longitude"),
                   tags$li("Presence of absence of plastic bag around the pot: metaData$PlasticBag"),
                   tags$li("Identification of data from the testing station: metaData.data$Datafile"),
                   tags$li("Picture of the device: metaData$CookerPic")
                 )
               )
        ),
        
      ),
    ),

# Second Tabpanel
#------------------------------------------------------------------------------- 
  tabPanel("Graphs of teststation data",
    tabsetPanel(tabPanel("Graphs day 1",
      mainPanel(
        plotOutput("Plot11"),
        plotOutput("Plot12"),
        plotOutput("Plot13")
    )
  ),
  tabPanel("Graphs day 2",
                       mainPanel(
                         plotOutput("Plot21"),
                         plotOutput("Plot22"),
                         plotOutput("Plot23")
                       )
  ),
  tabPanel("Graphs day 3",
                       mainPanel(
                         plotOutput("Plot31"),
                         plotOutput("Plot32"),
                         plotOutput("Plot33")
                       )
  )),
  ),

# Third Tabpanel
#-------------------------------------------------------------------------------
  tabPanel(
    "Single value measurement",
    mainPanel(
      p("blabla")
    )
  ),


inverse = T
    )
  )

#####################
# Shiny server side #
#####################

server <- function(input, output) {
    
    original1<-  reactive({
      dat_long<-read.csv(input$file1$datapath, sep = ";", header = T, dec = ".",encoding="latin1")
      names(dat_long) = c("Year", "Month", "Day", "Hour", "Minute", "Second",
                          "Outdoor_temp", "Wind_speed", "Air_pressure", "Rel_hum",
                          "Temp_pot1", "Temp_pot2", "Temp_pot3", "Solar_irr")
      return(dat_long)
    })
  
    data1 <- reactive({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$file1)
      req(input$file1meta)
      req(input$namefile1)
      
      dat_long <- read.csv(input$file1$datapath, sep = ";", header = T, dec = ".",encoding="latin1")
      message(paste("Test station data processed: "))
      message(paste("Number of records: ", dim(dat_long)[1]))
      
      # Check whether the number of columns is correct
      #------------------------------------------------
      if (dim(dat_long)[2] == 14){message("Correct number of columns")}
      if (dim(dat_long)[2] != 14){message("Incorrect number of columns")}
      
      # Rename the variables
      #----------------------
      names(dat_long) = c("Year", "Month", "Day", "Hour", "Minute", "Second",
                          "Outdoor_temp", "Wind_speed", "Air_pressure", "Rel_hum",
                          "Temp_pot1", "Temp_pot2", "Temp_pot3", "Solar_irr")
      
      # Create an experiment ID based on timestamp
      #--------------------------------------------
      dat_long$Date = as.Date(with(dat_long, paste(Year, Month, Day, sep="-")))
      dat_long$Experiment_ID = sub("*.csv","",sub(".*/", "", input$namefile1))
      dat_long$Record = with(dat_long, ave(Experiment_ID, Experiment_ID, FUN = seq_along))
      dat_long$Timestamp = ymd_hms(with(dat_long, 
                                        paste(paste(Year, Month, Day, sep = "-"), 
                                              paste(Hour, Minute, Second, sep = ":"), "UTC")))
      dat_long$Time_diff = dat_long$Timestamp - dat_long$Timestamp[1]
      dat_long$Time_period = findInterval(dat_long$Time_diff, vec = seq(0, as.numeric(max(dat_long$Time_diff)), 600))
      
      # Calculation of the mean of variables in different time periods
      #----------------------------------------------------------------
      dat_long$Temp_pot3[dat_long$Temp_pot3 == "x"] = NA
      col_ids = which(colSums(is.na(dat_long[7:14])) == 0) + 6
      means_per_period = apply(dat_long[,col_ids], 2, function(x){tapply(x, dat_long$Time_period, mean)})
      
      # Construct the output dataset
      #------------------------------
      output_dataset = data.frame(Experiment_ID = rep(dat_long$Experiment_ID[1],3), 
                                  Pot_ID = 1:3)
      
      nr_periods = length(unique(dat_long$Time_period))
      outdoor_temp_mat = matrix(nrow = 3, ncol = nr_periods)
      pot_ids = which(grepl("pot", names(dat_long)))
      
      prev_output_dataset = output_dataset
      for (period_id in 1:nr_periods){ 
        period_names = paste(colnames(means_per_period[, which(!grepl("pot", colnames(means_per_period)))]),"_P", period_id, sep = "")
        new_output_dataset = cbind(prev_output_dataset, matrix(rep(means_per_period[period_id, which(!grepl("pot", colnames(means_per_period)))], each = 3), 
                                                               nrow = 3, byrow = F))
        names(new_output_dataset)[(ncol(prev_output_dataset)+1):(ncol(prev_output_dataset) + length(col_ids[which(!grepl("pot", colnames(means_per_period)))]))] = period_names
        
        temp_name = paste("Temp_pot_P", period_id, sep = "")
        temp_vals = means_per_period[period_id, which(grepl("pot", colnames(means_per_period)))]
        all_vals = rep(NA, 3)
        all_vals[1:length(temp_vals)] = temp_vals 
        new_output_dataset = cbind(new_output_dataset, all_vals)
        names(new_output_dataset)[(ncol(prev_output_dataset) + length(col_ids[which(!grepl("pot", colnames(means_per_period)))])) + 1] = temp_name
        
        prev_output_dataset = new_output_dataset
      }
      
      output_dataset = prev_output_dataset
      
      # Add start and end temperature to output dataset
      #-------------------------------------------------
      output_dataset$Start_temp = as.numeric(c(dat_long$Temp_pot1[1], dat_long$Temp_pot2[1], dat_long$Temp_pot3[1]))
      output_dataset$End_temp = as.numeric(c(dat_long$Temp_pot1[nrow(dat_long)], dat_long$Temp_pot2[nrow(dat_long)], dat_long$Temp_pot3[nrow(dat_long)]))
      
      # Add begin and end temperature to each interval
      #----------------------------------------------
      temp_pairs <- data.frame(matrix(NA, ncol = 2, nrow = nr_periods*3))
      
      colnames(temp_pairs)<-c("Start_temp_pot","End_temp_pot")
      for (i in 0:2){
        for (period_id in 1:(nr_periods-1)){
          temp_pairs[period_id+i*nr_periods,]<-c(dat_long[which(dat_long$Time_period == period_id)[1],11+i],dat_long[which(dat_long$Time_period == period_id+1)[1],11+i])
        }
        temp_pairs[(i+1)*nr_periods,]<-c(dat_long[which(dat_long$Time_period == nr_periods)[1],11+i],dat_long[tail(which(dat_long$Time_period == nr_periods),1),11+i])
      }
      
      temp_pairs<-mapply(temp_pairs, FUN=as.numeric)
      
      # Read in the metadata file
      #---------------------------
      
      #############################################################################
      # The METADATA file should include the following information:               #
      # Date (calendar time): "metaData$TestDate"                                 #
      # Volume of water: "metaData$M"                                             #
      # Capacity of water: "metaData$Cv"                                          #
      # Brand of the cooker: "metaData$Cooker"                                    #
      # Type of device: "metaData$Tdevice"                                        #
      # Number of the testing station: "metaData$TestStation"                     #
      # Number of the testing probe: "metaData$TempSensor"                        #
      # Type of cooking vessel: "metaData$Pot"                                    #
      # Person performing the experiment: "metaData$Operator"                     #
      # Latitude of the testing site: "metaData$Latitude"                         #
      # Longitude of the testing site: "metaData$Longitude"                       #
      # Presence of absence of plastic bag around the pot: "metaData$PlasticBag"  #
      # Identification of data from the testing station: "metaData.data$Datafile" #  
      # Picture of the device: "metaData$CookerPic"                               # 
      #############################################################################
      
      metadata = read.csv(input$file1meta$datapath, header = T, sep = ";", dec = ",",encoding="latin1")
      metadata = metadata[order(metadata$Datafile, metadata$Pot_ID), ]
      
      # Link the metadata to the teststation data 
      #-------------------------------------------
      output_dataset$TestDate <- metadata$TestDate[metadata$Datafile == output_dataset$Experiment_ID[[1]]]
      output_dataset$M <- metadata$M[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Cv <- metadata$Cv[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Cooker <- metadata$Cooker[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Tdevice <- metadata$Tdevice[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$TestStation <- metadata$TestStation[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$TempSensor<- metadata$TempSensor[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Pot <- metadata$Pot[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Operator <- metadata$Operator[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Latitude <- metadata$Latitude[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Longitude <- metadata$Longitude[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$PlasticBag <- metadata$PlasticBag[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$CookerPic <- metadata$CookerPic[metadata$Datafile == output_dataset$Experiment_ID]
      
      
      output_dat = reshape(output_dataset, direction = "long",
                             idvar = "Pot_ID",
                             varying = list(grep("Temp_pot", names(output_dataset)),
                                            grep("Outdoor_temp", names(output_dataset)),
                                            grep("Wind", names(output_dataset)),
                                            grep("Air", names(output_dataset)),
                                            grep("Rel", names(output_dataset)),
                                            grep("Solar", names(output_dataset))),
                             v.names = c("Temp_pot", "Outdoor_temp", "Wind_speed", "Air_pressure",
                                         "Rel_hum", "Solar_irr"),
                             timevar = "Time_period")
        
      output_dataset = output_dat[order(output_dat$Pot_ID), ]
      
      
      # add the begin and end temp to this format
      
      return(cbind(output_dataset,temp_pairs))  
      
    })
    
    original2<-  reactive({
      dat_long<-read.csv(input$file2$datapath, sep = ";", header = T, dec = ".",encoding="latin1")
      names(dat_long) = c("Year", "Month", "Day", "Hour", "Minute", "Second",
                          "Outdoor_temp", "Wind_speed", "Air_pressure", "Rel_hum",
                          "Temp_pot1", "Temp_pot2", "Temp_pot3", "Solar_irr")
      return(dat_long)
    })
    
    data2 <- reactive({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$file2)
      req(input$file2meta)
      req(input$namefile2)
      
      dat_long <- read.csv(input$file1$datapath, sep = ";", header = T, dec = ".",encoding="latin1")
      message(paste("Test station data processed: "))
      message(paste("Number of records: ", dim(dat_long)[1]))
      
      # Check whether the number of columns is correct
      #------------------------------------------------
      if (dim(dat_long)[2] == 14){message("Correct number of columns")}
      if (dim(dat_long)[2] != 14){message("Incorrect number of columns")}
      
      # Rename the variables
      #----------------------
      names(dat_long) = c("Year", "Month", "Day", "Hour", "Minute", "Second",
                          "Outdoor_temp", "Wind_speed", "Air_pressure", "Rel_hum",
                          "Temp_pot1", "Temp_pot2", "Temp_pot3", "Solar_irr")
      
      # Create an experiment ID based on timestamp
      #--------------------------------------------
      dat_long$Date = as.Date(with(dat_long, paste(Year, Month, Day, sep="-")))
      dat_long$Experiment_ID = sub("*.csv","",sub(".*/", "", input$namefile1))
      dat_long$Record = with(dat_long, ave(Experiment_ID, Experiment_ID, FUN = seq_along))
      dat_long$Timestamp = ymd_hms(with(dat_long, 
                                        paste(paste(Year, Month, Day, sep = "-"), 
                                              paste(Hour, Minute, Second, sep = ":"), "UTC")))
      dat_long$Time_diff = dat_long$Timestamp - dat_long$Timestamp[1]
      dat_long$Time_period = findInterval(dat_long$Time_diff, vec = seq(0, as.numeric(max(dat_long$Time_diff)), 600))
      
      # Calculation of the mean of variables in different time periods
      #----------------------------------------------------------------
      dat_long$Temp_pot3[dat_long$Temp_pot3 == "x"] = NA
      col_ids = which(colSums(is.na(dat_long[7:14])) == 0) + 6
      means_per_period = apply(dat_long[,col_ids], 2, function(x){tapply(x, dat_long$Time_period, mean)})
      
      # Construct the output dataset
      #------------------------------
      output_dataset = data.frame(Experiment_ID = rep(dat_long$Experiment_ID[1],3), 
                                  Pot_ID = 1:3)
      
      nr_periods = length(unique(dat_long$Time_period))
      outdoor_temp_mat = matrix(nrow = 3, ncol = nr_periods)
      pot_ids = which(grepl("pot", names(dat_long)))
      
      prev_output_dataset = output_dataset
      for (period_id in 1:nr_periods){ 
        period_names = paste(colnames(means_per_period[, which(!grepl("pot", colnames(means_per_period)))]),"_P", period_id, sep = "")
        new_output_dataset = cbind(prev_output_dataset, matrix(rep(means_per_period[period_id, which(!grepl("pot", colnames(means_per_period)))], each = 3), 
                                                               nrow = 3, byrow = F))
        names(new_output_dataset)[(ncol(prev_output_dataset)+1):(ncol(prev_output_dataset) + length(col_ids[which(!grepl("pot", colnames(means_per_period)))]))] = period_names
        
        temp_name = paste("Temp_pot_P", period_id, sep = "")
        temp_vals = means_per_period[period_id, which(grepl("pot", colnames(means_per_period)))]
        all_vals = rep(NA, 3)
        all_vals[1:length(temp_vals)] = temp_vals 
        new_output_dataset = cbind(new_output_dataset, all_vals)
        names(new_output_dataset)[(ncol(prev_output_dataset) + length(col_ids[which(!grepl("pot", colnames(means_per_period)))])) + 1] = temp_name
        
        prev_output_dataset = new_output_dataset
      }
      
      output_dataset = prev_output_dataset
      
      # Add start and end temperature to output dataset
      #-------------------------------------------------
      output_dataset$Start_temp = as.numeric(c(dat_long$Temp_pot1[1], dat_long$Temp_pot2[1], dat_long$Temp_pot3[1]))
      output_dataset$End_temp = as.numeric(c(dat_long$Temp_pot1[nrow(dat_long)], dat_long$Temp_pot2[nrow(dat_long)], dat_long$Temp_pot3[nrow(dat_long)]))
      
      # Add begin and end temperature to each interval
      #----------------------------------------------
      temp_pairs <- data.frame(matrix(NA, ncol = 2, nrow = nr_periods*3))
      
      colnames(temp_pairs)<-c("Start_temp_pot","End_temp_pot")
      for (i in 0:2){
        for (period_id in 1:(nr_periods-1)){
          temp_pairs[period_id+i*nr_periods,]<-c(dat_long[which(dat_long$Time_period == period_id)[1],11+i],dat_long[which(dat_long$Time_period == period_id+1)[1],11+i])
        }
        temp_pairs[(i+1)*nr_periods,]<-c(dat_long[which(dat_long$Time_period == nr_periods)[1],11+i],dat_long[tail(which(dat_long$Time_period == nr_periods),1),11+i])
      }
      
      temp_pairs<-mapply(temp_pairs, FUN=as.numeric)
      
      # Read in the metadata file
      #---------------------------
      
      #############################################################################
      # The METADATA file should include the following information:               #
      # Date (calendar time): "metaData$TestDate"                                 #
      # Volume of water: "metaData$M"                                             #
      # Capacity of water: "metaData$Cv"                                          #
      # Brand of the cooker: "metaData$Cooker"                                    #
      # Type of device: "metaData$Tdevice"                                        #
      # Number of the testing station: "metaData$TestStation"                     #
      # Number of the testing probe: "metaData$TempSensor"                        #
      # Type of cooking vessel: "metaData$Pot"                                    #
      # Person performing the experiment: "metaData$Operator"                     #
      # Latitude of the testing site: "metaData$Latitude"                         #
      # Longitude of the testing site: "metaData$Longitude"                       #
      # Presence of absence of plastic bag around the pot: "metaData$PlasticBag"  #
      # Identification of data from the testing station: "metaData.data$Datafile" #  
      # Picture of the device: "metaData$CookerPic"                               # 
      #############################################################################
      
      metadata = read.csv(input$file1meta$datapath, header = T, sep = ";", dec = ",",encoding="latin1")
      metadata = metadata[order(metadata$Datafile, metadata$Pot_ID), ]
      
      # Link the metadata to the teststation data 
      #-------------------------------------------
      output_dataset$TestDate <- metadata$TestDate[metadata$Datafile == output_dataset$Experiment_ID[[1]]]
      output_dataset$M <- metadata$M[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Cv <- metadata$Cv[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Cooker <- metadata$Cooker[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Tdevice <- metadata$Tdevice[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$TestStation <- metadata$TestStation[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$TempSensor<- metadata$TempSensor[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Pot <- metadata$Pot[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Operator <- metadata$Operator[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Latitude <- metadata$Latitude[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Longitude <- metadata$Longitude[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$PlasticBag <- metadata$PlasticBag[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$CookerPic <- metadata$CookerPic[metadata$Datafile == output_dataset$Experiment_ID]
      
      
      output_dat = reshape(output_dataset, direction = "long",
                           idvar = "Pot_ID",
                           varying = list(grep("Temp_pot", names(output_dataset)),
                                          grep("Outdoor_temp", names(output_dataset)),
                                          grep("Wind", names(output_dataset)),
                                          grep("Air", names(output_dataset)),
                                          grep("Rel", names(output_dataset)),
                                          grep("Solar", names(output_dataset))),
                           v.names = c("Temp_pot", "Outdoor_temp", "Wind_speed", "Air_pressure",
                                       "Rel_hum", "Solar_irr"),
                           timevar = "Time_period")
      
      output_dataset = output_dat[order(output_dat$Pot_ID), ]
      
      
      # add the begin and end temp to this format
      
      return(cbind(output_dataset,temp_pairs))  
      
    })
    
    original3<-  reactive({
      dat_long<-read.csv(input$file3$datapath, sep = ";", header = T, dec = ".",encoding="latin1")
      names(dat_long) = c("Year", "Month", "Day", "Hour", "Minute", "Second",
                          "Outdoor_temp", "Wind_speed", "Air_pressure", "Rel_hum",
                          "Temp_pot1", "Temp_pot2", "Temp_pot3", "Solar_irr")
      return(dat_long)
    })
    
    data3 <- reactive({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$file3)
      req(input$file3meta)
      req(input$namefile3)
      
      dat_long <- read.csv(input$file1$datapath, sep = ";", header = T, dec = ".",encoding="latin1")
      message(paste("Test station data processed: "))
      message(paste("Number of records: ", dim(dat_long)[1]))
      
      # Check whether the number of columns is correct
      #------------------------------------------------
      if (dim(dat_long)[2] == 14){message("Correct number of columns")}
      if (dim(dat_long)[2] != 14){message("Incorrect number of columns")}
      
      # Rename the variables
      #----------------------
      names(dat_long) = c("Year", "Month", "Day", "Hour", "Minute", "Second",
                          "Outdoor_temp", "Wind_speed", "Air_pressure", "Rel_hum",
                          "Temp_pot1", "Temp_pot2", "Temp_pot3", "Solar_irr")
      
      # Create an experiment ID based on timestamp
      #--------------------------------------------
      dat_long$Date = as.Date(with(dat_long, paste(Year, Month, Day, sep="-")))
      dat_long$Experiment_ID = sub("*.csv","",sub(".*/", "", input$namefile1))
      dat_long$Record = with(dat_long, ave(Experiment_ID, Experiment_ID, FUN = seq_along))
      dat_long$Timestamp = ymd_hms(with(dat_long, 
                                        paste(paste(Year, Month, Day, sep = "-"), 
                                              paste(Hour, Minute, Second, sep = ":"), "UTC")))
      dat_long$Time_diff = dat_long$Timestamp - dat_long$Timestamp[1]
      dat_long$Time_period = findInterval(dat_long$Time_diff, vec = seq(0, as.numeric(max(dat_long$Time_diff)), 600))
      
      # Calculation of the mean of variables in different time periods
      #----------------------------------------------------------------
      dat_long$Temp_pot3[dat_long$Temp_pot3 == "x"] = NA
      col_ids = which(colSums(is.na(dat_long[7:14])) == 0) + 6
      means_per_period = apply(dat_long[,col_ids], 2, function(x){tapply(x, dat_long$Time_period, mean)})
      
      # Construct the output dataset
      #------------------------------
      output_dataset = data.frame(Experiment_ID = rep(dat_long$Experiment_ID[1],3), 
                                  Pot_ID = 1:3)
      
      nr_periods = length(unique(dat_long$Time_period))
      outdoor_temp_mat = matrix(nrow = 3, ncol = nr_periods)
      pot_ids = which(grepl("pot", names(dat_long)))
      
      prev_output_dataset = output_dataset
      for (period_id in 1:nr_periods){ 
        period_names = paste(colnames(means_per_period[, which(!grepl("pot", colnames(means_per_period)))]),"_P", period_id, sep = "")
        new_output_dataset = cbind(prev_output_dataset, matrix(rep(means_per_period[period_id, which(!grepl("pot", colnames(means_per_period)))], each = 3), 
                                                               nrow = 3, byrow = F))
        names(new_output_dataset)[(ncol(prev_output_dataset)+1):(ncol(prev_output_dataset) + length(col_ids[which(!grepl("pot", colnames(means_per_period)))]))] = period_names
        
        temp_name = paste("Temp_pot_P", period_id, sep = "")
        temp_vals = means_per_period[period_id, which(grepl("pot", colnames(means_per_period)))]
        all_vals = rep(NA, 3)
        all_vals[1:length(temp_vals)] = temp_vals 
        new_output_dataset = cbind(new_output_dataset, all_vals)
        names(new_output_dataset)[(ncol(prev_output_dataset) + length(col_ids[which(!grepl("pot", colnames(means_per_period)))])) + 1] = temp_name
        
        prev_output_dataset = new_output_dataset
      }
      
      output_dataset = prev_output_dataset
      
      # Add start and end temperature to output dataset
      #-------------------------------------------------
      output_dataset$Start_temp = as.numeric(c(dat_long$Temp_pot1[1], dat_long$Temp_pot2[1], dat_long$Temp_pot3[1]))
      output_dataset$End_temp = as.numeric(c(dat_long$Temp_pot1[nrow(dat_long)], dat_long$Temp_pot2[nrow(dat_long)], dat_long$Temp_pot3[nrow(dat_long)]))
      
      # Add begin and end temperature to each interval
      #----------------------------------------------
      temp_pairs <- data.frame(matrix(NA, ncol = 2, nrow = nr_periods*3))
      
      colnames(temp_pairs)<-c("Start_temp_pot","End_temp_pot")
      for (i in 0:2){
        for (period_id in 1:(nr_periods-1)){
          temp_pairs[period_id+i*nr_periods,]<-c(dat_long[which(dat_long$Time_period == period_id)[1],11+i],dat_long[which(dat_long$Time_period == period_id+1)[1],11+i])
        }
        temp_pairs[(i+1)*nr_periods,]<-c(dat_long[which(dat_long$Time_period == nr_periods)[1],11+i],dat_long[tail(which(dat_long$Time_period == nr_periods),1),11+i])
      }
      
      temp_pairs<-mapply(temp_pairs, FUN=as.numeric)
      
      # Read in the metadata file
      #---------------------------
      
      #############################################################################
      # The METADATA file should include the following information:               #
      # Date (calendar time): "metaData$TestDate"                                 #
      # Volume of water: "metaData$M"                                             #
      # Capacity of water: "metaData$Cv"                                          #
      # Brand of the cooker: "metaData$Cooker"                                    #
      # Type of device: "metaData$Tdevice"                                        #
      # Number of the testing station: "metaData$TestStation"                     #
      # Number of the testing probe: "metaData$TempSensor"                        #
      # Type of cooking vessel: "metaData$Pot"                                    #
      # Person performing the experiment: "metaData$Operator"                     #
      # Latitude of the testing site: "metaData$Latitude"                         #
      # Longitude of the testing site: "metaData$Longitude"                       #
      # Presence of absence of plastic bag around the pot: "metaData$PlasticBag"  #
      # Identification of data from the testing station: "metaData.data$Datafile" #  
      # Picture of the device: "metaData$CookerPic"                               # 
      #############################################################################
      
      metadata = read.csv(input$file1meta$datapath, header = T, sep = ";", dec = ",",encoding="latin1")
      metadata = metadata[order(metadata$Datafile, metadata$Pot_ID), ]
      
      # Link the metadata to the teststation data 
      #-------------------------------------------
      output_dataset$TestDate <- metadata$TestDate[metadata$Datafile == output_dataset$Experiment_ID[[1]]]
      output_dataset$M <- metadata$M[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Cv <- metadata$Cv[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Cooker <- metadata$Cooker[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Tdevice <- metadata$Tdevice[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$TestStation <- metadata$TestStation[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$TempSensor<- metadata$TempSensor[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Pot <- metadata$Pot[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Operator <- metadata$Operator[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Latitude <- metadata$Latitude[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$Longitude <- metadata$Longitude[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$PlasticBag <- metadata$PlasticBag[metadata$Datafile == output_dataset$Experiment_ID]
      output_dataset$CookerPic <- metadata$CookerPic[metadata$Datafile == output_dataset$Experiment_ID]
      
      
      output_dat = reshape(output_dataset, direction = "long",
                           idvar = "Pot_ID",
                           varying = list(grep("Temp_pot", names(output_dataset)),
                                          grep("Outdoor_temp", names(output_dataset)),
                                          grep("Wind", names(output_dataset)),
                                          grep("Air", names(output_dataset)),
                                          grep("Rel", names(output_dataset)),
                                          grep("Solar", names(output_dataset))),
                           v.names = c("Temp_pot", "Outdoor_temp", "Wind_speed", "Air_pressure",
                                       "Rel_hum", "Solar_irr"),
                           timevar = "Time_period")
      
      output_dataset = output_dat[order(output_dat$Pot_ID), ]
      
      
      # add the begin and end temp to this format
      
      return(cbind(output_dataset,temp_pairs))  
      
    })
    
# Downloadable csv of selected dataset
#-------------------------------------------------------------------------------    
output$downloadData1 <- downloadHandler(
      filename = function() {
        paste(input$namefile1, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data1(), file, row.names = FALSE)
      }
    )
    
output$downloadData2 <- downloadHandler(
      filename = function() {
        paste(input$namefile2, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data2(), file, row.names = FALSE)
      }
    )

output$downloadData3 <- downloadHandler(
      filename = function() {
        paste(input$namefile3, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data3(), file, row.names = FALSE)
      }
    )

# Make some graphs for data exploration.
#-------------------------------------------------------------------------------
output$Plot11 <- renderPlot({
  
  req(input$file1)
  
  data <- original1() %>%
    mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format="%Y %m %d %H %M %S"))
  
  # Plot temperature vs. time
  ggplot(data, aes(x = datetime, y = Outdoor_temp)) +
    geom_line(aes(y = Temp_pot1, color = "Temp_pot1"), show.legend = TRUE) + # Add Temp_pot1 in red
    geom_line(aes(color = "Outdoor_temp"), show.legend = TRUE) +
    labs(x = "Time", y = "Temperature (°C)", title = "Temperature vs. Time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S")+  # Adjust the breaks and labels as needed
    scale_color_manual(values = c("black","red"), 
                     labels = c( "Outdoor temperature","Temperature in pot 1"))  # Define colors and labels
})

output$Plot12 <- renderPlot({
  
  req(input$file1)
  
  data <- original1() %>%
    mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format="%Y %m %d %H %M %S"))
  
  ggplot(data, aes(x = datetime)) +
    geom_point(aes(y = Solar_irr, color = "Solar Irradiance"), show.legend = TRUE) +
    geom_point(aes(y = Wind_speed * 100, color = "Wind Speed"), show.legend = TRUE) +
    scale_y_continuous(sec.axis = sec_axis(~./100, name = "Wind Speed (m/s)")) +
    labs(x = "Time", y = "Solar irradiance (W/mA^2)", title = "Solar Irradiance and Wind Speed vs. Time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") +  # Adjust the breaks and labels as needed
    scale_color_manual(values = c("#FFFF66", "#99CCFF"), 
                       labels = c("Solar Irradiance", "Wind Speed"))  # Define colors and labels
  
})

output$Plot13 <- renderPlot({
  
  req(input$file1)
  
  data <- original1() %>%
    mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format="%Y %m %d %H %M %S"))
  
  ggplot(data, aes(x = datetime)) +
    geom_point(aes(y = Air_pressure, color = "Air Pressure"), show.legend = TRUE) +
    geom_point(aes(y = Rel_hum*20, color = "Relative Humidity"), show.legend = TRUE) +
    scale_y_continuous(sec.axis = sec_axis(~./20, name = "Relative Humidity (%)")) +
    labs(x = "Time", y = "Air pressure (hPa)", title = "Air Pressure and Relative Humidity vs. Time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") +  # Adjust the breaks and labels as needed
    scale_color_manual(values = c("#CCFFFF", "#009900"), 
                       labels = c("Air Pressure", "Relative Humidity"))  # Define colors and labels
  
})

output$Plot21 <- renderPlot({
  
  req(input$file2)
  
  data <- original2() %>%
    mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format="%Y %m %d %H %M %S"))
  
  # Plot temperature vs. time
  ggplot(data, aes(x = datetime, y = Outdoor_temp)) +
    geom_line(aes(y = Temp_pot1, color = "Temp_pot1"), show.legend = TRUE) + # Add Temp_pot1 in red
    geom_line(aes(color = "Outdoor_temp"), show.legend = TRUE) +
    labs(x = "Time", y = "Temperature (°C)", title = "Temperature vs. Time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S")+  # Adjust the breaks and labels as needed
    scale_color_manual(values = c("black","red"), 
                       labels = c( "Outdoor temperature","Temperature in pot 1"))  # Define colors and labels
})

output$Plot22 <- renderPlot({
  
  req(input$file2)
  
  data <- original2() %>%
    mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format="%Y %m %d %H %M %S"))
  
  ggplot(data, aes(x = datetime)) +
    geom_point(aes(y = Solar_irr, color = "Solar Irradiance"), show.legend = TRUE) +
    geom_point(aes(y = Wind_speed * 100, color = "Wind Speed"), show.legend = TRUE) +
    scale_y_continuous(sec.axis = sec_axis(~./100, name = "Wind Speed (m/s)")) +
    labs(x = "Time", y = "Solar irradiance (W/mA^2)", title = "Solar Irradiance and Wind Speed vs. Time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") +  # Adjust the breaks and labels as needed
    scale_color_manual(values = c("#FFFF66", "#99CCFF"), 
                       labels = c("Solar Irradiance", "Wind Speed"))  # Define colors and labels
  
})

output$Plot23 <- renderPlot({
  
  req(input$file2)
  
  data <- original2() %>%
    mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format="%Y %m %d %H %M %S"))
  
  ggplot(data, aes(x = datetime)) +
    geom_point(aes(y = Air_pressure, color = "Air Pressure"), show.legend = TRUE) +
    geom_point(aes(y = Rel_hum*20, color = "Relative Humidity"), show.legend = TRUE) +
    scale_y_continuous(sec.axis = sec_axis(~./20, name = "Relative Humidity (%)")) +
    labs(x = "Time", y = "Air pressure (hPa)", title = "Air Pressure and Relative Humidity vs. Time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") +  # Adjust the breaks and labels as needed
    scale_color_manual(values = c("#CCFFFF", "#009900"), 
                       labels = c("Air Pressure", "Relative Humidity"))  # Define colors and labels
  
})

output$Plot31 <- renderPlot({
  
  req(input$file3)
  
  data <- original3() %>%
    mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format="%Y %m %d %H %M %S"))
  
  # Plot temperature vs. time
  ggplot(data, aes(x = datetime, y = Outdoor_temp)) +
    geom_line(aes(y = Temp_pot1, color = "Temp_pot1"), show.legend = TRUE) + # Add Temp_pot1 in red
    geom_line(aes(color = "Outdoor_temp"), show.legend = TRUE) +
    labs(x = "Time", y = "Temperature (°C)", title = "Temperature vs. Time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S")+  # Adjust the breaks and labels as needed
    scale_color_manual(values = c("black","red"), 
                       labels = c( "Outdoor temperature","Temperature in pot 1"))  # Define colors and labels
})

output$Plot32 <- renderPlot({
  
  req(input$file3)
  
  data <- original3() %>%
    mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format="%Y %m %d %H %M %S"))
  
  ggplot(data, aes(x = datetime)) +
    geom_point(aes(y = Solar_irr, color = "Solar Irradiance"), show.legend = TRUE) +
    geom_point(aes(y = Wind_speed * 100, color = "Wind Speed"), show.legend = TRUE) +
    scale_y_continuous(sec.axis = sec_axis(~./100, name = "Wind Speed (m/s)")) +
    labs(x = "Time", y = "Solar irradiance (W/mA^2)", title = "Solar Irradiance and Wind Speed vs. Time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") +  # Adjust the breaks and labels as needed
    scale_color_manual(values = c("#FFFF66", "#99CCFF"), 
                       labels = c("Solar Irradiance", "Wind Speed"))  # Define colors and labels
  
})

output$Plot33 <- renderPlot({
  
  req(input$file3)
  
  data <- original3() %>%
    mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format="%Y %m %d %H %M %S"))
  
  ggplot(data, aes(x = datetime)) +
    geom_point(aes(y = Air_pressure, color = "Air Pressure"), show.legend = TRUE) +
    geom_point(aes(y = Rel_hum*20, color = "Relative Humidity"), show.legend = TRUE) +
    scale_y_continuous(sec.axis = sec_axis(~./20, name = "Relative Humidity (%)")) +
    labs(x = "Time", y = "Air pressure (hPa)", title = "Air Pressure and Relative Humidity vs. Time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M:%S") +  # Adjust the breaks and labels as needed
    scale_color_manual(values = c("#CCFFFF", "#009900"), 
                       labels = c("Air Pressure", "Relative Humidity"))  # Define colors and labels
  
})
}

#######################
# Run the application #
#######################

shinyApp(ui = ui, server = server)