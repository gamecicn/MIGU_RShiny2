library(shiny)
library(ggplot2)
library(RMySQL)
library(datasets)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  
  
  ###############################################################################
  #   Data Fetching Functions
  ###############################################################################
  
  delay_min_records_threshold <- eventReactive(input$go_update, {input$min_records_threshold})
  
  delay_data_range <- eventReactive(input$go_update, {input$dateRange})
  
  # "appid, ms, emmc_md5" is basic  columns_name
  get_data_from_DB <- function(columns_name) {
    
      data_range <- delay_data_range()
    
      mysql_con <- dbConnect(MySQL(), user="root", password="irdetogame", dbname='MGLOGBACKUP', host="10.86.51.31")
      
      sql_query <- sprintf("select appid, ms, emmc_md5 %s  from LOGDETAIL where  stime between '%s' and '%s' and reqtype='BILLINGREQ';", columns_name, data_range[1], data_range[2] )
      
      print(sql_query)
      
      df  <- fetch(dbSendQuery(mysql_con, sql_query), n=30000)
      on.exit(dbDisconnect(mysql_con)) 
        
      # eliminate some data
      df <- subset(df, emmc_md5 != "AAAAAAAAAAAAAAAAAAAAAAA=" & ms != "")
      
      min_records_threshold <- delay_min_records_threshold()
      
      # eliminate too little record appid
      if (as.numeric(min_records_threshold) > 0) {
          app_group <- split(df, df$appid)
        
          h <- as.data.frame(t(t(sapply(app_group, FUN = function(g) nrow(g)))))
          h$appid <- rownames(h)
          h <- subset(h, h$V1 >= as.numeric(min_records_threshold))
          
          df <- subset(df, df$appid %in% h$appid)
      }
      
      df
  }
  
  #------------------------------------------------------------------------------------------------------ 

  get_imei_analaysis_data <- reactive({
    
      rm(list=ls())
    
      print("get_imei_analaysis_data")
    
      withProgress(message = 'Fetching data ... ... ', value = 0,{
          df <- get_data_from_DB("")
      })
  })
  
  get_start_interval_analysis_data <- reactive({
      rm(list=ls())
      
      print("get_imei_analaysis_data")
      
      withProgress(message = 'Fetching data ... ... ', value = 0,{
        df <- get_data_from_DB(",imei, apicall_start_interval")
      })
      
      
  })
  
  ############################################################################################################
  observeEvent(input$go, {
    print("observeEvent(input$go")
  })
  
  get_selected_appid <- eventReactive(input$go, {input$select_id})
  
  get_select_app_record <- reactive({
    print("get_select_app_record")
    
    if (input$tabpanel == "ab_imei") {
      df <- get_imei_analaysis_data()
    }
    else if (input$tabpanel == "ab_start_interval"){
      df <- get_start_interval_analysis_data()
    }
    
    subset(df, appid == get_selected_appid() )
    
  })
  
  output$app_imei_detail <- renderDataTable({
    
    get_selected_detail_data()
  })
  
  get_selected_detail_data <- reactive({
    print("get_selected_detail_data")
    
    if (input$tabpanel == "ab_imei") {
      df <- get_app_imei_detail()
    }
    else if (input$tabpanel == "ab_start_interval"){
      df <- get_app_si_detail()
    }
    
    df
    
  })
  
  
  
  #==========================================================================================
  #  Tab 1 --- IMEI Abnormal
  #==========================================================================================
  
  calculate_diff_number <- function(df_g) {
    
    test_group <- split(df_g, as.factor(df_g$emmc_md5), drop=TRUE)

    q <- t(sapply(test_group, FUN=function(g) length(unique(g$ms))))
    q <- as.data.frame(t(q))
    
    q$emmc <- rownames(q)
    
    colnames(q) <- c("duplicat_times", "emmc")
    mis_emms <- subset(q, q$duplicat_times > 1)
    
    total_emms <- subset(df_g, df_g$emmc_md5 %in% mis_emms$emmc)
    
    return ( c(sum(q$duplicat_times != 1), nrow(total_emms),  nrow(df_g)) )
  }

  output$imei_abnormal <- renderDataTable({
      df <- get_imei_analaysis_data()
  
      withProgress(message = 'Calculating IMEI ... ... ', value = 0, {
      
        df_group<-split(df, df$appid, drop = TRUE)
        print("sapply .... ")
        
        x <- t(sapply(df_group, FUN = function(gp) calculate_diff_number(gp) ))
        print("transposing .... ")
        
        print("Finish ")
        colnames(x) <- c("Abnoram EMMC Count", "Mismatch Record Count", "All Record Count")
        return(as.data.frame(x))  
    })
  }, selection = 'single', options = list(pageLength = 100), class = 'cell-border strip hover')
  
  
  get_app_imei_detail <- function() {
    print("app_imei_detail")
    
    sd <- get_select_app_record()
    sd_group <- split(sd, sd$emmc_md5)
    
    q <- t(sapply(sd_group, FUN=function(g) length(unique(g$ms) )  ))
    q <- as.data.frame(t(q))
    
    q$emmc <- rownames(q)
    rownames(q) <- NULL
    
    colnames(q) <- c("count", 'emmc')
    
    # Get mismatch record
    q <- subset(q, q$count > 1)
    
    res <- subset(sd, sd$emmc_md5 %in% q$emmc)
    res <- res[, c("emmc_md5", "ms")]
    
    rownames(res) <- NULL
    
    res$mark = paste(res$emmc_md5, res$ms, sep='-')
    res_group <- split(res, res$mark)
    
    q <- t(sapply(res_group, FUN=function(g)  c( g$emmc_md5[1] ,g$ms[1], length(g$mark)    )) )
    q <- as.data.frame(q)
    
    #if(nrow(q) > 0) {
    rownames(q) <- NULL
    colnames(q) <- c("emmc", "ms", "count")
    #}
    
    DT::datatable(q, selection = 'single', class = 'cell-border strip hover')
  }
  

  
  #==========================================================================================
  #  Tab 2 --- Start Time Interval Abnormal
  #==========================================================================================
  get_top10_interval_rate <- function(app_records) {
    
    gt <- split(app_records, as.factor(app_records$apicall_start_interval), drop=TRUE)
    sic <- as.data.frame(t(t(sapply(gt, FUN=function(g) nrow(g)))))
    sic$apicall_start_interval <- rownames(sic)
    colnames(sic) <- c("count", "apicall_start_interval")
    sic = sic[order(sic$count, decreasing=TRUE),]  # sore decreasing
    
    # Get top 10 count
    
    top10_percentage = sum(head(sic, n=10)$count)/(nrow(app_records) * 1.0)
    
    c(top10_percentage, nrow(sic))
  }
  
  
  output$start_time_interval_abnormal <- renderDataTable({
      df <- get_start_interval_analysis_data()
      
 
      withProgress(message = 'Calculating STI ... ... ', value = 0, {
        
        # Calculate more columns
        df$less_10 <- as.numeric(df$apicall_start_interval) < 10
        df$less_30 <- as.numeric(df$apicall_start_interval) < 30
        df$less_60 <- as.numeric(df$apicall_start_interval) < 60
        df$less_120 <- as.numeric(df$apicall_start_interval) < 120
        
        # Split 
        print("split .... ")
        app_group <- split(df, as.factor(df$appid), drop=TRUE)
        
        
        print("sapply .... ")
        df_app_start_interval <- t(sapply(app_group, FUN=function(g) c(sum(g$less_10),
                                                                       sum(g$less_30), 
                                                                       sum(g$less_60), 
                                                                       sum(g$less_120),
                                                                       nrow(g),
                                                                       sum(g$less_10)/nrow(g),
                                                                       sum(g$less_30)/nrow(g),
                                                                       sum(g$less_60)/nrow(g),
                                                                       sum(g$less_120)/nrow(g),
                                                                       get_top10_interval_rate(g))))
        
        print("sapply 1 .... ")
        df_app_start_interval <- as.data.frame(df_app_start_interval)
        print("sapply 2 .... ")
        colnames(df_app_start_interval) <- c("l_10", "l_30", "l_60", "l_120", "records", 
                                             "l_10_r", "l_30_r", "l_60_r", "l_120_r", 
                                             "top10_rate", "seg")
        
        df_app_start_interval
        
        # Sort                         
        #df_app_start_interval = df_app_start_interval[order(df_app_start_interval$top10_interval_rate, decreasing=TRUE), ]
        
      })
  }, selection = 'single', options = list(pageLength = 100), class = 'cell-border strip hover')
  
  get_app_si_detail <- function(){
    
    print("app_si_detail")
    
    sd <- get_select_app_record()
    
    sd_group <- split(sd, as.factor(sd$apicall_start_interval), drop=TRUE)
    
    sic <- as.data.frame(t(t(sapply(sd_group, FUN=function(g) nrow(g)))))
    sic$apicall_start_interval <- rownames(sic)
    colnames(sic) <- c("count", "apicall_start_interval")
    
    #sic = sic[order(sic$count, decreasing=TRUE),]  # sore decreasing
    sic$apicall_start_interval <- as.numeric(sic$apicall_start_interval)
    
    
    DT::datatable(sic, selection = 'single', class = 'cell-border strip hover')
  }
  
})













