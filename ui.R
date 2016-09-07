library(shiny)
library(DT)
library(markdown)
library(ggplot2)
library(datasets)
library(shinyBS)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  
  titlePanel = ("Migu风控系统1.0.2"),
  
  #Title
  fluidRow(
      h1("Migu风控系统",align="center")
  ),
  #汇总信息
  fluidRow(
     column(2, h3('AntiRisk 102'))
  ),
  br(),
  fluidRow(
    sidebarLayout(
      
      sidebarPanel(width=3,  
        
                   wellPanel(
                       fluidRow(h3("选择数据")),
                       
                       fluidRow(
                         textInput("min_records_threshold", "最少记录条数", value = 0),
                         
                         actionButton("go_update", "更新数据")
                       )
                  ),
                   
                   
                   wellPanel(
                     fluidRow(h3("App 详细信息")),
                     fluidRow(
                       
                       textInput("select_id", "已选APPID", value = "00000000"),
                       
                       actionButton("go", "查看")
                     ),
                     
                     bsModal("modalExample", 
                             "EMMC 异常统计信息", 
                             "go", 
                             size = "large", 
                             
                             tabsetPanel(
                               id='tabpanel_app_detail',
                               
                               tabPanel('EMMC 汇总表',
                                        actionButton("go_emmc", "查看"),
                                        DT::dataTableOutput('app_imei_detail')
                               )
                             )
                     )
                  )
       ),
      
      
      #main nav bar pannel
      mainPanel(
        
        tabsetPanel(
          id='tabpanel',
          tabPanel('IMEI异常',
                   value = "ab_imei",
                   #downloadButton('downloadData_1', 'Download'),
                   DT::dataTableOutput('imei_abnormal')),
          
          tabPanel('SI异常',
                   value = "ab_start_interval",
                   #downloadButton('downloadData_2', 'Download'),
                   DT::dataTableOutput('start_time_interval_abnormal'))
        )
        
 
      ) 
      
    )
  )
  )
)

