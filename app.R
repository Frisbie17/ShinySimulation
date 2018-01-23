library(shiny)
library(shinydashboard)

source("global.R")


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "M/M/1 Queue DES Demo"),
                    dashboardSidebar(
                      sidebarMenu(id = "tabs",
                                  numericInput('meanIAT', 'Mean Intra-Arrival Time (secs)', 30, 0, 1000, 1),
                                  numericInput('meanST', 'Mean Service Time (secs)', 25, 0, 1000, 1),
                                  actionButton("setModel", "Set Model"),
                                  hr(),
                                  actionButton("runModel", "Run Model"),
                                  hr(),
                                  actionButton("getOutput", "Get Results"),
                                  hr(),
                                  downloadButton('download', label = "Download Code Set")
                      ) #End SidebarMenu
                      
                    ), #End dashboardSidebar
                    
                    dashboardBody(
                           h2("Output"),
                           h4(textOutput("simQLA")),
                           h4(textOutput("simAQT")),
                           h4(textOutput("cldQLA")),
                           h4(textOutput("cldAQT"))

                  ) #End Body
) #End UI


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #################################################
  ##   Download Functions for Code Base         ###
  #################################################
  
  output$download <- downloadHandler(
    filename = "MM1_Example.zip",
    
     #content is a function with argument file. content writes the plot to the device
    content = function(file) {
      file.copy("MM1_Example.zip", file)
    },
    
    contentType = "application/zip"
  )
  

  ###Make the cfg file
  observeEvent(input$setModel, {

  makeCfg(BaseCfgFilePath, CfgOutputFilePath, input$meanIAT, input$meanST)
    
    }) #END Observe Event make Cfg file

  observeEvent(input$runModel, {
    
    withProgress(message = 'Simulation in progress', value = 0,{
      for (i in 1:20){
        incProgress(1/20)
        Sys.sleep(.25)
      }
    })
    
    runJaamSim(JarFilePath, CfgOutputFilePath)

  }) #END Observe Event Run JaamSim Model
  
  observeEvent(input$getOutput, {

    result <- readJaamSimOutput(OutputFilePath)

    sQLA <- result[1]
    sAQT <- result[2]
    
    cQLA <- closedAQL(input$meanIAT, input$meanST)
    cAQT <- closedAQT(cQLA, input$meanIAT)
    
    output$simQLA <- renderText(paste0("Simulation mean queue length is: ", round(sQLA, 1), "."))
    output$simAQT <- renderText(paste0("Simulation mean time in queue is: ", round(sAQT, 2), " seconds."))
    output$cldQLA <- renderText(paste0("Closed form equation mean queue length is: ", round(cQLA, 1), "."))
    output$cldAQT <- renderText(paste0("Closed form equation mean time in queue is: ", round(cAQT, 2), " seconds."))
    
    })#End Observe Event Get Model Output
  
} #END SERVER

# Run the application 
shinyApp(ui = ui, server = server)

