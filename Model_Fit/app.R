load("fit.Rdata")
source("plot_para.R")
library(shiny)
library(tidyverse)
library(rlang)
library(gridExtra)
# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(9,
           wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 860px;",
                     plotOutput("plot1", height = 800))),
    column(3,
           fluidRow(column(12,
                           checkboxGroupInput("var", 
                                              label = "Choose a model to present",
                                              choices = list("best_fit",
                                                             "s_fit",
                                                             "single_fit",
                                                             "double_fit",
                                                             "quad_fit",
                                                             "abrupt_fit"),
                                              selected = "Percent White"))
                    ),
           fluidRow(column(12,
                           checkboxGroupInput("id", 
                                              label = "Choose a region to present",
                                              choices = list("1","2","3", "4","5", 
                                                             "6","7","8","9","10",
                                                             "11","12","13","14" ,"15",
                                                             "16","17","18"),
                                              selected = "Percent White"))
           )
           )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  ignore <- reactive({
    input$var
  })
  id <- reactive({
    as.numeric(input$id)
  })
  output$plot1 <- renderPlot({
    plot_final <- list()
    for(i in 1:length(id())){
      n = id()[i]
      p <- plot_para(est[[n]][[1]], est[[n]][[2]],est[[n]][[3]],n=n,MOV[[n]],Region_list[[n]],ignore=ignore())
      plot_final[[3 * (i - 1) + 1]] <- p[[1]]
      plot_final[[3 * (i - 1) + 2]] <- p[[2]]
      plot_final[[3 * (i - 1) + 3]] <- p[[3]]
    }
    plot_final[["ncol"]] <- 3
    do.call(grid.arrange, plot_final)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
