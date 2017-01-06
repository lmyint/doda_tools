library(shiny)

source("parser.R")

hrObj <- hr(style = "height: 10px; color: #8cc63f; background-color: #8cc63f; width: 80%")

ui <- fluidPage(
    titlePanel("Test UI parser"),
    sidebarLayout(
        sidebarPanel(
            p("Enter the path to an experiment Rmd file to preview what it will look like in DoDA."),
            p("All of the variations are displayed sequentially, separated by horizontal bars."),
            width = 3
        ),
        mainPanel(
            fluidRow(
                column(width = 8,
                    textInput("filename",
                        label = "Enter the filename (on your computer) of the experiment Rmd file:",
                        value = "/path/to/file/experiment.Rmd",
                        width = "100%")
                ),
                column(width = 2,
                    tags$br(),
                    actionButton("parse_file", label = "Parse")
                )
            ),
            hrObj,
            uiOutput("ui_assign")
        )
    )
)

server <- function(input, output, session) {
    observeEvent(input$parse_file, {
        uiList <- parseExperimentRmd(input$filename)
        concatUI <- do.call(c, lapply(uiList, function(obj) {
            list(obj, hrObj)
        }))
        output$ui_assign <- renderUI({
            concatUI
        })
    })
}

shinyApp(ui = ui, server = server)