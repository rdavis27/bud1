fyear <- 2018
shinyUI(fluidPage(
    #comment out scripts for testing
    tags$head(includeScript("analytics.js")),
    tags$head(includeScript("statcount.js")),
    headerPanel(paste0("Budget of the United States Government, FY ", fyear)),
    sidebarPanel(
        width = 2,
        selectInput("topic", "Topic",
                    choices  = c("Debt", "Deficit", "Receipts", "Outlays", "Outlays2", "Outlays3"),
                    selected =   "Debt"),
        selectInput("xunits", "Units",
                    choices  = c("Actual Dollars","Real Dollars","Percent of GDP"),
                    selected =                                   "Percent of GDP"),
        selectInput("graph", "Graph Variables",
                    choices  = c("GROSS_DEBT","PUBLIC_DEBT","GOV_ACC_DEBT","OASDI_DEBT","MEDI_DEBT"),
                    selected = c("GROSS_DEBT","PUBLIC_DEBT"), multiple = TRUE),
        checkboxInput("print", "Print Variables"),
        textInput("xscale", "X From,To,Step,Tick", value = ""),
        textInput("yscale", "Y From,To,Step,Tick", value = ""),
        numericInput("growth", "Years to Measure Growth", 0),
        selectInput("theme", "Theme",
                    choice   = c("theme_gray80","theme_gray85","theme_gray","theme_bw","theme_classic","theme_dark","theme_light","theme_linedraw","theme_minimal"),
                    selected =   "theme_gray85"),
        textInput("color", "Color", value = ""),
        textInput("shape", "Shape", value = ""),
        checkboxInput("ignore", "Ignore URL Parameters") ),
    mainPanel(
        div(
            tabsetPanel(id = "tabs",
                tabPanel("Output",
                    width = 10,
                    plotOutput("myggPlot", width = "900", height = "600"),
                    #plotOutput("myPlot", width = "100%", height = "600"),
                    verbatimTextOutput("myText")
                ),
                tabPanel("Usage",
                    width = 10,
                    includeMarkdown("bud_usage.Rmd")
                )
            )
        ),
        width = 10)
    )
)
