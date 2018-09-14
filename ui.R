library(shiny)
source("COHb.R")
ui <- fluidPage(
  textOutput("abstract"),
  plotOutput("timePlot"),
  tabsetPanel(
    tabPanel("Employee",
             fluidRow(
               column(4,textInput("name", label = "Employee Name:")),
               column(4,selectInput("gender", label = "Gender", c("male","female"))),
               column(4,textInput("ID", label = "Sample Number:", value = "#"))
             ),
             fluidRow(
               column(4,numericInput("h", label = "Height (inches):", value = 73))
               #column(4,numericInput("h.sd", label = "Uncertainty in height (inches):", value = 1)),
               #column(4,"Relative standard deviation",verbatimTextOutput("h.rsd"))
             ),
             fluidRow(
               column(4,numericInput("w", label = "Weight (pounds):", value = 264)),
               column(4,numericInput("w.sd", label = "Uncertainty in weight (pounds):", value = 2)),
               column(4,"Relative standard deviation",verbatimTextOutput("w.rsd"))
             ),
             conditionalPanel("input.intermediate",
                              fluidRow(column(4,"Estimated blood volume of employee:",verbatimTextOutput("Vb"))),
                              fluidRow(column(4,"XCOHb.A:",verbatimTextOutput("XCOHb.A"))),
                              fluidRow(column(4,"COHb.A:",verbatimTextOutput("COHb.A")))
             )
    ),
    tabPanel("Blood",
             
             fluidRow(
               column(4,numericInput("XCOHb", label = "COHb in blood sample (%):", value = 18.2))
               #column(4,numericInput("XCOHb.sd", label = "Uncertainty in COHb in blood sample (%):", value = 0.1)),
               #column(4,"Relative standard deviation",verbatimTextOutput("XCOHb.rsd"))
             ),
             fluidRow(
               column(4,numericInput("Hb", label = "Hemoglobin in blood (grams/100mL):", value = 19.3))
               #column(4,offset = 4,"Relative standard deviation",verbatimTextOutput("Hb.rsd"))
             ),
             conditionalPanel("input.intermediate",
                              fluidRow(column(12,"Concentration of COHb in blood sample:",verbatimTextOutput("COHb.D")))
                              )
    ),
    tabPanel("Environment",
             fluidRow(column(4,selectInput("PB_method", label = "Method", c("elevation","pressure")))),
             conditionalPanel(condition="input.PB_method=='elevation'",
                              fluidRow(column(4,numericInput("z", label = "Elevation (ft):", value = 0))),
                              fluidRow(column(4,"Atmospheric Pressure:",verbatimTextOutput("PB")))
             ),
             conditionalPanel(condition="input.PB_method=='pressure'",
                              fluidRow(column(4,numericInput("PB", label = "Atmospheric Pressure (mmHg):", value = 760)))
             )
             #fluidRow(
               #column(4,numericInput("z", label = "Elevation (feet):", value = 700)),
               #column(4,numericInput("z.sd", label = "Uncertainty in elevation (feet):", value = 100)),
               #column(4,"Relative standard deviation",verbatimTextOutput("z.rsd"))
             #),
             #fluidRow(
               #column(4,numericInput("T", label = "Temperature (°F):", value = 70)),
               #column(4,numericInput("T.sd", label = "Uncertainty in temperature (°F):", value = 2)),
               #column(4,"Relative standard deviation",verbatimTextOutput("T.rsd"))
             #)
    ),
    tabPanel("Exposure",
             fluidRow(column(4,numericInput("t_e", label = "Exposure Duration (minutes):", value = 160))),
             fluidRow(column(4,numericInput("AL_e", label = "Activity Level:", value = 1.5))),
             fluidRow(column(4,numericInput("SS", label = "Smoker Status:", value = 0))),
             fluidRow(column(4,numericInput("x.O2_e", label = "Oxygen level (% oxygen):", value = 21))),
             fluidRow(column(4,numericInput("x.CO_e.s", label = "CO exposure due to smoking on the job (ppm):", value = 12))),
             conditionalPanel("input.intermediate",
                              fluidRow(column(4,"VA:",verbatimTextOutput("VA_e"))),
                              fluidRow(column(4,"DL:",verbatimTextOutput("DL_e"))),
                              fluidRow(column(4,"beta:",verbatimTextOutput("beta_e"))),
                              fluidRow(column(4,"PICO:",verbatimTextOutput("PICO_e"))),
                              fluidRow(column(4,"PCO2:",verbatimTextOutput("PCO2_e"))),
                              fluidRow(column(4,"x.CO_e:",verbatimTextOutput("x.CO_e"))),
                              fluidRow(column(4,"x.CO_e.o:",verbatimTextOutput("x.CO_e.o")))
             )
    ),
    tabPanel("Clearance",
             fluidRow(column(4,numericInput("t_c", label = "Clearance duration (minutes):", value = 70))),
             fluidRow(column(4,numericInput("AL_c", label = "Activity Level:", value = 0))),
             fluidRow(column(4,numericInput("x.O2_c", label = "Oxygen level (% oxygen):", value = 21))),
             fluidRow(column(4,numericInput("x.CO_c", label = "Carbon Monoxide Level (ppm):", value = 2.847))),
             conditionalPanel("input.intermediate",
                              fluidRow(column(4,"VA:",verbatimTextOutput("VA_c"))),
                              fluidRow(column(4,"DL:",verbatimTextOutput("DL_c"))),
                              fluidRow(column(4,"beta:",verbatimTextOutput("beta_c"))),
                              fluidRow(column(4,"PICO:",verbatimTextOutput("PICO_c"))),
                              fluidRow(column(4,"PCO2:",verbatimTextOutput("PCO2_c"))),
                              fluidRow(column(4,"COHb.B:",verbatimTextOutput("COHb.B"))),
                              fluidRow(column(4,"XCOHb.B:",verbatimTextOutput("XCOHb.B")))
             )
    ),
    tabPanel("Oxygen Therapy",
             fluidRow(column(4,numericInput("t_t", label = "Oxygen therapy duration (minutes):", value = 8))),
             fluidRow(column(4,numericInput("AL_t", label = "Activity Level:", value = 0))),
             fluidRow(column(4,numericInput("x.O2_t", label = "Oxygen level (% oxygen):", value = 50))),
             fluidRow(column(4,numericInput("x.CO_t", label = "Carbon Monoxide Level (ppm):", value = 0))),
             conditionalPanel("input.intermediate",
                              fluidRow(column(4,"VA:",verbatimTextOutput("VA_t"))),
                              fluidRow(column(4,"DL:",verbatimTextOutput("DL_t"))),
                              fluidRow(column(4,"beta:",verbatimTextOutput("beta_t"))),
                              fluidRow(column(4,"PICO:",verbatimTextOutput("PICO_t"))),
                              fluidRow(column(4,"PCO2:",verbatimTextOutput("PCO2_t"))),
                              fluidRow(column(4,"COHb.C:",verbatimTextOutput("COHb.C"))),
                              fluidRow(column(4,"XCOHb.C:",verbatimTextOutput("XCOHb.C")))
             )
    ),
    tabPanel("Standard",
             "This is where the user will select the standard to be compared against."
    ),
    tabPanel("Parameters",
             checkboxInput("intermediate", "Show intermediate values", FALSE),
             fluidRow(column(4,numericInput("deltaT", label = "Runge–Kutta time step (seconds):", value = 60)))
    ),
    tabPanel("Summary",
             tableOutput("CSVcontents")
    )
  ),
  hr(),
  fluidRow(
    column(2,downloadButton("downloadData", "Download")),
    column(4,fileInput("CSVfile", "Upload CSV",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))),
    column(4,fileInput("PRNfile", "Upload PRN",multiple = FALSE,accept = c(".prn")))
  ),
  textOutput("PRNcontents")
    )