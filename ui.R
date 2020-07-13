library(shiny)
source("COHb.R")
ui <- fluidPage(
  textOutput("abstract"),
  plotOutput("timePlot"),
  tabsetPanel(
    tabPanel("Sample",
             fluidRow(column(4,textInput("ID", label = "Sample Number:", value = "#"))),
             fluidRow(column(4,selectInput("COHb_method", label = "COHb known from:", c("SpCO","blood","breath")))),
             fluidRow(
               column(4,numericInput("XCOHb", label = "COHb in blood (%):", value = 18.4)),
               column(4,
                      conditionalPanel(condition="input.COHb_method!='breath'","Standard Deviation:",verbatimTextOutput("XCOHb.sd")),
                      conditionalPanel(condition="input.COHb_method=='breath'",numericInput("XCOHb.sd", label = "Standard Deviation (%):", value = 0))
                      ),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("XCOHb.rsd")))
             ),
             fluidRow(column(4,selectInput("Hb_method", label = "Hb known from:", c("gender","blood")))),
             fluidRow(
               column(4,
                      conditionalPanel(condition="input.Hb_method!='blood'","Hemoglobin in blood:",verbatimTextOutput("Hb")),
                      conditionalPanel(condition="input.Hb_method=='blood'",numericInput("Hb", label = "Hemoglobin in blood (grams/100mL):", value = 15.8))
                      ),
               column(4,
                      conditionalPanel(condition="input.Hb_method!='blood'","Standard Deviation:",verbatimTextOutput("Hb.sd")),
                      conditionalPanel(condition="input.Hb_method=='blood'",numericInput("Hb.sd", label = "Standard Deviation (grams/100mL):", value = 0))
                      ),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("Hb.rsd")))
             ),
             conditionalPanel("input.intermediate",
                              fluidRow(
                                column(4,"Fraction of COHb in blood sample:",verbatimTextOutput("COHb.D")),
                                conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("COHb.D.sd")))
                                )
             )
    ),
    tabPanel("Employee",
             fluidRow(column(4,textInput("name", label = "Employee Name:")),
                      column(4,selectInput("gender", label = "Gender", c("male","female")))
             ),
             fluidRow(
               column(4,numericInput("h", label = "Height (inches):", value = 67)),
               column(4,numericInput("h.sd", label = "Standard Deviation (inches):", value = 1)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("h.rsd")))
             ),
             fluidRow(
               column(4,numericInput("w", label = "Weight (pounds):", value = 166)),
               column(4,numericInput("w.sd", label = "Standard Deviation (pounds):", value = 2)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("w.rsd")))
             ),
             fluidRow(
               column(4,selectInput("SS_method", label = "Smoker Status method", c("nonSmoker","cigarettes","level")))
               ),
             conditionalPanel(condition="input.SS_method=='cigarettes'",fluidRow(
               column(4,numericInput("cigarettes", label = "Cigarettes smoked per day:", value = 0, min = 0)),
               column(4,numericInput("cigarettes.sd", label = "Standard Deviation:", value = 0)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("cigarettes.rsd")))
             )),
             conditionalPanel(condition="input.SS_method=='level'",fluidRow(
               column(4,numericInput("SS", label = "Smoker Status:", value = 0, min = 0, max = 4)),
               column(4,"Standard Deviation",verbatimTextOutput("SS.sd")),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("SS.rsd")))
             )),
             conditionalPanel("input.intermediate",
                              fluidRow(
                                column(4,"Estimated blood volume of employee:",verbatimTextOutput("Vb")),
                                conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("Vb.sd")))
                              ),
                              fluidRow(
                                column(4,"Fraction of COHb in blood prior to exposure:",verbatimTextOutput("XCOHb.A")),
                                conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("XCOHb.A.sd")))
                              ),
                              fluidRow(
                                column(4,"COHb in blood prior to exposure:",verbatimTextOutput("COHb.A")),
                                conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("COHb.A.sd")))
                              )
             )
    ),
    tabPanel("Environment",
             fluidRow(column(4,selectInput("PB_method", label = "Pressure method", c("elevation","pressure")))),
             conditionalPanel(condition="input.PB_method=='elevation'",
                              fluidRow(
                                column(4,numericInput("z", label = "Elevation (ft):", value = 0)),
                                column(4,numericInput("z.sd", label = "Standard Deviation (ft):", value = 100)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("z.rsd")))
                                ),
                              fluidRow(
                                column(4,"Atmospheric Pressure:",verbatimTextOutput("PB")),
                                column(4,"Standard Deviation:",verbatimTextOutput("PB.sd"))
                                )
             ),
             conditionalPanel(condition="input.PB_method=='pressure'",
                              fluidRow(
                                column(4,numericInput("PB", label = "Atmospheric Pressure (mmHg):", value = 740)),
                                column(4,numericInput("PB.sd", label = "Standard Deviation (mmHg):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("PB.rsd")))
                              )
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
             fluidRow(
               column(4,numericInput("t_e", label = "Duration (minutes):", value = 350)),
               column(4,numericInput("t_e.sd", label = "Standard Deviation (minutes):", value = 15)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("t_e.rsd")))
             ),
             fluidRow(
               column(4,numericInput("AL_e", label = "Activity Level:", value = 1)),
               column(4,numericInput("AL_e.sd", label = "Standard Deviation:", value = 0.5)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("AL_e.rsd")))
             ),
             conditionalPanel("input.advanced",
                              fluidRow(
                                column(4,numericInput("x.O2_e", label = "Oxygen level (% oxygen):", value = 21)),
                                column(4,numericInput("x.O2_e.sd", label = "Standard Deviation (% oxygen):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("x.O2_e.rsd")))
                              )
             ),
             fluidRow(
               column(4,numericInput("x.CO_e.s", label = "CO exposure from smoking (ppm):", value = 0)),
               column(4,numericInput("x.CO_e.s.sd", label = "Standard Deviation (ppm):", value = 0)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("x.CO_e.s.rsd")))
             ),
             conditionalPanel("input.intermediate",
                              fluidRow(column(4,"VA:",verbatimTextOutput("VA_e")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("VA_e.sd")))),
                              fluidRow(column(4,"DL:",verbatimTextOutput("DL_e")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("DL_e.sd")))),
                              fluidRow(column(4,"beta:",verbatimTextOutput("beta_e")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("beta_e.sd")))),
                              fluidRow(column(4,"PICO:",verbatimTextOutput("PICO_e")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("PICO_e.sd")))),
                              fluidRow(column(4,"PCO2:",verbatimTextOutput("PCO2_e")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("PCO2_e.sd")))),
                              fluidRow(column(4,"x.CO_e:",verbatimTextOutput("x.CO_e")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("x.CO_e.sd")))),
                              fluidRow(column(4,"x.CO_e.o:",verbatimTextOutput("x.CO_e.o")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("x.CO_e.o.sd")))),
                              fluidRow(column(4,"8-hour total weight average (TWA) exposure:",verbatimTextOutput("TWA8Hours")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("TWA8Hours.sd"))))
             )
    ),
    tabPanel("Clearance",
             fluidRow(
               column(4,numericInput("t_c", label = "Duration (minutes):", value = 160)),
               column(4,numericInput("t_c.sd", label = "Standard Deviation (minutes):", value = 15)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("t_c.rsd")))
             ),
             fluidRow(
               column(4,numericInput("AL_c", label = "Activity Level:", value = 0)),
               column(4,numericInput("AL_c.sd", label = "Standard Deviation:", value = 0.5)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("AL_c.rsd")))
             ),
             conditionalPanel("input.advanced",
                              fluidRow(
                                column(4,numericInput("x.O2_c", label = "Oxygen level (% oxygen):", value = 21)),
                                column(4,numericInput("x.O2_c.sd", label = "Standard Deviation (% oxygen):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("x.O2_c.rsd")))
                              ),
                              fluidRow(
                                column(4,numericInput("x.CO_c", label = "Carbon Monoxide Level (ppm):", value = 2)),
                                column(4,numericInput("x.CO_c.sd", label = "Standard Deviation (ppm):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("x.CO_c.rsd")))
                              )
             ),
             conditionalPanel("input.intermediate",
                              fluidRow(column(4,"VA:",verbatimTextOutput("VA_c")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("VA_c.sd")))),
                              fluidRow(column(4,"DL:",verbatimTextOutput("DL_c")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("DL_c.sd")))),
                              fluidRow(column(4,"beta:",verbatimTextOutput("beta_c")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("beta_c.sd")))),
                              fluidRow(column(4,"PICO:",verbatimTextOutput("PICO_c")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("PICO_c.sd")))),
                              fluidRow(column(4,"PCO2:",verbatimTextOutput("PCO2_c")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("PCO2_c.sd")))),
                              fluidRow(column(4,"Concentration of COHb prior to clearance:",verbatimTextOutput("COHb.B")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("COHb.B.sd")))),
                              fluidRow(column(4,"Fraction of COHb prior to clearance:",verbatimTextOutput("XCOHb.B")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("XCOHb.B.sd"))))
             )
    ),
    tabPanel("Oxygen Therapy",
             fluidRow(
               column(4,numericInput("t_t", label = "Duration (minutes):", value = 0)),
               column(4,numericInput("t_t.sd", label = "Standard Deviation (minutes):", value = 0)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("t_t.rsd")))
             ),
             conditionalPanel("input.advanced",
                              fluidRow(
                                column(4,numericInput("AL_t", label = "Activity Level:", value = 0)),
                                column(4,numericInput("AL_t.sd", label = "Standard Deviation:", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("AL_t.rsd")))
                              )
             ),
             fluidRow(
               column(4,numericInput("x.O2_t", label = "Oxygen level (% oxygen):", value = 100)),
               column(4,numericInput("x.O2_t.sd", label = "Standard Deviation (% oxygen):", value = 0)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("x.O2_t.rsd")))
             ),
             conditionalPanel("input.advanced",
                              fluidRow(
                                column(4,numericInput("x.CO_t", label = "Carbon Monoxide Level (ppm):", value = 0)),
                                column(4,numericInput("x.CO_t.sd", label = "Standard Deviation (ppm):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("x.CO_t.rsd")))
                              )
             ),
             conditionalPanel("input.intermediate",
                              fluidRow(column(4,"VA:",verbatimTextOutput("VA_t")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("VA_t.sd")))),
                              fluidRow(column(4,"DL:",verbatimTextOutput("DL_t")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("DL_t.sd")))),
                              fluidRow(column(4,"beta:",verbatimTextOutput("beta_t")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("beta_t.sd")))),
                              fluidRow(column(4,"PICO:",verbatimTextOutput("PICO_t")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("PICO_t.sd")))),
                              fluidRow(column(4,"PCO2:",verbatimTextOutput("PCO2_t")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("PCO2_t.sd")))),
                              fluidRow(column(4,"Concentration of COHb prior to therapy:",verbatimTextOutput("COHb.C")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("COHb.C.sd")))),
                              fluidRow(column(4,"Fraction of COHb prior to therapy:",verbatimTextOutput("XCOHb.C")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("XCOHb.C.sd"))))
             )
    ),
    tabPanel("Standard",
             "This is where the user will select the standard to be compared against."
    ),
    tabPanel("Parameters",
             fluidRow(column(4,numericInput("deltaT", label = "Runge–Kutta time step (seconds):", value = 60))),
             checkboxInput("advanced", "Show advanced features", FALSE),
             checkboxInput("intermediate", "Show intermediate values", FALSE),
             checkboxInput("doMonteCarlo", "Perform Monte Carlo simulations", FALSE),
             conditionalPanel("input.doMonteCarlo",fluidRow(column(4,numericInput("n", label = "Number of Monte Carlo simulations:", value = 100)))),
             checkboxInput("showRSD", "Show relative standard deviation values", FALSE)
    ),
    tabPanel("Summary"
             #tableOutput("CSVcontents")
    )
  ),
  hr(),
  fluidRow(
    column(2,downloadButton("downloadData", "Download")),
    column(4,fileInput("CSVfile", "Upload CSV",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))),
    column(4,fileInput("PRNfile", "Upload PRN",multiple = FALSE,accept = c(".prn")))
  ),
  textOutput("CSVcontents"),
  textOutput("PRNcontents")
)