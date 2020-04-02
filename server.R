library(shiny)
source("COHb.R")
server <- function(input, output, session) {
  OEL = 35*ppm
  OelLabel = "OSHA PEL"
  XCOHb = reactive(input$XCOHb*percent)
  Hb = reactive(input$Hb*gram/(100*mL))
  COHb.D = reactive(COHb(XCOHb=XCOHb(),Hb=Hb()))
  PB = reactive(
    if (input$PB_method=='elevation') {P(input$z*ft)} # I need to implement the function in the library here
    else if (input$PB_method=='pressure') {input$PB*mmHg}
  )
  output$PB = renderPrint(cat(PB()/mmHg,"mmHg"))
  t_t = reactive(input$t_t*minute)
  AL_t = reactive(input$AL_t)
  x.O2_t = reactive(input$x.O2_t*percent)
  x.CO_t = reactive(input$x.CO_t*ppm)
  t_c = reactive(input$t_c*minute)
  AL_c = reactive(input$AL_c)
  x.O2_c = reactive(input$x.O2_c*percent)
  x.CO_c = reactive(input$x.CO_c*ppm)
  SS = reactive(input$SS)
  t_e = reactive(input$t_e*minute)
  AL_e = reactive(input$AL_e)
  x.O2_e = reactive(input$x.O2_e*percent)
  x.CO_e.s = reactive(input$x.CO_e.s*ppm)
  
  VA_t = reactive(VA(AL=AL_t(),PB=PB()))
  output$VA_t = renderPrint(cat(VA_t()/(liter/minute),"liter/minute"))
  
  DL_t = reactive(DL(AL=AL_t(),PB=PB(),x.O2=x.O2_t()))
  output$DL_t = renderPrint(cat(DL_t()/(mL/minute/mmHg),"mL/minute/mmHg"))
  
  beta_t = reactive(beta(PB=PB(),DL=DL_t(),VA=VA_t()))
  output$beta_t = renderPrint(cat(beta_t()/(mmHg/mL),"mmHg*second/mL"))
  
  PICO_t = reactive(PICO(PB=PB(),x.CO=x.CO_t()))
  output$PICO_t = renderPrint(cat(PICO_t()/mmHg,"mmHg"))
  
  PCO2_t = reactive(PCO2(PB=PB(),x.CO=x.CO_t(),x.O2=x.O2_t()))
  output$PCO2_t = renderPrint(cat(PCO2_t()/mmHg,"mmHg"))
  
  COHb.C = reactive(findInitCOHb(t=t_t(),COHb.f=COHb.D(),Vb=Vb(),beta=beta_t(),PICO=PICO_t(),PCO2=PCO2_t(),Hb=Hb()))
  output$COHb.C = renderPrint(cat(COHb.C()))
  
  XCOHb.C = reactive(COHb.C()/Hf/Hb())
  output$XCOHb.C = renderPrint(cat(XCOHb.C()/percent,"%"))
  
  VA_c = reactive(VA(AL=AL_c(),PB=PB()))
  output$VA_c = renderPrint(cat(VA_c()/(liter/minute),"liter/minute"))
  
  DL_c = reactive(DL(AL=AL_c(),PB=PB(),x.O2=x.O2_c()))
  output$DL_c = renderPrint(cat(DL_c()/(mL/minute/mmHg),"mL/minute/mmHg"))
  
  beta_c = reactive(beta(PB=PB(),DL=DL_c(),VA=VA_c()))
  output$beta_c = renderPrint(cat(beta_c()/(mmHg/mL),"mmHg*second/mL"))
  
  PICO_c = reactive(PICO(PB=PB(),x.CO=x.CO_c()))
  output$PICO_c = renderPrint(cat(PICO_c()/mmHg,"mmHg"))
  
  PCO2_c = reactive(PCO2(PB=PB(),x.CO=x.CO_c(),x.O2=x.O2_c()))
  output$PCO2_c = renderPrint(cat(PCO2_c()/mmHg,"mmHg"))
  
  COHb.B = reactive(findInitCOHb(t=t_c(),COHb.f=COHb.C(),Vb=Vb(),beta=beta_c(),PICO=PICO_c(),PCO2=PCO2_c(),Hb=Hb()))
  output$COHb.B = renderPrint(cat(COHb.B()))
  
  XCOHb.B = reactive(COHb.B()/Hf/Hb())
  output$XCOHb.B = renderPrint(cat(XCOHb.B()/percent,"%"))
  
  XCOHb.A = reactive(XCOHb.0(SS=SS()))
  output$XCOHb.A = renderPrint(cat(XCOHb.A()/percent,"%"))
  
  COHb.A = reactive(Hf*Hb()*XCOHb.A())
  output$COHb.A = renderPrint(cat(COHb.A()))
  
  DL_e = reactive(DL(AL=AL_e(),PB=PB(),x.O2=x.O2_e()))
  output$DL_e = renderPrint(cat(DL_e()/(mL/minute/mmHg),"mL/minute/mmHg"))
  
  VA_e = reactive(VA(AL=AL_e(),PB=PB()))
  output$VA_e = renderPrint(cat(VA_e()/(liter/minute),"liter/minute"))
  
  beta_e = reactive(beta(PB=PB(),DL=DL_e(),VA=VA_e()))
  output$beta_e = renderPrint(cat(beta_e()/(mmHg/mL),"mmHg*second/mL"))
  
  x.CO_e = reactive(findMeanCO(t=t_e(),COHb.i=COHb.A(),COHb.f=COHb.B(),Vb=Vb(),beta=beta_e(),Hb=Hb(),PB=PB(),x.O2=x.O2_e()))
  output$x.CO_e = renderPrint(cat(x.CO_e()/ppm,"ppm"))
  
  PICO_e = reactive(PICO(PB=PB(),x.CO=x.CO_e()))
  output$PICO_e = renderPrint(cat(PICO_e()/mmHg,"mmHg"))
  
  PCO2_e = reactive(PCO2(PB=PB(),x.CO=x.CO_e(),x.O2=x.O2_e()))
  output$PCO2_e = renderPrint(cat(PCO2_e()/mmHg,"mmHg"))
  
  x.CO_e.o = reactive(x.CO_e()-x.CO_e.s())
  output$x.CO_e.o = renderPrint(cat(x.CO_e.o()/ppm,"ppm"))
  
  AveragingPeriod8h = reactive(ifelse(t_e() > 480*minute, t_e(), 480*minute))
  TWA8Hours = reactive(x.CO_e.o()*t_e()/AveragingPeriod8h())
  
  
  h.rsd = reactive(input$h.sd/input$h)
  output$h.rsd = renderText(h.rsd())
  w.rsd = reactive(input$w.sd/input$w)
  output$w.rsd = renderText(w.rsd())
  Vb = reactive({
    w = input$w*pound
    h = input$h*inch
    Vb.m(W=w,H=h)
  })

  output$Vb = renderPrint(cat(Vb()/liter,"liter"))
  output$COHb.D = renderPrint(cat(COHb.D()))
  
  PB.rsd = reactive(input$PB.sd/input$PB)
  output$PB.rsd = renderText(PB.rsd())
  z.rsd = reactive(input$z.sd/input$z)
  output$z.rsd = renderText(z.rsd())
  T.rsd = reactive(input$T.sd/input$T)
  output$T.rsd = renderText(T.rsd())
  t_e.rsd = reactive(input$t_e.sd/input$t_e)
  output$t_e.rsd = renderText(t_e.rsd())
  AL_e.rsd = reactive(input$AL_e.sd/input$AL_e)
  output$AL_e.rsd = renderText(AL_e.rsd())
  SS.rsd = reactive(input$SS.sd/input$SS)
  output$SS.rsd = renderText(SS.rsd())
  x.O2_e.rsd = reactive(input$x.O2_e.sd/input$x.O2_e)
  output$x.O2_e.rsd = renderText(x.O2_e.rsd())
  XCOHb.rsd = reactive(input$XCOHb.sd/input$XCOHb)
  output$XCOHb.rsd = renderText(XCOHb.rsd())
  Hb.rsd = reactive(input$Hb.sd/input$Hb)
  output$Hb.rsd = renderText(Hb.rsd())
  PB.rsd = reactive(input$PB.sd/input$PB)
  output$PB.rsd = renderText(PB.rsd())
  x.CO_e.s.rsd = reactive(input$x.CO_e.s.sd/input$x.CO_e.s)
  output$x.CO_e.s.rsd = renderText(x.CO_e.s.rsd())
  t_c.rsd = reactive(input$t_c.sd/input$t_c)
  output$t_c.rsd = renderText(t_c.rsd())
  AL_c.rsd = reactive(input$AL_c.sd/input$AL_c)
  output$AL_c.rsd = renderText(AL_c.rsd())
  x.O2_c.rsd = reactive(input$x.O2_c.sd/input$x.O2_c)
  output$x.O2_c.rsd = renderText(x.O2_c.rsd())
  x.CO_c.rsd = reactive(input$x.CO_c.sd/input$x.CO_c)
  output$x.CO_c.rsd = renderText(x.CO_c.rsd())
  t_t.rsd = reactive(input$t_t.sd/input$t_t)
  output$t_t.rsd = renderText(t_t.rsd())
  AL_t.rsd = reactive(input$AL_t.sd/input$AL_t)
  output$AL_t.rsd = renderText(AL_t.rsd())
  x.O2_t.rsd = reactive(input$x.O2_t.sd/input$x.O2_t)
  output$x.O2_t.rsd = renderText(x.O2_t.rsd())
  x.CO_t.rsd = reactive(input$x.CO_t.sd/input$x.CO_t)
  output$x.CO_t.rsd = renderText(x.CO_t.rsd())
  
  output$timePlot <- renderPlot({
    t.e = 0:t_e()
    t.e = seq(0,t_e(),input$deltaT)
    e = rk4(y=COHb.A(),t=t.e,parms=NULL,func=CFK,Vb=Vb(),beta=beta_e(),PICO=PICO_e(),PCO2=PCO2_e(),Hb=Hb())
    t.c = seq(t_e(),(t_c()+t_e()),input$deltaT)
    c = rk4(y=COHb.B(),times=t.c,parms=NULL,func=CFK,Vb=Vb(),beta=beta_c(),PICO=PICO_c(),PCO2=PCO2_c(),Hb=Hb())
    t.o = seq((t_c()+t_e()),(t_c()+t_e()+t_t()),input$deltaT)
    o = rk4(y=COHb.C(),times=t.o,parms=NULL,func=CFK,Vb=Vb(),beta=beta_t(),PICO=PICO_t(),PCO2=PCO2_t(),Hb=Hb())
    par(mar=c(4,4,1.5,1.5),mex=0.8,cex=0.8,mgp=c(2,0.5,0),tcl=0.3)
    plot(c(0,(t_e()+t_c()+t_t())/60),c(0,XCOHb.B()/percent),type='n',xlab="Time (minutes)",ylab="COHb (%)")
    polygon(c(0,e[,1]/60,t_e()/60), c(0,e[,2],0)/Hf/Hb()/percent, col='red', density=10, angle=60) 
    polygon(c(t_e()/60,c[,1]/60,(t_e()+t_c())/60), c(0,c[,2],0)/Hf/Hb()/percent, col='blue', density=15, angle=-30)
    polygon(c((t_e()+t_c())/60,o[,1]/60,(t_e()+t_c()+t_t())/60), c(0,o[,2],0)/Hf/Hb()/percent, col='green', density=10, angle=45)
    abline(h=XCOHb.B()/percent,lty=3)
    text(t_e()/60,1.02*XCOHb.B()/percent,sprintf("Maximum COHb = %.1f%%", XCOHb.B()/percent))
    text(0,0.96*XCOHb.B()/percent,sprintf("Occupational Exposure = %.1f ppm", x.CO_e.o()/ppm),adj = c(0,0))
  })
  
  output$abstract = renderPrint(cat("Employee",input$name,"(calculation ",input$ID,
                                    ") was subjected to a calculated mean carbon monoxide occupational exposure of",round(x.CO_e.o()/ppm),
                                    "ppm (SAE =",0,
                                    ") for a duration of",t_e()/minute,
                                    "minutes. The carboxyhemoglobin in the employee's blood reached a calculated peak level of",round(XCOHb.B()/percent,1),
                                    "% at the end of the exposure. The calculated 8-hour total weight average (TWA) exposure is",round(TWA8Hours()/ppm,1),
                                    "ppm CO (SAE =",0, #sprintf('%.2f',SAE8hTWA)
                                    ") which is",round(TWA8Hours()/OEL,2),
                                    "times the",OelLabel,
                                    "of",OEL/ppm,
                                    "ppm."))
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$ID, ".csv", sep = "")
    },
    content = function(file) {
      value = c(NA,NA,NA,input$h,input$w,input$XCOHb,input$Hb,NA,input$PB,input$t_e,input$AL_e,input$SS,input$x.O2_e,input$x.CO_e.s,input$t_c,input$AL_c,input$x.O2_c,input$x.CO_c,input$t_t,input$AL_t,input$x.O2_t,input$x.CO_t)
      uncertainty = c(NA,NA,NA,input$h.sd,input$w.sd,input$XCOHb.sd,input$Hb.sd,NA,input$PB.sd,input$t_e.sd,input$AL_e.sd,input$SS.sd,input$x.O2_e.sd,input$x.CO_e.s.sd,input$t_c.sd,input$AL_c.sd,input$x.O2_c.sd,input$x.CO_c.sd,input$t_t.sd,input$AL_t.sd,input$x.O2_t.sd,input$x.CO_t.sd)
      units = c(input$name,input$gender,input$ID,"inch","pound","%","grams/100mL",input$PB_method,"mmHg","minutes","","","%","ppm","minutes","","%","ppm","minutes","","%","ppm")
      df = data.frame(value, uncertainty, units)
      row.names(df) = c("name","gender","ID","height","weight","COHb in blood","hemoglobin in blood","pressure method","atmospheric pressure","exposure duration","exposure activity level","smoker status","exposure oxygen level","CO exposure from smoking","clearance duration","clearance activity level","clearance oxygen level","clearance carbon monoxide level","oxygen therapy duration","oxygen therapy activity level","oxygen therapy oxygen level","oxygen therapy carbon monoxide level")
      write.csv(df, file)
    }
  )
  
  output$CSVcontents <- renderPrint({
    req(input$CSVfile)
    tryCatch(
      {
        inFile <- input$CSVfile
        if (is.null(inFile))return(NULL)
        tmp = read.csv(inFile$datapath, header = TRUE, row.names = 1)
        updateTextInput(session, inputId = "name", value = tmp["name","units"])
        updateTextInput(session, inputId = "gender", value = tmp["gender","units"])
        updateTextInput(session, inputId = "ID", value = tmp["ID","units"])
        updateTextInput(session, inputId = "h", value = tmp["height","value"])
        updateTextInput(session, inputId = "h.sd", value = tmp["height","uncertainty"])
        updateTextInput(session, inputId = "w", value = tmp["weight","value"])
        updateTextInput(session, inputId = "w.sd", value = tmp["weight","uncertainty"])
        updateTextInput(session, inputId = "XCOHb", value = tmp["COHb in blood","value"])
        updateTextInput(session, inputId = "XCOHb.sd", value = tmp["COHb in blood","uncertainty"])
        updateTextInput(session, inputId = "Hb", value = tmp["hemoglobin in blood","value"])
        updateTextInput(session, inputId = "Hb.sd", value = tmp["hemoglobin in blood","uncertainty"])
        updateTextInput(session, inputId = "PB_method", value = tmp["pressure method","units"])
        updateTextInput(session, inputId = "PB", value = tmp["atmospheric pressure","value"])
        updateTextInput(session, inputId = "PB.sd", value = tmp["atmospheric pressure","uncertainty"])
        updateTextInput(session, inputId = "t_e", value = tmp["exposure duration","value"])
        updateTextInput(session, inputId = "t_e.sd", value = tmp["exposure duration","uncertainty"])
        updateTextInput(session, inputId = "AL_e", value = tmp["exposure activity level","value"])
        updateTextInput(session, inputId = "AL_e.sd", value = tmp["exposure activity level","uncertainty"])
        updateTextInput(session, inputId = "SS", value = tmp["smoker status","value"])
        updateTextInput(session, inputId = "SS.sd", value = tmp["smoker status","uncertainty"])
        updateTextInput(session, inputId = "x.O2_e", value = tmp["exposure oxygen level","value"])
        updateTextInput(session, inputId = "x.O2_e.sd", value = tmp["exposure oxygen level","uncertainty"])
        updateTextInput(session, inputId = "x.CO_e.s", value = tmp["CO exposure from smoking","value"])
        updateTextInput(session, inputId = "x.CO_e.s.sd", value = tmp["CO exposure from smoking","uncertainty"])
        updateTextInput(session, inputId = "t_c", value = tmp["clearance duration","value"])
        updateTextInput(session, inputId = "t_c.sd", value = tmp["clearance duration","uncertainty"])
        updateTextInput(session, inputId = "AL_c", value = tmp["clearance activity level","value"])
        updateTextInput(session, inputId = "AL_c.sd", value = tmp["clearance activity level","uncertainty"])
        updateTextInput(session, inputId = "x.O2_c", value = tmp["clearance oxygen level","value"])
        updateTextInput(session, inputId = "x.O2_c.sd", value = tmp["clearance oxygen level","uncertainty"])
        updateTextInput(session, inputId = "x.CO_c", value = tmp["clearance carbon monoxide level","value"])
        updateTextInput(session, inputId = "x.CO_c.sd", value = tmp["clearance carbon monoxide level","uncertainty"])
        updateTextInput(session, inputId = "t_t", value = tmp["oxygen therapy duration","value"])
        updateTextInput(session, inputId = "t_t.sd", value = tmp["oxygen therapy duration","uncertainty"])
        updateTextInput(session, inputId = "AL_t", value = tmp["oxygen therapy activity level","value"])
        updateTextInput(session, inputId = "AL_t.sd", value = tmp["oxygen therapy activity level","uncertainty"])
        updateTextInput(session, inputId = "x.O2_t", value = tmp["oxygen therapy oxygen level","value"])
        updateTextInput(session, inputId = "x.O2_t.sd", value = tmp["oxygen therapy oxygen level","uncertainty"])
        updateTextInput(session, inputId = "x.CO_t", value = tmp["oxygen therapy carbon monoxide level","value"])
        updateTextInput(session, inputId = "x.CO_t.sd", value = tmp["oxygen therapy carbon monoxide level","uncertainty"])
        #tmp
      },
      error = function(e) {stop(safeError(e))}
    )
    return()
  })
  
  output$PRNcontents <- renderPrint({
    req(input$PRNfile)
    tryCatch(
      {
        con = file(description=input$PRNfile$datapath, open="r")
        ID = scan(file=con, nlines=1, what="raw", sep=",", quiet=TRUE)[1]                                    #Sample Number: ID
          updateTextInput(session, inputId = "ID", value = paste(ID))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Exposure Duration (minutes): t_e
          t_e = as.numeric(temp[1])*minute
          t_e.rsd = as.numeric(temp[2])
          t_e.sd = t_e*t_e.rsd
          updateTextInput(session, inputId = "t_e", value = paste(t_e/minute))
          updateTextInput(session, inputId = "t_e.sd", value = paste(t_e.sd/minute))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Total Time for Clearance and Oxygen Therapy (minutes): t_c + t_t
          t_ct = as.numeric(temp[1])*minute
          t_ct.rsd = as.numeric(temp[2])
          t_ct.sd = t_ct*t_ct.rsd
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Exposure Duration (minutes): t_e
          XCOHb = as.numeric(temp[1])*percent
          XCOHb.rsd = as.numeric(temp[2])
          XCOHb.sd = XCOHb*XCOHb.rsd
          updateTextInput(session, inputId = "XCOHb", value = paste(XCOHb/percent))
          updateTextInput(session, inputId = "XCOHb.sd", value = paste(XCOHb.sd/percent))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Weight (pounds): w
          w = as.numeric(temp[1])*pound
          w.rsd = as.numeric(temp[2])
          w.sd = w*w.rsd
          updateTextInput(session, inputId = "w", value = paste(w/pound))
          updateTextInput(session, inputId = "w.sd", value = paste(w.sd/pound))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Height (inches): h
          h = as.numeric(temp[1])*inch
          h.rsd = as.numeric(temp[2])
          h.sd = h*h.rsd
          updateTextInput(session, inputId = "h", value = paste(h/inch))
          updateTextInput(session, inputId = "h.sd", value = paste(h.sd/inch))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Smoker Status: SS
          SS = as.numeric(temp[1])
          SS.rsd = as.numeric(temp[2])
          SS.sd = SS*SS.rsd
          updateTextInput(session, inputId = "SS", value = paste(SS))
          updateTextInput(session, inputId = "SS.sd", value = paste(SS.sd))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Activity Level during exposure: AL_e
          AL_e = as.numeric(temp[1])
          AL_e.rsd = as.numeric(temp[2])
          AL_e.sd = AL_e*AL_e.rsd
          updateTextInput(session, inputId = "AL_e", value = paste(AL_e))
          updateTextInput(session, inputId = "AL_e.sd", value = paste(AL_e.sd))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Activity Level during clearance: AL_c
          AL_c = as.numeric(temp[1])
          AL_c.rsd = as.numeric(temp[2])
          AL_c.sd = AL_c*AL_c.rsd
          updateTextInput(session, inputId = "AL_c", value = paste(AL_c))
          updateTextInput(session, inputId = "AL_c.sd", value = paste(AL_c.sd))
        AL_t = 0
        AL_t.sd = 0
          updateTextInput(session, inputId = "AL_t", value = paste(AL_t))
          updateTextInput(session, inputId = "AL_t.sd", value = paste(AL_t.sd))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Hemoglobin in blood (grams/100mL): Hb
          Hb = as.numeric(temp[1])
          Hb.rsd = as.numeric(temp[2])
          Hb.sd = Hb*Hb.rsd
          updateTextInput(session, inputId = "Hb", value = paste(Hb))
          updateTextInput(session, inputId = "Hb.sd", value = paste(Hb.sd))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Atmospheric Pressure (mmHg): PB
          PB = as.numeric(temp[1])
          PB.rsd = as.numeric(temp[2])
          PB.sd = PB*PB.rsd
          updateTextInput(session, inputId = "PB", value = paste(PB))
          updateTextInput(session, inputId = "PB.sd", value = paste(PB.sd))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #CO exposure due to smoking on the job (ppm): x.CO_e.s
          x.CO_e.s = as.numeric(temp[1])
          x.CO_e.s.rsd = as.numeric(temp[2])
          x.CO_e.s.sd = x.CO_e.s*x.CO_e.s.rsd
          updateTextInput(session, inputId = "x.CO_e.s", value = paste(x.CO_e.s))
          updateTextInput(session, inputId = "x.CO_e.s.sd", value = paste(x.CO_e.s.sd))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Carbon Monoxide Level during Clearance (ppm): x.CO_c
          x.CO_c = as.numeric(temp[1])
          x.CO_c.rsd = as.numeric(temp[2])
          x.CO_c.sd = x.CO_c*x.CO_c.rsd
          updateTextInput(session, inputId = "x.CO_c", value = paste(x.CO_c))
          updateTextInput(session, inputId = "x.CO_c.sd", value = paste(x.CO_c.sd))
        tmp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)
        t_t = as.numeric(tmp[1])*minute                                                                      #Oxygen therapy duration (minutes): t_t
          updateTextInput(session, inputId = "t_t", value = paste(t_t/minute))
          updateTextInput(session, inputId = "t_t.sd", value = 0)                                            #This is assumed to be zero and all uncertainty goes to clearance
        x.O2_e = as.numeric(tmp[2])                                                                          #Oxygen level (% oxygen) during exposure: x.O2_e
          updateTextInput(session, inputId = "x.O2_e", value = paste(x.O2_e/percent))
          updateTextInput(session, inputId = "x.O2_e.sd", value = 0)
        x.O2_c = x.O2_e                                                                                      #Oxygen level (% oxygen) during clearance: x.O2_c (assumed equal to exposure)
          updateTextInput(session, inputId = "x.O2_c", value = paste(x.O2_c/percent))
          updateTextInput(session, inputId = "x.O2_c.sd", value = 0)
          updateTextInput(session, inputId = "x.CO_t", value = 0)
          updateTextInput(session, inputId = "x.CO_t.sd", value = 0)
        t_c = t_ct - t_t                                                                                     #Clearance time (minutes): t_c (calculated)
          t_c.sd = t_ct.sd                                                                                   # We will assume that all of the uncertainty in t_ct can be attributed to t_c
          updateTextInput(session, inputId = "t_c", value = paste(t_c/minute))
          updateTextInput(session, inputId = "t_c.sd", value = paste(t_c.sd/minute))
        x.O2_t = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])                #Oxygen level (% oxygen) during therapy: x.O2_t
          updateTextInput(session, inputId = "x.O2_t", value = paste(x.O2_t/percent))
          updateTextInput(session, inputId = "x.O2_t.sd", value = 0)
          updateTextInput(session, inputId = "name", value = paste(""))
          updateTextInput(session, inputId = "gender", value = paste("male"))
          updateSelectInput(session, inputId = "PB_method", selected = "pressure")
        close(con)
      },
      error = function(e) {stop(safeError(e))}
    )
    return()
  })
}