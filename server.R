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
  

  #h.rsd = reactive(input$h.sd/input$h)
  #output$h.rsd = renderText(h.rsd())
  #w.rsd = reactive(input$w.sd/input$w)
  #output$w.rsd = renderText(w.rsd())
  Vb = reactive({
    w = input$w*pound
    h = input$h*inch
    Vb.m(W=w,H=h)
  })
  output$Vb = renderPrint(cat(Vb()/liter,"liter"))
  
  
  #XCOHb.rsd = reactive(input$XCOHb.sd/input$XCOHb)
  #output$XCOHb.rsd = renderText(XCOHb.rsd())
  #Hb.rsd = function() 0.073
  #output$Hb.rsd = renderText(Hb.rsd())
  
  output$COHb.D = renderPrint(cat(COHb.D()))
  
  PB.rsd = reactive(input$PB.sd/input$PB)
  output$PB.rsd = renderText(PB.rsd())
  z.rsd = reactive(input$z.sd/input$z)
  output$z.rsd = renderText(z.rsd())
  T.rsd = reactive(input$T.sd/input$T)
  output$T.rsd = renderText(T.rsd())
  t_e.rsd = reactive(input$t_e.sd/input$t_e)
  output$t_e.rsd = renderText(t_e.rsd())
  
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
      value = c(input$ID, input$h, input$w)
      uncertainty = c(NA, 1, 5)
      units = c("","inch","pound")
      df = data.frame(value, uncertainty, units)
      row.names(df) = c("ID","height","weight")
      write.csv(df, file, row.names = TRUE)
    }
  )
  
  output$CSVcontents <- renderTable({
    req(input$CSVfile)
    tryCatch(
      {df <- read.csv(input$CSVfile$datapath,header = TRUE,sep = ",",row.names=1)},
      error = function(e) {stop(safeError(e))}
    )
    updateTextInput(session, inputId = "name", value = "Phillip Toone")
    updateTextInput(session, inputId = "ID", value = paste(df["ID","value"]))
    return(df)
  })

  output$PRNcontents <- renderPrint({
    req(input$PRNfile)
    tryCatch(
      {
        con = file(description=input$PRNfile$datapath, open="r")
        ID = scan(file=con, nlines=1, what="raw", sep=",", quiet=TRUE)[1]
        updateTextInput(session, inputId = "ID", value = paste(ID))
        t_e = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])*minute
        updateTextInput(session, inputId = "t_e", value = paste(t_e/minute))
        t_ct = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])*minute
        XCOHb = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])*percent
        updateTextInput(session, inputId = "XCOHb", value = paste(XCOHb/percent))
        w = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])*pound
        updateTextInput(session, inputId = "w", value = paste(w/pound))
        h = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])*inch
        updateTextInput(session, inputId = "h", value = paste(h/inch))
        SS = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])
        updateTextInput(session, inputId = "SS", value = paste(SS))
        AL_e = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])
        updateTextInput(session, inputId = "AL_e", value = paste(AL_e))
        AL_c = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])
        updateTextInput(session, inputId = "AL_c", value = paste(AL_c))
        AL_t = 0
        updateTextInput(session, inputId = "AL_t", value = paste(AL_t))
        Hb = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])*gram/(100*mL)
        updateTextInput(session, inputId = "Hb", value = paste(Hb/(gram/(100*mL))))
        PB = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])*mmHg
        updateTextInput(session, inputId = "PB", value = paste(PB/mmHg))
        x.CO_e.s = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])*ppm
        updateTextInput(session, inputId = "x.CO_e.s", value = paste(x.CO_e.s/ppm))
        x.CO_c = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])*ppm
        updateTextInput(session, inputId = "x.CO_c", value = paste(x.CO_c/ppm))
        tmp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)
        t_t = as.numeric(tmp[1])*minute
        updateTextInput(session, inputId = "t_t", value = paste(t_t/minute))
        x.O2_e = as.numeric(tmp[2])
        updateTextInput(session, inputId = "x.O2_e", value = paste(x.O2_e/percent))
        x.O2_c = x.O2_e
        updateTextInput(session, inputId = "x.O2_c", value = paste(x.O2_c/percent))
        updateTextInput(session, inputId = "x.CO_t", value = paste(0))
        t_c = t_ct - t_t
        updateTextInput(session, inputId = "t_c", value = paste(t_c/minute))
        x.O2_t = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])
        updateTextInput(session, inputId = "x.O2_t", value = paste(x.O2_t/percent))
        updateTextInput(session, inputId = "name", value = paste(""))
        updateTextInput(session, inputId = "gender", value = paste("male"))
        updateSelectInput(session, inputId = "PB_method", selected = "pressure")
        close(con)
        #df <- read.csv(input$PRNfile$datapath,header = FALSE,sep = ",",row.names = NULL)
        },
      error = function(e) {stop(safeError(e))}
    )
    #updateTextInput(session, inputId = "ID", value = paste(df[1,1]))
    #updateTextInput(session, inputId = "w", value = paste(df[1,2]))
    return()
  })
}