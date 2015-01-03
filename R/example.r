new.wat = function(ex.li,...) {
  wat = new.env()
  if (is.null(names(ex.li)))
    names(ex.li) = paste0("ex", seq_along(ex.li))
  wat$ex.li = ex.li
  
  wat$ex.ind = 1
  wat$ex = ex.li[[wat$ex.ind]]
  wat
}


survey.example = function() {
  library(shinyEvents)
  library(shinyAce)

  ui1 = fluidRow(
    h3("Is 2 the oddest prime?"),
    radioInput("ans", "Your answer:", c("yes","no"="no"))  
  )
  ex1 = list(ui=ui1, var=c("ans"))

  ui2 = fluidRow(
    h3("How old is Ulm?")
    numericInput("age", "Your answer in years:")  
  )
  ex2 = list(ui=ui2, var=c("age"))

  wat = new.wat(ex.li=list(ex1,ex2))
  
  app = eventsApp()
  app$glob$wat.li = list(wat)
  
  app$initHandler = function(session,...) {
    wat = app$glob$wat.li[[1]]
    
    loc = new.env()
    loc$ex.ind = wat$ex.ind
    app$loc = loc
    app$wat = wat
  }
  
  main.ui = fluidPage(
    uiOutput("baseUI")
  )
  
  login.ui = fluidRow(
    selectInput("role","Your role:",c("teacher","student")),
    actionButton("loginBtn","Start...")
  )  
  buttonHandler(session,id="loginBtn", function(session,app,...) {
    role = isolate(session$input$role)
    app$loc$role = role
    if (role=="teacher") {
      updateUI(session,"baseUI",teacher.ui)
    } else {
      updateUI(session,"baseUI",student.ui)      
    }
  })
  
  make.teacher.ui = function(session, app,...) {
    wat = app$wat; loc=app$loc
    fluidRow(
      selectInput("ex","Question / exercise:",names(wat$ex)),
      actionButton("showBtn","Show to students...")
    )
  )

  make.student.ui = function(session, app,...) {
    wat = app$wat; loc=app$loc
    wat$ex$ui
  )


  timerHandler(session,"refreshWat",2000, function(session,app,...) {
    wat = app$wat; loc=app$loc
    wat$ex$ui    
  })
  
  app$handlers[["refreshChatWindow"]]
  runEventsApp(app,ui=ui)
}
