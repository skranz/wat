#survey.example()
survey.example = function() {
  library(shinyEvents)
  library(shinyAce)
  library(hwriter)
  set.restore.point.options(display.restore.point = TRUE)
  session = NULL
  

  ex1 = ja.nein.frage()
  ggk = choice.question("","Ihre Antwort:",c("größer","gleich","kleiner"),name="größer-gleich-kleiner")  
  ex2 = number.question("Which is the oddest prime?",name="oddest prime")

  wat = new.wat(list(ex1,ggk,ex2))
  app = liveWatApp(wat)
  runEventsApp(app)
}

ja.nein.frage = function(question="",answer.lab="Ihre Antwort", answers=c("ja","nein"), var="ans",name="ja-nein") {
  choice.question(question, answer.lab, answers, var, name)
}

choice.question = function(question,answer.lab="", answers=c("yes","no"), var="ans",name=NULL) {
  ui = fluidRow(
    h3(question),
    radioButtons(var, answer.lab,answers)  
  )
  list(name=name,ui=ui, vars=c(var))
}

number.question = function(question,answer.lab="", value=NULL, min=NA, max=NA, step=NA,var="ans",name=NULL) {
  ui = fluidRow(
    h3(question),
    numericInput(var, answer.lab,value,min,max,step)  
  )
  list(name=name,ui=ui, vars=c(var))
}

liveWatApp = function(wat) {
  restore.point("liveWatApp")
  session=NULL
  app = eventsApp()
  app$glob$wat.li = list(wat)
  
  app$initHandler = function(session,app,...) {
    wat = app$glob$wat.li[[1]]
    loc = new.env()
    loc$ex.counter = 0
    app$loc = loc
    app$wat = wat
  }
  
  main.ui = fluidPage(
    uiOutput("baseUI")
  )
  setAppUI(main.ui, app)

  login.ui = make.login.ui(session)
  updateUI(session, "baseUI", login.ui)

  timerHandler(session,"refreshStudentUI",2000, function(session,app,...) {
    restore.point("refreshStudentUI")
    wat = app$wat; loc=app$loc
    if (is.null(loc$role) | is.null(loc$ex.counter)) return()
    
    if (loc$role=="student") {
      cat("\nStudent timer...")
      restore.point("refreshStudentUI.student")
      if(wat$ex.counter != loc$ex.counter) {
        loc$ex.counter = wat$ex.counter  
        ui = make.student.ui(session, app)
        updateUI(session,"baseUI", ui)
        
        #ui = actionButton("studentSubmitBtn","Submit...")
        #updateUI(session,"studentSubmitUI", ui)

      }
    } else if (loc$role=="teacher") {
      if (wat$ex.counter==0) return()
      cat("\nTeacher timer...")
      restore.point("refreshStudentUI.teacher")
      updateExerciseMonitor(session,id="teacherMonitor", wat=wat)
    }
  })
  
  buttonHandler(session,"studentSubmitBtn", function(session, app,...) {
     submit.student.answers(session, app,...)
     ui = make.student.post.submit.ui(session,app)
     updateUI(session,"studentSubmitUI", ui)
  })
  
  app
}

new.wat = function(ex.li,...) {
  restore.point("new.wat")
  wat = new.env()
  wat.names = sapply(seq_along(ex.li), function(i) {
    ex = ex.li[[i]]
    if (!is.null(ex$name)) return(ex$name)
    if (!is.null(names(ex.li))) return(names(ex.li)[i])
    paste0("ex",i)
  })
  names(ex.li)=wat.names  
  wat$ex.li = ex.li
  wat$ex.ind = 1
  wat$ex = ex.li[[wat$ex.ind]]
  wat$ex.counter = 0 
  wat$ans.li = list()
  wat
}

wat.full.ui = function(ex, ex.counter) {
  restore.point("make.full.ui")
  full.ui = fluidRow(
    ex$ui,
    actionButton("studentSubmitBtn","Submit..."),
    uiOutput("studentSubmitUI"),
    br(),
    p(paste0("Posted as question no. ", ex.counter, " at ", format(Sys.time(),"%H:%M:%S"), ""))
  )
  full.ui
}

uiExerciseMonitor = function(id, ...) {
  restore.point("uiExerciseMonitor")
  addId = function(str) paste0(id,str)
  ui = tabsetPanel(addId("Tabset"),
    tabPanel("Exercise",            
      uiOutput(addId("Exercise"))
    ),
    tabPanel("Answers",           
      uiOutput(addId("Answers"))      
    ),
    tabPanel("Barplot",            
      uiOutput(addId("Barplot"))            
    )
  )
  ui
}

updateExerciseMonitor = function(session, id="", wat=app$wat, loc=app$loc, app=getApp(session),..., header=NULL) {
  restore.point("updateExerciseMonitor")

  ec = wat$ex.counter
  ex = wat$ex
  
  exId = paste0(id,"Exercise")
  ansId = paste0(id,"Answers")
  barId = paste0(id,"Barplot")
  
  # Update exercise panel
  if (ec==0) {
    ui =  h3("No exercise shown yet...")
    updateUI(session, exId, ui)
    updateUI(session, ansId, ui)
    updateUI(session, barId, ui)    
    return()
  }
  df = NULL
  if (ec <= length(wat$ans.li))
    df = wat$ans.li[[ec]]
  
  num.ans = NROW(df)
  
  # Exercise Panel
  ui = fluidRow(
    header,
    wat$ex$ui,
    h4(paste0(num.ans, " answers so far..."))
  )
  updateUI(session, exId, ui)
  
  # Answers Panel
  if (num.ans==0) {
    ui = p("No answers yet...")
  } else {
    df = df[NROW(df):1,]
    out = hwrite(df, NULL, border=2, row.names=FALSE, cellpadding=5)
    ui = fluidRow(
      p(paste0(NROW(df),"  answers so far...")),
      HTML(out)
    )
  }
  updateUI(session, ansId, ui)

  # Barplot panel
  if (num.ans==0) {
    ui = p("No answers yet...")
  } else {
    ui = p("Here should be barplots...")
  }
  updateUI(session,barId, ui)
}

answers.barplot = function(df, vars) {
  for (var in vars) {
    x = df[[var]]
    tab = table(x)
    barplot(height = tab,main=var)
    hist(x)  
  }
}


make.teacher.ui = function(session, app=getApp(session),...) {
  restore.point("make.teacher.ui")
  wat = app$wat; loc=app$loc
  choices = seq_along(wat$ex.li)
  names(choices) = names(wat$ex.li)
  ui = fluidRow(
    selectInput("teacherExInd","Question / exercise:",choices),
    actionButton("teacherShowBtn","Show to students..."),
    uiExerciseMonitor("teacherMonitor")
  )
  
  buttonHandler(session,"teacherShowBtn", function(session, app,...) {
    wat = app$wat; loc=app$loc
    restore.point("teacherShowButton")
    wat$ex.ind = as.numeric(isolate(session$input$teacherExInd))
    wat$ex.counter = wat$ex.counter+1
    loc$ex.counter = wat$ex.counter
    wat$time.posted = Sys.time()
    ex =  wat$ex.li[[wat$ex.ind]]
    ex$full.ui = wat.full.ui(ex, wat$ex.counter)
    wat$ex = ex
  })
  ui
}

submit.student.answers = function(session, app=getApp(session),...) {
  restore.point("submit.student.answers")
  wat = app$wat; loc=app$loc

  vars = wat$ex$vars
  ans = lapply(vars, function(var) {
    session$input[[var]]
  })
  restore.point("submit.student.answers.2")

  names(ans)=vars
  
  # Add answers to data base
  li = c(list(user.name=loc$user.name),ans, 
         list(datetime.question = wat$time.posted,datetime.answer=Sys.time()))

  loc.df = data.frame(li)

  ec = loc$ex.counter
  df = NULL
  if (ec <= length(wat$ans.li))
    df = wat$ans.li[[ec]]
  
  if (is.null(df)) {
    df = loc.df
  } else {
    df = rbind(df, loc.df)
  }
  wat$ans.li[[ec]] = df

}

make.student.ui = function(session, app=getApp(session),...) {
  restore.point("make.student.ui")
  wat = app$wat; loc=app$loc
  if (wat$ex.counter >0 ) {
    ui = wat$ex$full.ui
    return(ui)
  }
  NULL
}

make.student.post.submit.ui = function(session, app=getApp(session),...) {
  restore.point("make.student.post.submit.ui")
  wat = app$wat; loc=app$loc
  h3("You submitted your answer!")  
  
}

make.login.ui = function(session, app=getApp(session),...) {
  restore.point("make.login.ui")

  user.name = paste0("guest", sample.int(10000,1))
  login.ui = fluidRow(
    h3("Welcome to Live-WAT"),
    textInput("loginUserName", "Username:", user.name ),
    selectInput("loginRole","Choose your role:",c("teacher","student")),
    actionButton("loginBtn","Start...")
  )

  buttonHandler(session,id="loginBtn", function(session,app,...) {
     role = isolate(session$input$loginRole)
     user.name = isolate(session$input$loginUserName)
     restore.point("loginBtnHandler")
     app$loc$role = role
     app$loc$user.name = user.name
     if (role=="teacher") {
       updateUI(session,"baseUI",make.teacher.ui(session=session))
     } else {
       updateUI(session,"baseUI",make.student.ui(session=session))      
     }
  })
  login.ui  
}
