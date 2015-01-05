#survey.example(TRUE)

#' An example of a simple teaching app with some given default questions in German
simpleTeachingApp = function(run.app=FALSE) {
  library(wat)
  library(shinyEvents)
  library(shinyAce)
  library(hwriter)
  set.restore.point.options(display.restore.point = TRUE)
  session = NULL
  

  questions = list(
    choice.question("","Ihre Antwort:",c("ja","nein"),name="ja-nein"),  
    choice.question("","Ihre Antwort:",c("groesser","gleich","kleiner"),name="grosser-gleich-kleiner"),
    choice.question("","Ihre Antwort:",c("positiv","null", "negativ"),name="pos-null-neg"),
    choice.question("","Ihre Antwort:",c("a)","b)", "c)","d)"),name="abcd"),
    number.question("","Bitte antworten Sie mit einer Zahl:",name="Zahl"),    
    number.question("Which is the oddest prime?",name="oddest prime")
  )

  wat = new.wat(questions)
  app = liveWatApp(wat)
  if (!run.app)
    return(app)
  runEventsApp(app)
}

choice.question = function(question,answer.lab="", answers=c("yes","no"), var="answer",name=NULL) {
  ui = fluidRow(
    h3(question),
    xRadioButtons(inputId=var, label=answer.lab,choices=answers)  
  )
  choices = list(answers)
  names(choices) = var
  list(name=name,ui=ui, vars=c(var), choices = choices)
}

number.question = function(question,answer.lab="", value=NULL, min=NA, max=NA, step=NA,var="answer",name=NULL) {
  ui = fluidRow(
    h3(question),
    numericInput(var, answer.lab,value,min,max,step)  
  )
  list(name=name,ui=ui, vars=c(var), choices=NULL)
}

#' Create a new App for live assisted teaching
#' 
#' @param wat a wat object
#' @param refresh.time time interval in which clients will be refreshed in milli-seconds
liveWatApp = function(wat, refresh.time = 2000L) {
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

  timerHandler(session,"refreshStudentUI",refresh.time, function(session,app,...) {
    restore.point("refreshStudentUI")
    wat = app$wat; loc=app$loc
    if (is.null(loc$role) | is.null(loc$ex.counter)) return()
    
    if (loc$role=="student") {
      cat("\nStudent timer...")
      restore.point("refreshStudentUI.student")
      if(wat$ex.counter != loc$ex.counter) {
        loc$ex.counter = wat$ex.counter
        cat("loc$ex.counter: ",loc$ex.counter)
        ui = make.student.ui(session, app)
        updateUI(session,"baseUI", ui)
        
        #ui = actionButton("studentSubmitBtn","Submit...")
        #updateUI(session,"studentSubmitUI", ui)

      }
    } else if (loc$role=="teacher") {
      if (wat$ex.counter==0) return()
      cat("\nTeacher timer...")
      restore.point("refreshStudentUI.teacher")
      if (wat$ans.counter == loc$ans.counter) return()
      
      loc$ans.counter = wat$ans.counter
      updateExerciseMonitor(session,id="teacherMonitor", wat=wat, update.ex=FALSE)
    }
  })
  
  buttonHandler(session,"studentSubmitBtn", function(session, app,...) {
     submit.student.answers(session, app,...)
     #ui = make.student.post.submit.ui(session,app)
     #updateUI(session,"studentSubmitUI", ui)
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

uiStudentEx = function(ex, ex.counter) {
  restore.point("make.full.ui")
  full.ui = fluidRow(
    ex$ui,
    actionButton("studentSubmitBtn","Submit..."),
    bsAlert("studentSubmitAlert"),
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

updateExerciseMonitor = function(session, id="", wat=app$wat, loc=app$loc, app=getApp(session),..., header=NULL, update.ex = TRUE) {
  restore.point("updateExerciseMonitor")

  ec = wat$ex.counter
  ex = wat$ex
  
  exId = paste0(id,"Exercise")
  ansId = paste0(id,"Answers")
  barId = paste0(id,"Barplot")
  
  # Update exercise panel
  if (ec==0) {
    ui =  h3("No exercise shown yet...")
    if (update.ex)
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
  if (update.ex) {
    ui = fluidRow(
      header,
      wat$ex$ui
    )
    updateUI(session, exId, ui)
  }  
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
    updateUI(session,barId, ui)
  } else {
    barId = paste0(id,"Barplot")
    vars = wat$ex$vars
    nv = length(vars)
    if (nv >= 1) {
      restore.point("make.multi.bar.plot")
      # Multiplot ui
      mpId = paste0(barId,"MultiPlot")
      mp = uiMultiPlot(mpId,nv)
      updateUI(session,barId, mp)
      for (i in 1:nv) {
        var = vars[i]
        updateMultiPlot(session,mp,i,answers.barplot(df,vars[i], ex=ex))                  
      }
    }
  }

}

answers.barplot = function(df, var, ex=NULL) {
  restore.point("answers.barplot")
  x = df[[var]]
  tab = table(x)
  if (!is.null(ex$choices[[var]])) {
    choices = ex$choices[[var]]
    atab = rep(0, length(choices))
    names(atab) = choices
    atab[names(tab)] = tab
    tab = atab
  }
  
  if (length(tab)<=9) {
    color = brewer.pal(max(3,length(tab)),"Set3")
  } else {
    color = rainbow(length(tab))
  }
  
  barX = barplot(height = tab,main=var,col =color, legend.text = )
  text(cex=2, x=barX, y=pmax(0.5,tab/2), tab, xpd=TRUE)
}


make.teacher.ui = function(session, app=getApp(session),...) {
  restore.point("make.teacher.ui")
  wat = app$wat; loc=app$loc
  choices = seq_along(wat$ex.li)
  names(choices) = names(wat$ex.li)
  ui = fluidRow(
    selectInput("teacherExInd","Question / exercise:",choices),
    actionButton("teacherShowBtn","Show to students..."),
    h4("Exercise Monitor"),
    uiExerciseMonitor("teacherMonitor")
  )
  
  buttonHandler(session,"teacherShowBtn", function(session, app,...) {
    wat = app$wat; loc=app$loc
    restore.point("teacherShowButton")
    wat$ex.ind = as.numeric(isolate(session$input$teacherExInd))
    wat$ex.counter = wat$ex.counter+1
    loc$ex.counter = wat$ex.counter
    wat$ans.counter = loc$ans.counter = 0
    
    wat$time.posted = Sys.time()
    ex =  wat$ex.li[[wat$ex.ind]]
    ex$full.ui = uiStudentEx(ex, wat$ex.counter)
    wat$ex = ex
    updateExerciseMonitor(session,id="teacherMonitor", wat=wat, update.ex=TRUE)
  })
  ui
}

submit.student.answers = function(session, app=getApp(session),...) {
  restore.point("submit.student.answers")
  wat = app$wat; loc=app$loc

  cat("loc$ex.counter: ",loc$ex.counter)


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
  if (ec <= length(wat$ans.li)) {
    restore.point("before error")
    df = wat$ans.li[[ec]]
  }
  if (is.null(df)) {
    df = loc.df
  } else {
    df = rbind(df, loc.df)
  }
  try(wat$ans.li[[ec]] <- df)
  wat$ans.counter = NROW(df)
  
  title = paste0("Answer submitted at ", format(Sys.time(),"%H:%M:%S"))
  createAlert(session, "studentSubmitAlert", title=title, message="", append=TRUE,type="info")
  #make.student.post.submit.ui
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
  #h3("You submitted your answer!")  
  NULL
}

make.login.ui = function(session, app=getApp(session),...) {
  restore.point("make.login.ui")

  user.name = paste0("guest", sample.int(10000,1))
  login.ui = fluidRow(
    h3("Welcome to Live-WAT"),
    textInput("loginUserName", "Username:", user.name ),
    helpText("Choose your role:"),
    actionButton("teacherLoginBtn","Teacher"),
    actionButton("studentLoginBtn","Student")
  )

  buttonHandler(session, "teacherLoginBtn", loginHandler)
  buttonHandler(session, "studentLoginBtn", loginHandler)
  login.ui
}

loginHandler = function(session, id, app=getApp(session),...) {
   wat = app$wat; loc=app$loc
   role = str.left.of(id, "LoginBtn")
   user.name = isolate(session$input$loginUserName)
   restore.point("loginHandler")
   app$loc$role = role
   app$loc$user.name = user.name
   if (role=="teacher") {
     loc$ex.counter = wat$ex.counter
     loc$ans.counter = wat$ans.counter
     updateUI(session,"baseUI",make.teacher.ui(session=session))
   } else {
     loc$ex.counter = wat$ex.counter
     loc$ans.counter = wat$ans.counter
     updateUI(session,"baseUI",make.student.ui(session=session))      
   }
}
