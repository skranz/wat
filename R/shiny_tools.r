
xRadioButtons = function(inputId, label, choices, selected=NULL,toggle="radio", type=c("buttons","radio")[2],...) {
  dots = list(...)
  restore.point("radioButtonGroup")
  if (type=="radio") {
    return(radioButtons(inputId, label, choices, selected,...))
  }
  btnLab = names(choices)
  if (is.null(btnLab)) btnLab = choices
  btnId = paste0(inputId,"___",choices)
  
  btns = lapply(seq_along(choices), function(i) {
    bsButton(btnId[i], label=btnLab[i], value = choices[i])
  })
  res = do.call(bsButtonGroup, c(list(inputId=inputId, label=label, toggle=toggle, value=selected),btns, dots ))
  res
}

uiMultiPlot = function(id, num.plots=1,height="200px",...) {
  plotsId = paste0(id, "___",1:num.plots)
  pout = lapply(plotsId, plotOutput, height=height,...)
  res = do.call(fluidRow, pout)
  attr(res, "plotsId") <- plotsId
  res
}

updateMultiPlot = function(session,mp, num, expr,   plotId = plotsId[num], plotsId = attr(mp,"plotsId"),...) {
  updatePlot(session, plotId, expr,...)
}