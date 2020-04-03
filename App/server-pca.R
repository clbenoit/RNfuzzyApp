# server-pca.R

runPCA <- reactiveValues(runPCAValue = FALSE)


output$CondPCAParams <- renderUI({
  if (AnalysisRun$AnalysisRunValue){
    uiOutput("PCAParams")
  }else{
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "You must perform a DEA before.",
      type = "info"
    )
    helpText("Please perform a DEA first.")
  }
  
  
})

output$PCAParams <- renderUI({
  tagList(
    numericInput(
      inputId = "pcFDR",
      label = "FDR Cut-off",
      min = 0.00001,
      value = 0.001,
      max = 0.01,
      step = 0.001
    ),
    
    do.call(actionBttn, c(
      list(
        inputId = "pcRun",
        label = "Run PCA",
        icon = icon("play")
      )
    ))
  )
})


observeEvent(input$pcRun, {
  runPCA$runPCAValue <- input$pcRun
  data <- var$norData
  data <- data[var$result$q.value <= input$pcFDR,] 
  data <- t(log1p(data)) 
  data.pca <- prcomp(data[, apply(data, 2, var) != 0],
                     center = T,
                     scale. = T)
  var$pcadata <- data.pca
})


# 2D plotly object
output$D2pca <- renderPlotly({
  if (length(var$pcadata) > 0) {
    tcc <- var$tccObject
    data.pca <- var$pcadata
    data <- data.frame(data.pca$x)
    data$name <- rownames(data)
    group <- tcc$group
    group$name <- rownames(group)
    data <- left_join(x = data, y = group, by = "name")
    p <- plot_ly(
      data = data,
      x = ~ PC1,
      y = ~ PC2,
      color = ~ factor(group),
      text = ~ name,
      textposition = "top right",
      type = "scatter",
      mode = "markers+text"
    ) %>%
      layout(title = "PCA Plot (2D)")
    p
  } else {
    return()
  }
})

#  3D plotly object 
output$D3pca <- renderPlotly({
  if (length(var$pcadata) > 0) {
    tcc <- var$tccObject
    data.pca <- var$pcadata
    data <- data.frame(data.pca$x)
    data$name <- rownames(data)
    group <- tcc$group
    group$name <- rownames(group)
    data <- left_join(x = data, y = group, by = "name")
    p <- plot_ly(
      data = data,
      x = ~ PC1,
      y = ~ PC2,
      z = ~ PC3,
      color = ~ factor(group),
      text = ~ name,
      textposition = "top right",
      type = "scatter3d",
      mode = "markers+text"
    ) %>%
      layout(title = "PCA Plot (3D)")
    p
  } else {
    return()
  }
})


# Render 2D Plot UI 
output$D2PlotUI <- renderUI({
  if (runPCA$runPCAValue) {
    plotlyOutput("D2pca") %>% withSpinner()
  } else {
    helpText("Click [Run PCA] to compute first.")
  }
})

# Render 3D Plot UI 
output$D3PlotUI <- renderUI({
  if (runPCA$runPCAValue) {
    plotlyOutput("D3pca") %>% withSpinner()
  } else {
    helpText("Click [Run PCA] to compute first.")
  }
})

