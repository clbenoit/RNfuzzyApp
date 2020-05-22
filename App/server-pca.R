# server-pca.R

runPCA <- reactiveValues(runPCAValue = FALSE) # to precise the run button has not been clicked yet


output$CondPCAParams <- renderUI({              # if a DEA has not been performed error message to perform it
  if (AnalysisRun$AnalysisRunValue){            # if DEA done, it shows the parameters
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

output$PCAParams <- renderUI({                # set of paramters 
  tagList(
    numericInput(
      inputId = "pcFDR",
      label = "FDR Cut-off",
      min = 0.00001,
      value = 0.001,
      max = 0.01,
      step = 0.001
    ),
    
    do.call(actionBttn, c(                 # run button 
      list(
        inputId = "pcRun",
        label = "Run PCA",
        icon = icon("play")
      )
    ))
  )
})


observeEvent(input$pcRun, {              # when the run button is clicked
  runPCA$runPCAValue <- input$pcRun      # precise the button has been clicked
  data <- var$norData                    # use normalized data
  data <- data[var$result$q.value <= input$pcFDR,]  # selection of genes with respect of the selected fdr cut-off
  data <- t(log1p(data))                 # transform data
  data.pca <- prcomp(data[, apply(data, 2, var) != 0], #pca
                     center = T,
                     scale. = T)
  var$pcadata <- data.pca               # save the pca to reuse the data
})


# 2D plotly object
output$D2pca <- renderPlotly({          
  if (length(var$pcadata) > 0) {
    tcc <- var$tccObject
    data.pca <- var$pcadata
    data <- data.frame(data.pca$x)      # conversion to a data frame 
    data$name <- rownames(data)
    group <- tcc$group
    group$name <- rownames(group)
    data <- left_join(x = data, y = group, by = "name") # to perform a pca over the groups 
    p <- plot_ly(
      data = data,
      x = ~ PC1,
      y = ~ PC2,
      color = ~ factor(group),          # colors according to groups
      text = ~ name,                    # hovering a point on the plot gives the name of the group
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
output$D3pca <- renderPlotly({       # same in 3D 
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
  if (runPCA$runPCAValue) {                    # if the pca run button has been clicked 
    plotlyOutput("D2pca") %>% withSpinner()    # it shows the 2D pca
  } else {                                     # if not, it just precise to run it 
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

