#server-mfuzz.R


observeEvent(input$mfuzzCountData, {   # when a table is being uploaded 
  tryCatch({
    var$mfuzzTable <-    # assign the table as a data frame to this variable 
      data.frame(fread(input$mfuzzCountData$datapath), row.names = 1)
    v$importActionValue = FALSE # precise no button has been clicked yet
  },
  error = function(e) {  # error messages about the input table 
    sendSweetAlert(
      session = session,
      title = "Input data error!",
      text = as.character(message(e)),
      type = "error"
    )
    return()
  },
  warning = function(w) {
    sendSweetAlert(
      session = session,
      title = "Input data warning!",
      text = "Error in dataset",
      type = "warning"
    )
    return()
  })
  
  var$mfuzzTable <- var$mfuzzTable[rowSums(var$mfuzzTable >= 1) > 0 , ]
  var$timepoints <- as.vector(colnames(var$mfuzzTable))
  print(var$timepoints)
  
  output$inertia_plot <-renderPlotly({
    hierdata <- as.matrix((var$mfuzzTable))
    # Euclidean distance
    dist <- dist(hierdata, diag=TRUE)
    # Hierarchical Clustering with hclust
    hc <- hclust(dist)
    inertia <- sort(hc$height, decreasing = TRUE)
    max <- as.numeric(input$maxclass)
    inertia <- inertia[1:max]
    az <- c(1:max)
    df <- as.data.frame(inertia, row.names = c(1:max))
    df["class"] <- az
    fig <- plot_ly(
      df,
      x = ~az,
      y = ~inertia,
      type = "scatter",
      mode = "markers"
      
    )%>% add_lines(y = df$az, line = list(shape = "vh"))
    fig <- fig %>% layout(
      title = "Inertia drops",
      xaxis = list(title = "Clusters"),
      yaxis = list(title = "Inertia"),
      showlegend = F
    )
  })
  



observeEvent(input$inertiaclass,{   # when a filter of low count genes is set 
  if (input$inertiaclass != 0) {
    mfmat <- as.matrix((var$mfuzzTable))
    mfmat <- DGEList(counts = mfmat, group=colnames(var$mfuzzTable))
    mfmat <- calcNormFactors(mfmat)
    mfcpm <- cpm(mfmat, normalized.lib.size=TRUE)
    
    
    timepoint <- colnames(var$mfuzzTable)
    test_data <- rbind(timepoint, mfcpm)
    row.names(test_data)[1]<-"time"
    
    #save it to a temp file so ti doesn't clutter up the blog directory
    tmp <- tempfile()
    write.table(test_data,file=tmp, sep='\t', quote = F, col.names=NA)
    #read it back in as an expression set
    
    mfdata <- table2eset(file=tmp)
    mfdata.s <- standardise(mfdata)
    m1 <- mestimate(mfdata.s)
    cent <- input$inertiaclass
    N_cl<- mfuzz(mfdata.s, centers=cent, m = m1)
    ov <- overlap(N_cl)
    
  }
  
  output$elbow_plot <- renderPlot({
    data.s <- as.matrix(mfdata.s)
    scaledata <- t(scale(t(data.s))) # Centers and scales data.
    scaledata <- scaledata[complete.cases(scaledata),]
    
    
    #helper function for the within sum of squared error
    sumsqr <- function(x, clusters){
      sumsqr <- function(x) sum(scale(x, scale = FALSE)^2)
      wss <- sapply(split(as.data.frame(x), clusters), sumsqr)
      return(wss)
    }
    
    #get the wss for repeated clustering
    iterate_fcm_WSS <- function(df,m){
      totss <- numeric()
      for (i in 2:20){
        FCMresults <- cmeans(df,centers=i,m=m)
        totss[i] <- sum(sumsqr(df,FCMresults$cluster))
      }
      return(totss)
    }
    wss_2to20 <- iterate_fcm_WSS(scaledata,m1)
    max <- as.numeric(input$maxclass)
    elb <- plot(1:max, wss_2to20[1:max], type="b", xlab="Number of Clusters", ylab="WSS")
    elb
  })
  
  output$overlap_plot <- renderPlot({
    Ptmp<- overlap.plot(N_cl,over=ov, thres=as.numeric(input$ov_threshold))
    Ptmp
  })
  

  

  output$mfuzz_plots <- renderPlot({
    
    if(input$inertiaclass < 10){
      mfrow <- c(3,4)
    }else{
      mfrow <- c(5,4)
    }
    fuzz <- mfuzz.plot(mfdata.s,cl=N_cl,mfrow = mfrow, time.labels = var$timepoints,new.window = F)
    fuzz
    clusters_list<- acore(mfdata.s,N_cl, min.acore = 0)
    save(list = "clusters_list", file = "~/Desktop/clusters_list.rda")
  })
  
  
  })




})




output$inertia_elbowUI <- renderUI({
  if (nrow(var$mfuzzTable) != 0){
  tabsetPanel(
    # render plots 
    tabPanel(title = "Inertia", plotlyOutput("inertia_plot") %>% withSpinner()),
    tabPanel(title = "Elbow", plotOutput("elbow_plot") %>% withSpinner())
  )}else{
    helpText("input a count matrix first.")
  }
})
output$overlap <- renderUI({
  if (nrow(var$mfuzzTable) != 0){   
    tagList(
      fluidRow(column(
        12, plotOutput('overlap_plot',height = 700) %>% withSpinner()
      )))} else {                   
        helpText("input a count matrix first.")
      }
})

output$mfuzz <- renderUI({
  if (nrow(var$mfuzzTable) != 0){    
    tagList(
      fluidRow(column(12, plotOutput('mfuzz_plots',height = 700) %>% withSpinner()
      )))} else {                      
        helpText("input a count matrix first.")
      }
})