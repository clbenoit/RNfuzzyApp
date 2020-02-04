# server-volcano-plot.R

runVolcano <- reactiveValues(runVolcanoValue = FALSE)

# This function render a series UI of Volcano Plot parameters 
observeEvent(input$sider, {
  if (input$sider == "volcanoplotTab") {
      output$valcanoParameter <- renderUI({
        tagList(
          textInput("graphicTitle", "Graphic Title", value = "Volcano Plot"),
          textInput("xlabs", "X-axis Label", value = "log<sub>2</sub>(Fold Change)"),
          textInput("ylabs", "Y-axis Label", value = "-log<sub>10</sub>(P-value)"),
          sliderInput(
            "CutFC",
            "Fold Change (X-axis) Cut-off",
            min = ceiling(min(resultTable()$m.value)),
            max = floor(max(resultTable()$m.value)),
            value = c(-1, 1),
            step = 0.5
          ),
          sliderInput(
            inputId = "Cutpvalue",
            label = "P-value Cut-off",
            min = 0.01,
            value = 0.05,
            max = 1,
            step = 0.01
          ),
          sliderInput(
            "volcanoPointSize",
            "Point Size",
            min = 1,
            max = 5,
            value = 3,
            step = 0.2
          ),
          spectrumInput(
            inputId = "downColor",
            label = tagList("Down-regulated in G2", htmlOutput("downPreview")),
            choices = list(
              list(
                "green",
                'black',
                'white',
                'blanchedalmond',
                'steelblue',
                'forestgreen'
              ),
              as.list(brewer.pal(n = 9, name = "Blues")),
              as.list(brewer.pal(n = 9, name = "Greens")),
              as.list(brewer.pal(n = 11, name = "Spectral"))
            ),
            options = list(`toggle-palette-more-text` = "Show more")
            
          ),
          spectrumInput(
            inputId = "upColor",
            label = tagList("Up-regulated in G2", htmlOutput("upPreview")),
            choices = list(
              list(
                "red",
                'black',
                'white',
                'blanchedalmond',
                'steelblue',
                'forestgreen'
              ),
              as.list(brewer.pal(n = 9, name = "Oranges")),
              as.list(brewer.pal(n = 9, name = "Reds")),
              as.list(brewer.pal(n = 11, name = "Spectral"))
            ),
            options = list(`toggle-palette-more-text` = "Show more")
          ),
          do.call(actionBttn, c(
            list(
              inputId = "makeVolcanoPlot",
              label = "Generate Volcano Plot",
              icon = icon("play")
            )
          ))
        )
      })
  }
})

# Preview up and down regulated genes under Color selection 
observeEvent({
  input$CutFC
  input$Cutpvalue
}, {
  dt <- resultTable()
  downCut <- input$CutFC[1]
  upCut <- input$CutFC[2]
  pvalueCut <- input$Cutpvalue
  
  downCount <-
    nrow(dt[dt$m.value <= downCut & dt[["p.value"]] <= pvalueCut,])
  upCount <-
    nrow(dt[dt$m.value >= upCut & dt[["p.value"]] <= pvalueCut,])
  
  output$downPreview <- renderText({
    paste0("<font color=\"",
           input$downColor,
           "\"><b>",
           downCount,
           " genes</b></font>")
  })
  output$upPreview <- renderText({
    paste0("<font color=\"",
           input$upColor,
           "\"><b>",
           upCount,
           " genes</b></font>")
  })
})

# Check the `Generate` button, if the botton has been clicked, generate volcano plot 

observeEvent(input$makeVolcanoPlot, {
  yaxis <- "p.value"
  output$volcanoPloty <- renderPlotly({
    validate(need(resultTable()[[yaxis]] != "", "No p-values for ploting."))
    if (length(variables$groupList) > 2) {
      sendSweetAlert(
        session = session,
        title = "ERROR",
        text = "Volcano Plot is unavailable for multiple comparison now.",
        type = "info"
      )
    }
    
    req(input$makeVolcanoPlot)
    isolate({
      dt <- resultTable()
      
      downCut <- input$CutFC[1]
      upCut <- input$CutFC[2]
      
      dt$color <- "None"
      tryCatch({
        dt[dt$m.value <= downCut,]$color <- "Down"
        dt[dt$m.value >= upCut,]$color <- "Up"
        dt[dt[[yaxis]] > input$Cutpvalue,]$color <-
          "None"
      }, error = function(e) {
        sendSweetAlert(session = session, title = "ERROR", text = "No data was satisfied to your cut-off!")
      })
      
      x <- factor(dt$color)
      levels(x) <- list("Down" = 0,
                        "None" = 1,
                        "Up" = 2)
      
      # link to bar plot
      key <- resultTable()$gene_id
      
      if (is.null(input$resultTableInVolcanalPlot_rows_selected)) {
        annotation <- list()
      } else {
        markerSelect <- dt[input$resultTableInVolcanalPlot_rows_selected, ]
        
        annotation <- list(
          x = markerSelect$m.value,
          y = -log10(markerSelect[[yaxis]]),
          text = markerSelect$gene_id,
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 7,
          ax = 20,
          ay = 40
        )
      }
      
      p <- plot_ly(
        data = dt,
        x = ~ m.value,
        y = ~ -log10(dt[[yaxis]]),
        type = "scatter",
        mode = "markers",
        color = ~ x,
        colors = c(input$downColor, "black", input$upColor),
        marker = list(size = input$volcanoPointSize),
        hoverinfo = "text",
        text = ~ paste(
          "</br>Gene:",
          resultTable()$gene_id,
          "</br>A value:",
          round(a.value, 4),
          "</br>M value:",
          round(m.value, 4),
          "</br>p-value:",
          round(p.value, 4),
          "</br>q-value:",
          round(q.value, 4),
          "</br>Rank:",
          rank
        ),
        key =  ~ key,
        source = "volcano"
      ) %>%
        layout(
          xaxis = list(title = input$xlabs),
          yaxis = list(title = input$ylabs),
          title = input$graphicTitle,
          legend = list(
            orientation = 'h',
            xanchor = "center",
            x = 0.5,
            y = 1.05
          ),
          annotations = annotation,
          shapes = list(
            list(
              type = 'line',
              y0 =  ~ min(-log10(dt[[yaxis]])),
              y1 =  ~ max(-log10(dt[[yaxis]])),
              x0 = upCut,
              x1 = upCut,
              line = list(dash = 'dot', width = 2)
            ),
            list(
              type = 'line',
              y0 =  ~ min(-log10(dt[[yaxis]])),
              y1 =  ~ max(-log10(dt[[yaxis]])),
              x0 = downCut,
              x1 = downCut,
              line = list(dash = 'dot', width = 2)
            ),
            list(
              type = 'line',
              y0 = -log10(input$Cutpvalue),
              y1 = -log10(input$Cutpvalue),
              x0 =  ~ min(m.value),
              x1 =  ~ max(m.value),
              line = list(dash = 'dot', width = 2)
            )
          )
        )
      variables$VolcanoPlotObject <- p
      p
    })
  })
  runVolcano$runVolcanoValue <- input$makeVolcanoPlot
})

# Render volcanoUI 
output$volcanoUI <- renderUI({
  if(runVolcano$runVolcanoValue){
    tagList(
      fluidRow(
        column(8, plotlyOutput("volcanoPloty") %>% withSpinner()),
        column(4, plotlyOutput("VolcanoBarPlot") %>% withSpinner())
      )
    )
  } else {
    helpText("Please click [Generate Volcano Plot] first.")
  }
})

# Render table result

output$resultTableInVolcanalPlot <- DT::renderDataTable({
  if (nrow(resultTable()) == 0) {
    DT::datatable(resultTable())
  } else {
    if (length(input$Cutpvalue) > 0) {
      fdrCut <- input$Cutpvalue
      
    } else {
      fdrCut <- 0
    }
    
    t <- DT::datatable(
      resultTable(),
      colnames = c(
        "Gene Name",
        "A Value",
        "M Value",
        "P Value",
        "Q Value (FDR)",
        "Rank",
        "estimated DEG"
      ),
      filter = "bottom",
      caption = tags$caption(
        tags$li(
          "Gene Name was colored according to Fold Changeand set bold according to P-value cut-off."
        )
      ),
      extensions = c("Scroller", "Buttons"),
      option = list(
        dom = 'Bfrtip',
        buttons =
          list(
            'copy',
            'print',
            list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            )
          ),
        deferRender = TRUE,
        scrollY = 400,
        scroller = TRUE,
        searchHighlight = TRUE,
        orderClasses = TRUE,
        scrollX = TRUE,
        columnDefs = list(list(
          visible = FALSE, targets = -1
        ))
      )
    ) %>% formatRound(
      columns = c("a.value",
                  "m.value",
                  "p.value",
                  "q.value"),
      digits = 3
    )
    
    if (!is.na(sum(resultTable()$m.value))) {
      t   %>% formatStyle("gene_id", "m.value",
                          color = styleInterval(input$CutFC,
                                                c(
                                                  input$downColor, "black", input$upColor
                                                ))) %>% formatStyle("gene_id",
                                                                    "p.value",
                                                                    fontWeight = styleInterval(fdrCut, c("bold", "normal")))
    } else {
      t
    }
  }
})


# render barplot next to volcanoplot

output$VolcanoBarPlot <- renderPlotly({
  eventdata <- event_data("plotly_hover", source = "volcano")
  validate(need(
    !is.null(eventdata),
    "Hover over the point to show gene's expression level of interest."
  ))

  gene_id <- eventdata$key

  expression <-
    variables$CountData[row.names(variables$CountData) == gene_id,]

  expressionNor <-
    t(t(variables$norData[row.names(variables$norData) == gene_id,]))
  
  data <- variables$CountData
  data.cl <- variables$groupListConvert
  
  expression <- t(expression[data.cl != 0])
  data.cl <- data.cl[data.cl != 0]
  
  xOrder <-
    data.frame("name" = row.names(expression), "group" = data.cl)
  xOrderVector <- unique(xOrder[order(xOrder$group),]$name)
  xform <- list(categoryorder = "array",
                categoryarray = xOrderVector,
                title = "")
  
  plot_ly(
    x = ~ row.names(expression),
    y = ~ expression[, 1],
    color = as.factor(data.cl),
    text = expression[, 1],
    textposition = 'outside',
    showlegend = FALSE,
    type = "bar",
    name = "Raw"
  ) %>%
  layout(
    xaxis = xform,
    yaxis = list(title = "Raw Count"),
    title = colnames(expression)
  )
})