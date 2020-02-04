# server-ma-plot.R

runMA <- reactiveValues(runMAValues = FALSE)


summary_for_norm_result <- function(df){
  sum_gene <- function(x, df){
    sum(df$q.value<=x)
  }
  # Set cut-offs
  span <- c(0, seq(0.05, 1, 0.05))
  deg_in_cutoff <- sapply(span, sum_gene, df)
  total_gene <- nrow(df)
  
  # Create table
  df <- data.frame(
    "Cutoff" = sprintf('%#.2f', span),
    "Under_Count" = deg_in_cutoff,
    "Percentage" = paste(round(deg_in_cutoff / total_gene, 4) * 100, "%")
  )
  df <- tbl_df(df) %>% mutate(Between_Count = Under_Count - lag(Under_Count)) %>%
    mutate(Count = paste0(Under_Count, "(+", Between_Count, ")"))
  return(df)
}

#parameters
observeEvent(input$sider, {
  if (input$sider == "maplotTab") {
    output$MAPlotParameter <- renderUI({
      tagList(
        sliderInput(
          "pointSize",
          "Point Size",
          min = 1,
          max = 5,
          value = 3,
          step = 0.2
        ),
        sliderInput(
          "maFDR",
          "FDR Cut-off",
          min = 0.01,
          max = 1,
          value = input$fdr,
          step = 0.01
        ),
        spectrumInput(
          inputId = "fdrColor",
          label = tagList("DEGs Color", htmlOutput("maFDRpreview")),
          choices = list(
            list(
              "#B22222",
              'black',
              'white',
              'blanchedalmond',
              'steelblue',
              'forestgreen'
            ),
            as.list(brewer.pal(n = 9, name = "Reds")),
            as.list(brewer.pal(n = 9, name = "Greens")),
            as.list(brewer.pal(n = 11, name = "Spectral")),
            as.list(brewer.pal(n = 8, name = "Dark2"))
          ),
          options = list(`toggle-palette-more-text` = "Show more")
        ),
        do.call(actionBttn, c(
          list(
            inputId = "makeMAPlot",
            label = "Generate MA Plot",
            icon = icon("play")
          )
        ))
      )
    })
  }
})

# generating maplot after parameters set

observeEvent(input$makeMAPlot, {
  output$maploty <- renderPlotly({
    validate(need(resultTable()$a.value != "", "No MA values for ploting."))
    
    req(input$makeMAPlot)
    isolate({
      key <- resultTable()$gene_id #link to bar plot
      
      if (is.null(input$resultTableInPlot_rows_selected)) {
        annotation <- list()
      } else {
        markerSelect <-
          resultTable()[input$resultTableInPlot_rows_selected, ]
        
        annotation <- list(
          x = markerSelect$a.value,
          y = markerSelect$m.value,
          text = markerSelect$gene_id,
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 7,
          ax = 20,
          ay = 40
        )
      }
      
      x <- cut(resultTable()$q.value, breaks = c(0, input$maFDR, 1))
      levels(x) <-
        list(
          "DEG" = paste("(0,", input$maFDR, "]", sep = ""),
          "non-DEG" = paste("(", input$maFDR, ",1]", sep = "")
        )
      
      p <- plot_ly(
        data = resultTable(),
        x = ~ a.value,
        y = ~ m.value,
        type = "scatter",
        mode = "markers",
        color = ~ x,
        colors = c(input$fdrColor, "#000000"),
        marker = list(size = input$pointSize),
        hoverinfo = "text+name",
        text = ~ paste(
          "</br>Gene:",
          resultTable()$gene_id,
          "</br>A value:",
          round(a.value, 4),
          "</br>M value:",
          round(m.value, 4),
          "</br>Rank:",
          rank
        ),
        key =  ~ key,
        source = "ma"
      ) %>%
        layout(
          xaxis = list(title = "A = (log<sub>2</sub>(G2)+log<sub>2</sub>(G1))/2"),
          yaxis = list(title = "M = log<sub>2</sub>(G2)-log<sub>2</sub>(G1)"),
          title = paste0(
            "MA Plot with q-value < ",
            input$maFDR,
            " (",
            input$maFDR * 100,
            "% FDR)"
          ),
          annotations = annotation,
          legend = list(
            orientation = 'h',
            xanchor = "center",
            x = 0.5,
            y = 1.05
          )
        )
      variables$MAPlotObject <- p
      p
    })
  })
  runMA$runMAValues <- input$makeMAPlot
})

# Under FDR cutoff, preview the gene number 
output$maFDRpreview <- renderText({
  count <- nrow(resultTable()[resultTable()$q.value <= input$maFDR,])
  paste0("<font color=\"",
         input$fdrColor,
         "\"><b>",
         count,
         " genes</b></font>")
})

# Render UI
output$MAPlotUI <- renderUI({
  if (runMA$runMAValues) {
    tagList(fluidRow(
      column(8, plotlyOutput("maploty") %>% withSpinner()),
      column(4, plotlyOutput("geneBarPlot") %>% withSpinner())
    ))
  } else {
    helpText("Please click [Generate MA Plot] first.")
  }
})


output$geneBarPlot <- renderPlotly({
  eventdata <- event_data("plotly_hover", source = "ma")
  validate(need(
    !is.null(eventdata),
    "Hover over the point to show gene's expression level of interest."
  ))
  # Get point number
  gene_id <- eventdata$key
  # Get expression level (Original)
  expression <-
    variables$CountData[row.names(variables$CountData) == gene_id, ]
  # Get expression level (Normalized)
  expressionNor <-
    t(t(variables$norData[row.names(variables$norData) == gene_id, ]))
  
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
    textposition = "outside",
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


# This function render a table of Result table.

output$resultTableInPlot <- DT::renderDataTable({
  if (nrow(resultTable()) == 0) {
    DT::datatable(resultTable())
  } else {
    if (length(input$maFDR) > 0) {
      fdrCut <- input$maFDR
      fdrColor <- input$fdrColor
    } else {
      fdrCut <- 0
      fdrColor <- "#B22222"
    }
    
    DT::datatable(
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
          HTML("<font color=\"#B22222\">Gene Name</font> is colored when under FDR cut-off.")
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
    ) %>% formatStyle("gene_id",
                      "q.value",
                      color = styleInterval(fdrCut, c(fdrColor, "")))
  }
})


# Render final table

output$fdrCutoffTableInMAPage <- DT::renderDataTable({
  df <- summary_for_norm_result(resultTable())
  
  df <- df[, c("Cutoff", "Count", "Percentage")]
  colnames(df) <- c("Cut-off", "DEGs(#)", "DEGs(%)")
  DT::datatable(
    df,
    caption = "Number (#) and Percentage (%) of DEGs satisfying different FDR cut-off.",
    option = list(
      pageLength = 10,
      columnDefs = list(list(
        className = 'dt-right', targets = "_all"
      )),
      dom = "tp"
    ),
    rownames = FALSE
  )
})
