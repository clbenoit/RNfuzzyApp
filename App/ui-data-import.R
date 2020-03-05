# ui-data-import.R
navbarPage(theme=shinytheme("sandstone"),"Data Visualization",
  tabPanel(
    title = tagList(icon("table"), "Data"),
    fluidPage(
      fluidRow(column(3,
    box(
    title = tagList(icon("cloud-upload"), "Upload"),
    solidHeader = T,
    status = "primary",
    width = NULL,
      fileInput(
        "uploadCountData",
        "Upload Count Data",
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv"),
        buttonLabel = "Upload...",
        placeholder = "No file has been uploaded."
      ),
    tags$div(
      HTML('<div class="panel panel-primary">
                    <div class="panel-heading"> <span style="padding-left:10px"><b> Input file description</b> </span></div>
                  <div class="panel-body">
                  <style type="text/css">
                  .tg {
                  border-collapse: collapse;
                  border-spacing: 0;
                  border: none;
                  }
                  .tg td {
                  font-family: Arial, sans-serif;
                  font-size: 14px;
                  padding: 10px 5px;
                  border-style: solid;
                  border-width: 0px;
                  overflow: hidden;
                  word-break: normal;
                  }
                  .tg th {
                  font-family: Arial, sans-serif;
                  font-size: 14px;
                  font-weight: normal;
                  padding: 10px 5px;
                  border-style: solid;
                  border-width: 0px;
                  overflow: hidden;
                  word-break: normal;
                  }
                  .tg .tg-s6z2 {
                  text-align: center
                  }
                  </style>
                  <table class="tg">
                  <tr>
                  <th class="tg-031e"> <span class="label label-primary"> Format</span></th>
                  <th class="tg-031e"> comma-separated values (CSV)
                  </tr>
                  <tr>
                  <th class="tg-031e"> <span class="label label-primary"> Column 1</span></th>
                  <th class="tg-031e"> Sample ID
                  </tr>
                  <tr>
                  <th class="tg-031e"> <span class="label label-primary"> Column 2-n</span></th>
                  <th class="tg-031e"> Other metadata (condition, covariates) </th>
                  </tr>
                  <tr>
                  </table>
                  </div>
                  </div>'))
           )),
    
    column(9,
  box(
    title = tagList(icon("tags"), "Group Assignment"),
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    textAreaInput(
      "groupSelect",
      "Input your group info",
      rows = 6,
      placeholder = paste(
        "rep1,Group1",
        "rep2,Group1",
        "rep1,Group2",
        "rep2,Group2",
        "rep1,Group3",
        "rep2,Group3",
        sep = '\n'
      )
    ),


    do.call(actionBttn, c(
      list(
        inputId = "confirmedGroupList",
        label = "Assign Group Label",
        icon = icon("play"))
      )
    )))),
  box(
    title = tagList(icon("table"), "Read Count Table"),
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("DataSummary"),
    uiOutput("showTable")
  )
  )),
tabPanel(title = tagList(icon("bar-chart"), "Count Distribution"),
         uiOutput("sampleDistributionBoxPanel")),
tabPanel(title = tagList(icon("sitemap"), "Hierarchical Clustering"),
         uiOutput("clustUI")),
tabPanel(title = tagList(icon("object-group"), "PCA"),
         uiOutput("pcaUI"))
)
