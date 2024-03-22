box::use(
  R6[R6Class],
  shiny[reactiveValues, observeEvent, shinyOptions, req],

)

#' @export
DataManager <- R6::R6Class(
  classname = "DataManager",
  public = list(
    buttonsStates = reactiveValues(runPCA = FALSE,
                                    AnalysisRun = FALSE),
    # reactive variables that are saved and can be use in another file than the original where it was created
    var = reactiveValues(
      count = NULL,                        #
      InputTable = data.frame(),           # original input dataa frame
      LowCountGenes = data.frame(),        # data frame of filtered data from the original one
      CountData = data.frame(),            # input data frame after filtering low count genes 
      groupdf = data.frame(),              # data frame from the group assignement, associating replicates to groups
      matrixcount = matrix(),              # matrix of CountData
      groupList = NULL,                    # list of the groups only
      selectedgroups = NULL,               # selected groups of the original data frame is not all are selected
      mfuzzTable = data.frame(),           # mfuzz upload table    
      mfuzzCountData = data.frame(),       # mfuzz data table
      timepoints = NULL,
      design = NULL,                       # design for deseq2 and edgeR
      DEAMETHOD = NULL,                    # chosen analysis method
      result = data.frame("Results will show here." = character(0)), #vmessage to put in the beginning when no input yet
      tccObject = NULL,                    # tcc object containing results of the tcc calculation 
      norData = matrix(),                  # matrix of normalized counts
      pcadata = NULL),             # pca data 
    usefullFunction = function(observation_id) {
    }
  )
)
