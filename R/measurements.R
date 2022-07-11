measurementsUI <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("reload"), "Reload Data", class="btn-primary"),
    hr(),
    fluidRow(
      column(12, withLoader(DTOutput(ns("geneDisease"))))
    )
  )
}

measurementsServer <- function(id, geneId, diseaseId){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    ### Query
    measurements <- reactive({
      query <- "select gd.*, g.symbol, d.name 
            from gene_disease as gd
            left join genes as g
            on gd.geneid = g.geneid
            left join diseases as d
            on gd.diseaseid = d.diseaseid  "
      pool %>%
        dbGetQuery(query) %>%
        collect() 
    })
    
    ### Store identifiers in order to be able to be reseted
    rv <- reactiveValues(geneId = NULL, diseaseId = NULL)
    
    observe({
      rv$geneId <- geneId()
      rv$diseaseId <- diseaseId()
    })

    
    ### Format data
    data <- reactive({
      measurements() %>%
        filter( if( !is.null(rv$geneId) ) geneid == rv$geneId else TRUE ) %>%
        filter( if( !is.null(rv$diseaseId) ) diseaseid == rv$diseaseId else TRUE ) %>%
        mutate(symbol = createLink_Symbol(geneid, symbol),
               nctid =   createLink_NCIT(nctid),
               name = createLink_Name(diseaseid, name)) %>%
      relocate(symbol, .after=nctid) %>%
      relocate(name, .after=symbol) %>%
      rename("Gene" = symbol, "Condition" = name,
             "NCT ID" = nctid, "Measurement" = sentence,
            # "Num. Pmids" = npmids,
             "Biomarker Type" = bmtype ) %>%  arrange(desc(`year`) )
    })

    ### Produce table
    js <- sprintf(
      "table.on('click', 'td', function(){
        var cell = table.cell(this);
        var colindex = cell.index().column;
        var colname = table.column(colindex).header().innerText;
        Shiny.setInputValue('%s', colname);
        console.log(colname);
      });", session$ns("columnName")
    )
    
    output$geneDisease <- renderDataTable(
      datatable(
        data(),
        escape = FALSE,
        filter = "top",
        options = list(dom = 'ltipr',
                       pageLength = 5,
                       columnDefs = list(list(className = 'dt-center', targets ="_all"),
                                         list(visible = FALSE, targets = c(3,4,7,8,10,13,15)))),
        rownames = FALSE,
        selection = list(mode = 'single', target = 'cell'),
        callback = JS(js)
      ) #%>% formatStyle("Num. Pmids", backgroundColor = "yellow")
    )
    
    measurementData <- eventReactive(req(length(input$geneDisease_cell_clicked) > 0), {
      colName <- input$columnName
      geneId <- NULL
      diseaseId <- NULL
      if (colName == "Num. Pmids") {
        geneId <- data()[input$geneDisease_cell_clicked$row, "geneid", drop =TRUE]
        diseaseId <- data()[input$geneDisease_cell_clicked$row, "diseaseid", drop =TRUE]
        return(
          list(
            geneId = geneId,
            diseaseId = diseaseId
          )
        )
      } else return(NULL)
    })
       
    ### Reset button
    observeEvent(input$reload, {
      rv$geneId <- NULL
      rv$diseaseId <- NULL
    })
    
    return(measurementData)
    
  })
}
