summaryUI <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("reload"), "Reload Data", class="btn-primary"),
    hr(),
    fluidRow(
      column(12, withLoader(DT::DTOutput(ns("geneDiseaseSummary"))))      
    )
  )
}

summaryServer <- function(id, geneId, diseaseId) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ### Query
    summaries <- reactive({
      pool %>%
        tbl("gene_disease_summary") %>% 
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
      summaries() %>%
        filter( if( !is.null(rv$geneId) ) geneid == rv$geneId else TRUE ) %>%
        filter( if( !is.null(rv$diseaseId) ) diseaseid == rv$diseaseId else TRUE ) %>%
        rename( "Gene" = symbol, "Condition" = name,
                "year initial CT" = year_initial_ct,
                "year final CT" = year_final_ct,
                "Num. Clin.Trials" =  nclinicaltrials,
                "Num. Pmids" = npmids, 
                "year initial PMID" = year_initial_pmid, 
                "year final PMID" = year_final_pmid)  %>%
        arrange(desc(`Num. Pmids`) )# %>%
        #mutate(Fisher = formatC(Fisher, format="e", digits=2, zero.print = TRUE)) %>% 
        #relocate(c(`Num. Clin.Trials`, `Num. Pmids`, `year initial`, `year final`), .after = OddsRatio)
    })
    
    ### Produce table
    
    # js
    js <- sprintf(
      "table.on('click', 'td', function(){
        var cell = table.cell(this);
        var colindex = cell.index().column;
        var colname = table.column(colindex).header().innerText;
        Shiny.setInputValue('%s', colname);
        console.log(colname);
      });", session$ns("columnName")
    )
    
    # table
    output$geneDiseaseSummary <- renderDataTable(
      datatable(
        data(),
        filter = "top",
        options = list(dom = 'ltipr',
                       columnDefs = list(
                         list(className = 'dt-center', targets ="_all"),
                         list(visible = FALSE, targets=c(0,1,10))
                       )
        ),
        selection = list(mode = 'single', target = 'cell'),
        escape = FALSE,
        rownames = FALSE,
        callback = JS(js)
        ) %>%
        formatStyle("Num. Clin.Trials", backgroundColor = "purple") %>% 
        formatStyle("Num. Pmids", backgroundColor = "yellow")
    )
    
    ## data to be returned (ids for other tables)
    summaryData <- eventReactive(req(length(input$geneDiseaseSummary_cell_clicked) > 0), {
      colName <- input$columnName
      geneId <- NULL
      diseaseId <- NULL
      if (colName %in% c("Num. Clin.Trials", "Num. Pmids")) {
        geneId <- data()[input$geneDiseaseSummary_cell_clicked$row, "geneid", drop =TRUE]
        diseaseId <- data()[input$geneDiseaseSummary_cell_clicked$row, "diseaseid", drop =TRUE]
        return(
          list(
            geneId = geneId,
            diseaseId = diseaseId,
            colName = colName
          )
        )
      } else return(NULL)
    })
    
    ### Reset button
    observeEvent(input$reload, {
      rv$geneId <- NULL
      rv$diseaseId <- NULL
    })

    return(summaryData)

  })
}