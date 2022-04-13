summaryUI <- function(id){
  ns <- NS(id)
  withLoader(DT::DTOutput(ns("geneDiseaseSummary")))
}

summaryServer <- function(id, geneId, diseaseId) {
  moduleServer(id, function(input, output, session) {
    
    ### Query
    summaries <- reactive({
      pool %>%
        tbl("gene_disease_summary") %>% 
        collect() 
    })
    
    ### Format data
    data <- reactive({
      summaries() %>%
        filter( if( !is.null(geneId()) ) geneid == geneId() else TRUE ) %>%
        filter( if( !is.null(diseaseId()) ) diseaseid == diseaseId() else TRUE ) %>%
        rename( "Gene" = symbol, "Condition" = name,
                "year initial" = year_initial, "year final" = year_final,
                "Num. Clin.Trials" =  nclinicaltrials,
                "Num. Pmids" = npmids)  %>%
        arrange(desc(`Num. Clin.Trials`) ) %>%
        mutate(Fisher = formatC(Fisher, format="e", digits=2, zero.print = TRUE)) %>% 
        relocate(c(`Num. Clin.Trials`, `Num. Pmids`, `year initial`, `year final`), .after = OddsRatio)
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
    
    output$geneDiseaseSummary <- renderDataTable(
      datatable(
        data(),
        filter = "top",
        options = list(dom = 'ltipr',
                       columnDefs = list(
                         list(className = 'dt-center', targets ="_all"),
                         list(visible = FALSE, targets=c(1,2,10))
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

    return(summaryData)

  })
}