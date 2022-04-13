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
          select(-geneid, -diseaseid, -id) %>%
          rename( "Gene" = symbol, "Condition" = name,
                  "year initial" = year_initial, "year final" = year_final,
                  "Num. Clin.Trials" =  nclinicaltrials,
                  "Num. Pmids" = npmids)  %>%
          arrange(desc(`Num. Clin.Trials`) ) %>%
          select(Gene, Condition, Fisher, OddsRatio,
                 `Num. Clin.Trials`,   `Num. Pmids`,
                 `year initial`, `year final`) %>%
          mutate(`Num. Clin.Trials` = createLink_Button(`Num. Clin.Trials`),
                 Fisher = formatC(Fisher, format="e", digits=2, zero.print = TRUE))
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
      data(),
      filter = "top",
      options = list(dom = 'ltipr',
                     columnDefs = list(list(className = 'dt-center', targets ="_all"))
      ),
      selection = list(mode = 'single', target = 'cell'),
      escape = FALSE,
      rownames = FALSE,
      callback = JS(js)
    )
    
    summaryData <- eventReactive(req(length(input$geneDiseaseSummary_cell_clicked) > 0), {
      colName <- input$columnName
      geneId <- NULL
      diseaseId <- NULL
      if (colName %in% c("Num. Clin.Trials", "Num. Pmids" )) {
        geneId <- data()[input$geneDiseaseSummary_cell_clicked$row, "Gene", drop =TRUE]
        diseaseId <- data()[input$geneDiseaseSummary_cell_clicked$row, "Condition", drop =TRUE]
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


# 
# gdsProxy <- dataTableProxy("gene_disease_summary")
# 
# observeEvent(input$gds_data, {
#   value <- input$gene_disease_summary_cell_clicked
#   gdsProxy %>% selectCells(NULL)
#   col <- value$col
#   if(col == "4"){
#     dt$gd_symbol <- input$gds_data[1]
#     dt$gd_name <- input$gds_data[2]
#     updateNavbarPage( inputId = "navbarPage", selected = "gene_disease")
#   } 
# })