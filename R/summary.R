summaryUI <- function(id){
  ns <- NS(id)
  withLoader(DT::DTOutput(ns("gene_disease_summary")))
}

summaryServer <- function(id) {
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
          # filter( if( !is.null(dt$Symbol) ) geneid == dt$Symbol else TRUE ) %>%
          # filter( if( !is.null(dt$DiseaseId) ) diseaseid == dt$DiseaseId else TRUE ) %>%
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
    output$gene_disease_summary <- renderDataTable(
      data(),
      filter = "top",
      options = list(dom = 'ltipr',
                     columnDefs = list(list(className = 'dt-center', targets ="_all"))
      ),
      selection = list(mode = 'single', target = 'cell'),
      escape = FALSE,
      rownames = FALSE,
      callback = JS("table.on('click.dt', 'tr',
                        function() {
                          var data = table.rows(this).data().toArray();
                          Shiny.setInputValue('gds_data', data, {priority: 'event'});
                        });"))
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