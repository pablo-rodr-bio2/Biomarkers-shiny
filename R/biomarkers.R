biomarkersUI <- function(id){
  ns <- NS(id)
  withLoader(DT::dataTableOutput(ns("genes")))
}

biomarkersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    geneid <- reactive(NULL)
    
    ### Query
    genes <-  reactive({
      pool %>%
        tbl("genes") %>% 
        collect() %>% 
        mutate(Gene = createLink_Symbol(geneid, symbol))
    })

    ### Format data
    data <- reactive(
      genes() %>% 
        rename( "type of gene" = type_of_gene,
                "protein class" = PROTEIN_CLASS_NAMES,
                "DPI" = dpi, 
                "DSI" = dsi, 
                "pLI"=pli,
                "year initial" = year_initial,
                "year final" = year_final,
                "Num. Clin.Trials" =  nclinicaltrials,
                "Num. Diseases" = ndiseases,
                "Num. Pmids" = npmids)  %>%
        select(Gene, description, `type of gene`, DSI, DPI, pLI,`protein class`,
               `Num. Diseases`, `Num. Clin.Trials`, `Num. Pmids`,
               `year initial`, `year final`) %>%
        arrange(desc(`Num. Clin.Trials`))
    )
    
    ### Produce table
    
    # javascript for the table: gets the column name of the clicked cell
    js <- sprintf(
      "table.on('click', 'td', function(){
        var cell = table.cell(this);
        var colindex = cell.index().column;
        var colname = table.column(colindex).header().innerText;
        Shiny.setInputValue('%s', colname);
      });", session$ns("columnName")
    )
    
    # in order to use formatStyle(), must use datatable() wrapper first
    output$genes <- DT::renderDataTable({
      datatable(
        data(),
        filter = "top",
        options = list(scrollX = TRUE, dom = 'ltipr', pageLength = 10,
                       columnDefs = list(list(className = 'dt-center', targets ="_all"))),
        rownames = FALSE,
        escape = FALSE,
        selection = list(mode = 'single', target = 'cell'),
        callback = JS(js)
      ) %>% formatStyle("Gene", backgroundColor = "green") %>% 
        formatStyle("Num. Diseases", backgroundColor = "green") %>% 
        formatStyle("Num. Clin.Trials", backgroundColor = "purple") %>% 
        formatStyle("Num. Pmids", backgroundColor = "yellow")
      })
    
    
    geneData <- eventReactive(req(length(input$genes_cell_clicked) > 0), {
      colName <- input$columnName
      geneId <- NULL
      if (colName %in% c("Gene", "Num. Diseases", "Num. Clin.Trials", "Num. Pmids") ) {
        geneId <- data()[input$genes_cell_clicked$row, "Gene", drop =TRUE]
        geneId <- geneId %>% read_html() %>% html_node("a") %>% html_attr("id")
        return(
          list(
            geneId = geneId,
            colName = colName
          )
        )
      } else return(NULL)
      
    })
    
    return(geneData)

  })
}
