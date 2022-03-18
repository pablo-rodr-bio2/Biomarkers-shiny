biomarkersUI <- function(id){
  ns <- NS(id)
  tagList(
    withLoader(DT::dataTableOutput(ns("genes"))),
    textOutput(ns("text1"))
  )
  
}

biomarkersServer <- function(id, genes, parent_session) {
  moduleServer(id, function(input, output, session) {
    
    geneid <- reactive(NULL)
    
    ### save data from MySQL and then format it
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
               `Num. Clin.Trials`, `Num. Diseases`, `Num. Pmids`,
               `year initial`, `year final`) %>%
        arrange(desc(`Num. Clin.Trials`))
    )
    
    ### produce table
    # in order to use formatStyle(), must use datatable() wrapper first
    output$genes <- DT::renderDataTable({
      datatable(
        data(),
        filter = "top",
        options = list(scrollX = TRUE, dom = 'ltipr', pageLength = 10,
                       columnDefs = list(list(className = 'dt-center', targets ="_all"))),
        rownames = FALSE,
        escape = FALSE,
        selection = list(mode = 'single', target = 'cell')
      ) %>% 
        formatStyle("Num. Pmids", backgroundColor = "yellow")
      })
    
    geneid <- eventReactive(req(length(input$genes_cell_clicked) > 0), {
      col <- input$genes_cell_clicked$col
      if(col == 0) {
        geneid <- data()[input$genes_cell_clicked$row, "Gene", drop =TRUE]
        geneid <- geneid %>% read_html() %>% html_node("a") %>% html_attr("id")
        geneid
      } else return(NULL)
    })
    
    output$text1 <- renderText(geneid())
    
    return(geneid)

  })
}

