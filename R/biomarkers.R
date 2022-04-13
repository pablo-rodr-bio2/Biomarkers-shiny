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
      ) %>% 
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

# output$genes <- DT::renderDataTable({
#   datatable(
#     dt$dt_genes %>%
#       rename( "type of gene" = type_of_gene,
#               "protein class" = PROTEIN_CLASS_NAMES,
#               "DPI" = dpi, "DSI" = dsi, "pLI"=pli,
#               "year initial" = year_initial, "year final" = year_final, 
#               "Num. Clin.Trials" =  nclinicaltrials, 
#               "Num. Diseases" = ndiseases, "Num. Pmids" = npmids)  %>% 
#       select(Gene, description, `type of gene`, DSI, DPI, pLI,`protein class`,
#              `Num. Clin.Trials`, `Num. Diseases`, `Num. Pmids`,
#              `year initial`, `year final`
#       ) %>%  arrange(desc(`Num. Clin.Trials`)),
#     filter = "top",
#     options = list(scrollX = TRUE, dom = 'ltipr', pageLength = 10,
#                    columnDefs = list(list(className = 'dt-center', targets ="_all"))),
#     rownames = FALSE,
#     escape = FALSE,
#     selection = list(mode = 'single', target = 'cell'),
#     callback = JS("table.on('click.dt', 'tr',
#                 function() {
#                   data = table.rows(this).data().toArray();
#                   Shiny.setInputValue('geneid', data, {priority: 'event'});
#                 });")  
#   ) %>% 
#     formatStyle("Num. Pmids", backgroundColor = "yellow")
# 
#   })
# 
# # %>% 
# #   mutate(`Num. Clin.Trials` = createLink_Button(`Num. Clin.Trials`),
# #          `Num. Diseases`= createLink_Button(`Num. Diseases`),
# #          `Num. Pmids` = createLink_Button(`Num. Pmids`))
# 
# 
# geneProxy <- dataTableProxy('genes')
# 
# observeEvent(req(input$geneid), {
#   cell_clicked <- input$genes_cell_clicked
#   geneProxy %>%  selectCells(NULL)
#   if(cell_clicked$col == 0){                ###### redirects to 'Summary'
#     value <- cell_clicked$value
#     dt$Symbol <- dt$dt_genes %>% 
#       filter(Gene == value) %>% 
#       select(geneid) %>% 
#       as.character
#     updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
#   }
#   if(cell_clicked$col == 7){                ####### redirects to 'Measurements'
#     value <- input$geneid[1]
#     geneProxy %>%  selectCells(NULL)
#     dt$gd_symbol <- dt$dt_genes %>% 
#       filter(Gene == value) %>% 
#       select(symbol) %>% 
#       as.character
#     updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
#   }
#   if(cell_clicked$col == 8){                ####### redirects to 'Summary'
#     value <- input$geneid[1]
#     geneProxy %>%  selectCells(NULL)
#     dt$Symbol <- dt$dt_genes %>%
#       filter(Gene == value) %>%
#       select(geneid) %>%
#       as.character
#     updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
#   }
#   if(cell_clicked$col == 9){                ####### redirects to 'Publications'
#     value <- input$geneid[1]
#     geneProxy %>%  selectCells(NULL)
#     dt$Symbol <- dt$dt_genes %>%
#       filter(Gene == value) %>%
#       select(geneid) %>%
#       as.character
#     updateNavbarPage(inputId = "navbarPage", selected = "publications")
#   }
# })

