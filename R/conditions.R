conditionsUI <- function(id){
  ns <- NS(id)
  DT::dataTableOutput(ns("diseases"))  
}

conditionServer <- function(id){
  moduleServer(id, function(input, output, session) {
    
    ### Query
    diseases <- reactive({
      pool %>% 
        tbl("diseases") %>% 
        collect() %>%
        mutate( Condition = createLink_Name(diseaseid, name))
    })
    
    ### Format data
    data <- reactive({
      diseases() %>%
            rename("Semantic Type" = sty,
                   "Num. Biomarkers" = nbiomarkers,
                   "year initial" = year_initial, "year final" = year_final,
                   "Num. Clin.Trials" =  nclinicaltrials,
                   "Num. Pmids" = npmids) %>%
            select(Condition, "Semantic Type",  "Num. Biomarkers" ,
                   `Num. Clin.Trials`,   `Num. Pmids`,
                   `year initial`, `year final`) %>%  arrange(desc(`Num. Clin.Trials`) ) %>%
            mutate(`Num. Biomarkers` = createLink_Button(`Num. Biomarkers`),
                   `Num. Clin.Trials` = createLink_Button(`Num. Clin.Trials`),
                   `Num. Pmids` = createLink_Button(`Num. Pmids`))
      
    })
    
    ### Produce table
    js <- sprintf(
      "table.on('click', 'td', function(){
        var cell = table.cell(this);
        var colindex = cell.index().column;
        var colname = table.column(colindex).header().innerText;
        Shiny.setInputValue('%s', colname);
      });", session$ns("columnName")
    )
    
    output$diseases <- renderDataTable(
      data(),
      filter = "top",
      selection = list(mode = 'single', target = 'cell'),
      options = list(dom = 'ltipr', columnDefs = list(list(className = 'dt-center', targets ="_all"))),
      rownames = FALSE,
      escape = FALSE,
      callback = JS(js)
    )
    
    diseaseData <- eventReactive(req(length(input$diseases_cell_clicked) > 0), {
      colName <- input$columnName
      diseaseId <- NULL
      if (colName %in% c("Condition", "Num. Biomarkers", "Num. Clin.Trials", "Num. Pmids") ) {
        diseaseId <- data()[input$diseases_cell_clicked$row, "Condition", drop =TRUE]
        diseaseId <- diseaseId %>% read_html() %>% html_node("a") %>% html_attr("id")
        return(
          list(
            diseaseId = diseaseId,
            colName = colName
          )
        )
      } else return(NULL)
    })
    
    return(diseaseData)
  })
}


# 
# diseaseProxy <- dataTableProxy('diseases')
# 
# observeEvent(input$diseaseid, {
#   cell_clicked <- input$diseases_cell_clicked
#   diseaseProxy %>% selectCells(NULL)
#   if(cell_clicked$col == 0){
#     value <- cell_clicked$value
#     dt$DiseaseId <- dt$dt_diseases %>%
#       filter(Condition == value) %>%
#       select(diseaseid) %>%
#       as.character()
#     updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
#   }
#   if(cell_clicked$col == 2){
#     value <- input$diseaseid[1]
#     dt$DiseaseId <- dt$dt_diseases %>%
#       filter(Condition == value) %>%
#       select(diseaseid) %>%
#       as.character()
#     updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
#   }
#   if(cell_clicked$col == 3){
#     value <- input$diseaseid[1]
#     dt$gd_name <- dt$dt_diseases %>%
#       filter(Condition == value) %>%
#       select(name) %>%
#       as.character()
#     updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
#   }
#   if(cell_clicked$col == 4){
#     value <- input$diseaseid[1]
#     dt$DiseaseId <- dt$dt_diseases %>%
#       filter(Condition == value) %>%
#       select(diseaseid) %>%
#       as.character()
#     updateNavbarPage(inputId = "navbarPage", selected = "publications")
#   }
# })