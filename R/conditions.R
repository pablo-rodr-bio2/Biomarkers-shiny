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
    output$diseases <- renderDataTable(
      data(),
      filter = "top",
      selection = list(mode = 'single', target = 'cell'),
      options = list(dom = 'ltipr', columnDefs = list(list(className = 'dt-center', targets ="_all"))),
      rownames = FALSE,
      escape = FALSE,
      callback = JS("table.on('click.dt', 'tr',
                      function() {
                        var data = table.rows(this).data().toArray();
                        Shiny.setInputValue('diseaseid', data, {priority: 'event'});
                      });")
      )
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