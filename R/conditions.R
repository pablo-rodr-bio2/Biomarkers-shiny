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
                   "year initial CT" = year_initial_ct,
                   "year final CT" = year_final_ct,
                   "Num. Clin.Trials" =  nclinicaltrials,
                   "Num. Pmids" = npmids, 
                   "year initial PMID" = year_initial_pmid, 
                   "year final PMID" = year_final_pmid)  %>%
            select(Condition, "Semantic Type",  "Num. Biomarkers" ,
                   `Num. Clin.Trials`,   
                   `year initial CT`, `year final CT`, `Num. Pmids`, 
                   `year initial PMID`, `year final PMID`) %>%
        arrange(desc(`Num. Clin.Trials`) )
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
      datatable(
        data(),
        filter = "top",
        selection = list(mode = 'single', target = 'cell'),
        options = list(dom = 'ltipr', columnDefs = list(list(className = 'dt-center', targets ="_all"))),
        rownames = FALSE,
        escape = FALSE,
        callback = JS(js)
      ) %>% formatStyle("Condition", backgroundColor = "green") %>% 
        formatStyle("Num. Biomarkers", backgroundColor = "green") %>% 
        formatStyle("Num. Clin.Trials", backgroundColor = "purple") %>% 
        formatStyle("Num. Pmids", backgroundColor = "yellow")
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