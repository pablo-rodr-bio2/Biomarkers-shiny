publicationUI <- function(id){
  ns <- NS(id)
  withLoader(DTOutput(ns("publications")))
}

publicationServer <- function(id){
  moduleServer(id, function(input, output, session){
    
    ###  Query
    publications <- reactive({
      pool %>%
      tbl("publications") %>% 
      collect()
    })
    
    ### Format data
    data <- reactive({
      publications() %>% 
        mutate(nctid =   createLink_NCIT(nctid),
               pmid =   createLink_PMID(pmid)) %>%
        select(-id) %>%
        rename("NCT ID" = nctid) %>%
        arrange(desc(year))
    })
    
    ### Produce table
    output$publications <- renderDataTable(
      data(), filter = "top",
      options = list(dom = 'ltipr',
                     columnDefs = list(list(className = 'dt-center', targets ="_all"))),
      selection = list(mode = 'single', target = 'cell'),
      escape = FALSE,
      rownames = FALSE
    )
  })
}