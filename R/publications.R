publicationUI <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("reload"), "Reload Data", class="btn-primary"),
    hr(),
    withLoader(DTOutput(ns("publications")))
  )
}

publicationServer <- function(id, geneId, diseaseId){
  moduleServer(id, function(input, output, session){
    
    ###  Query
    publications <- reactive({
      pool %>%
      tbl("publications") %>% 
      collect()
    })
    
    ### Store identifiers in order to be able to be reseted
    rv <- reactiveValues(geneId = NULL, diseaseId = NULL)
    
    observe({
      rv$geneId <- geneId()
      rv$diseaseId <- diseaseId()
    })
    
    ### Format data
    data <- reactive({
      publications() %>% 
        filter( if( !is.null(rv$geneId) ) geneid == rv$geneId else TRUE ) %>%
        filter( if( !is.null(rv$diseaseId) ) diseaseid == rv$diseaseId else TRUE ) %>%
        mutate(    pmid =   createLink_PMID(pmid)) %>%
        select(-id) %>%
        rename("PMID" = pmid) %>%
        arrange(desc(year))
    })
    
    ### Produce table
    output$publications <- renderDataTable(
      data(), filter = "top",
      options = list(dom = 'ltipr',
                     columnDefs = list(list(className = 'dt-center', targets ="_all"),
                                       list(visible = FALSE, targets = c(1,3,5,11)))),
      selection = list(mode = 'single', target = 'cell'),
      escape = FALSE,
      rownames = FALSE
    )
    
    ### Reset button
    observeEvent(input$reload, {
      rv$geneId <- NULL
      rv$diseaseId <- NULL
    })
  })
}