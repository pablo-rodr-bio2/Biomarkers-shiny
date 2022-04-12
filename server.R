function(input, output, session) {
  
  ###############################################################
  ################# REACTIVE VALUES #############################
  ###############################################################
  
  dt <- reactiveValues(Symbol = NULL,
                       DiseaseId = NULL,
                       Biomarker = NULL,
                       gd_symbol = NULL,
                       gd_name = NULL)

  ###############################################################
  ###################### TABLES #################################
  ###############################################################
  
  
  ########################## GENES ##########################
  
  modified_symbol <- biomarkersServer("genes1", parent_session = session)
  
  observeEvent(modified_symbol(), {
    dt$Symbol <- modified_symbol()
    updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
  })

  
  conditionServer("diseases1")
  

  
  summaryServer("summary1")
  

  
  measurementsServer("measurements1")
  
  
  
  publicationServer("publications1")
  
  
  ###############################################################
  ############### BUTTONS FOR RELOAD DATA #######################
  ###############################################################
  
  observeEvent(input$reload1, {
    dt$Symbol <- NULL
    dt$DiseaseId <- NULL
    dt$gd_symbol <- NULL
    dt$dt_gds <- pool %>%
      tbl("gene_disease_summary") %>% 
      collect()
  })
  
  observeEvent(input$reload2, {
    dt$dt_gd <- NULL
    query <- "select gd.*, g.symbol, d.name
            from gene_disease as gd
            left join genes as g
            on gd.geneid = g.geneid
            left join diseases as d
            on gd.diseaseid = d.diseaseid"
    dt$dt_gd <- pool %>%
      dbGetQuery(query) %>%
      collect()
  })
  
  observeEvent(input$reload3, {
    dt$dt_publications <- NULL
    dt$dt_publications <- pool %>%
      tbl("publications") %>% 
      collect() 
  })

  
  ###############################################################
  
  ### Redirect Header title to "About" page  
  
  observeEvent(input$header_title, {
    updateNavbarPage(inputId = "navbarPage", selected = "about")
  })
  
  
  addResourcePath("tmpuser", getwd())
  output$homeWeb <- renderUI({
    tags$iframe(seamless = "seamless",
                frameborder = "0",
                src = "tmpuser/rmarkdown/index.html",
                width = "100%",
                height = 800)
  })
  

  textServer("text1", reactive(input$gd_data))

  
  observeEvent(req(input$biomarker), {
    dt$Biomarker <- input$biomarker
    reloadData(gene_diseaseProxy)
    updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
  })
  
}

