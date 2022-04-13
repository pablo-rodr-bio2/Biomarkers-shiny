function(input, output, session) {
  
  ###############################################################
  ################# REACTIVE VALUES #############################
  ###############################################################
  
  dt <- reactiveValues(GeneId = NULL,
                       DiseaseId = NULL,
                       Biomarker = NULL,
                       gd_geneId = NULL,
                       gd_diseaseId = NULL)

  ###############################################################
  ###################### TABLES #################################
  ###############################################################
  
  
  ########################## GENES ##########################
  
  geneSelected <- biomarkersServer("genes1")
  
  observeEvent(geneSelected(), {
    dt$GeneId <- geneSelected()$geneId
    colName <- geneSelected()$colName
    if (colName %in% c("Gene", "Num. Diseases")) {
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
    }
    if (colName == "Num. Clin.Trials") {
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
    }
    if (colName == "Num. Pmids") {
      updateNavbarPage(inputId = "navbarPage", selected = "publications")
    }
  })

  
  diseaseSelected <- conditionServer("diseases1")
  
  observeEvent(diseaseSelected(), {
    dt$DiseaseId <- diseaseSelected()$diseaseId
    colName <- diseaseSelected()$colName
    if (colName %in% c("Condition", "Num. Biomarkers")) {
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
    }
    if (colName == "Num. Clin.Trials") {
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
    }
    if (colName == "Num. Pmids") {
      updateNavbarPage(inputId = "navbarPage", selected = "publications")
    }
  })
  

  summarySelected <- summaryServer("summary1", reactive(dt$GeneId), reactive(dt$DiseaseId))
  
  observeEvent(summarySelected(), {
    dt$gd_geneId <- summarySelected()$geneId
    dt$gd_diseaseId <- summarySelected()$diseaseId
    colName <- summarySelected()$colName
    if (colName == "Num. Clin.Trials"){
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
    }
    if( colName == "Num. Pmids"){
      updateNavbarPage(inputId = "navbarPage", selected = "publications")
    }
  })
  
  measurementsServer("measurements1", reactive(dt$GeneId), reactive(dt$DiseaseId), 
                     reactive(dt$gd_geneId), reactive(dt$gd_diseaseId))
  
  
  publicationServer("publications1", reactive(dt$GeneId), reactive(dt$DiseaseId))
  
  
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
  

  # textServer("text1", summarySelected())

  
  observeEvent(req(input$biomarker), {
    dt$Biomarker <- input$biomarker
    reloadData(gene_diseaseProxy)
    updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
  })
  
}

