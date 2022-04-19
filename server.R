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
  
  
  ########################## BIOMARKERS ##########################
  
  geneSelected <- biomarkersServer("genes1")
  
  observeEvent(geneSelected(), {
    colName <- geneSelected()$colName
    if (colName %in% c("Gene", "Num. Diseases")) {
      dt$GeneId <- geneSelected()$geneId
      dt$DiseaseId <- NULL
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
    }
    if (colName == "Num. Clin.Trials") {
      dt$gd_geneId <- geneSelected()$geneId
      dt$gd_diseaseId <- NULL
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
    }
    if (colName == "Num. Pmids") {
      dt$GeneId <- geneSelected()$geneId
      dt$DiseaseId <- NULL
      updateNavbarPage(inputId = "navbarPage", selected = "publications")
    }
  })

  ########################## CONDITIONS  ########################## 
  
  diseaseSelected <- conditionServer("diseases1")
  
  observeEvent(diseaseSelected(), {
    colName <- diseaseSelected()$colName
    if (colName %in% c("Condition", "Num. Biomarkers")) {
      dt$DiseaseId <- diseaseSelected()$diseaseId
      dt$GeneId <- NULL
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
    }
    if (colName == "Num. Clin.Trials") {
      dt$gd_diseaseId <- diseaseSelected()$diseaseId
      dt$gd_geneId <- NULL
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
    }
    if (colName == "Num. Pmids") {
      dt$DiseaseId <- diseaseSelected()$diseaseId
      dt$GeneId <- NULL
      updateNavbarPage(inputId = "navbarPage", selected = "publications")
    }
  })
  
  
  ########################## SUMMARY  ########################## 
  
  summarySelected <- summaryServer("summary1", reactive(dt$GeneId), reactive(dt$DiseaseId))
  
  observeEvent(summarySelected(), {
    colName <- summarySelected()$colName
    dt$gd_geneId <- summarySelected()$geneId
    dt$gd_diseaseId <- summarySelected()$diseaseId
    if (colName == "Num. Clin.Trials"){
      dt$gd_geneId <- summarySelected()$geneId
      dt$gd_diseaseId <- summarySelected()$diseaseId
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
    }
    if( colName == "Num. Pmids"){
      dt$GeneId <- summarySelected()$geneId
      dt$DiseaseId <- summarySelected()$diseaseId
      updateNavbarPage(inputId = "navbarPage", selected = "publications")
    }
  })
  
  ########################## MEASUREMENTS  ########################## 
  
  measurementSelected <- measurementsServer("measurements1", reactive(dt$gd_geneId), reactive(dt$gd_diseaseId))
  
  observeEvent(measurementSelected(), {
    dt$GeneId <- measurementSelected()$geneId
    dt$DiseaseId <- measurementSelected()$diseaseId
    updateNavbarPage(inputId = "navbarPage", selected = "publications")
  })
  
  
  ########################## PUBLICATIONS  ########################## 
  
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
                style='width:100%; height:90vh;')
  })
  
  # textServer("text1", summarySelected())

  
  observeEvent(req(input$biomarker), {
    dt$Biomarker <- input$biomarker
    reloadData(gene_diseaseProxy)
    updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
  })
  
}

