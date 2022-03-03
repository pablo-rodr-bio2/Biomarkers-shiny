function(input, output, session) {

  observeEvent(input$header_title, {
    updateNavbarPage(inputId = "navbarPage", selected = "about")
  })
  
  observeEvent(input$reload1, {
    dt$Symbol <- NULL
    dt$DiseaseId <- NULL
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
  
  
  dt <- reactiveValues(dt_genes = NULL,
                       dt_diseases = NULL,
                       dt_studies = NULL,
                       dt_gds = NULL,
                       dt_gd = NULL,
                       dt_publications = NULL,
                       Symbol = NULL,
                       DiseaseId = NULL)
  
  observe({
    
    switch (req(input$navbarPage),
      "genes" = {
        dt$dt_genes <- pool %>%
        tbl("genes") %>% 
        collect() %>% 
          rename("type of gene" = type_of_gene, "GeneId" = geneid) %>% 
          select(-GeneId)
        },
      
      "diseases" = {
        dt$dt_diseases <- pool %>% 
          tbl("diseases") %>% 
          collect()
      },
      
      "studies" = {
        dt$dt_studies <- pool %>% 
          tbl("studies") %>% 
          collect()
      },
      
      "gene_disease_summary" = {
        dt$dt_gds <- pool %>%
          tbl("gene_disease_summary") %>% 
          collect() %>% 
          filter( if( !is.null(dt$Symbol) ) symbol == dt$Symbol else TRUE ) %>% 
          filter( if( !is.null(dt$DiseaseId) ) diseaseid == dt$DiseaseId else TRUE )
        },
      
      "gene_disease" = {
        query <- "select gd.*, g.symbol, d.name
            from gene_disease as gd
            left join genes as g
            on gd.geneid = g.geneid
            left join diseases as d
            on gd.diseaseid = d.diseaseid"
        dt$dt_gd <- pool %>%
          dbGetQuery(query) %>%
          collect() %>% 
          filter( if( !is.null(dt$Symbol) ) symbol == dt$Symbol else TRUE ) %>% 
          filter( if( !is.null(dt$DiseaseId) ) diseaseid == dt$DiseaseId else TRUE )
      },
      
      "publications" = {
        dt$dt_publications <- pool %>% 
          tbl("publications") %>% 
          collect()
      }
    )
  })

  
  output$genes <- DT::renderDataTable({
    dt$dt_genes %>% 
      mutate(symbol = createLink_Button(symbol))
    }, 
    filter = "top",
    options = list(scrollX = TRUE, dom = 'ltipr'),
    rownames = FALSE,
    escape = FALSE,
    selection = list(mode = 'single', target = 'cell'))
  
  
  geneProxy <- dataTableProxy('genes')
  
  observeEvent(input$genes_cells_selected, {
    value <- input$genes_cells_selected
    geneProxy %>%  selectCells(NULL)
    if(length(value > 1)) {
      col <- value[,2]
      if(col == "0"){
        value[,2] <- value[,2] + 1
        ## very awful but i refuse to convert tibble to data.frame just for this
        dt$Symbol <- dt$dt_genes[value[,1], value[,2], drop = TRUE]
        updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
      }
    }
  })
  
  output$diseases <- DT::renderDataTable({
    dt$dt_diseases %>% 
      mutate(diseaseid = createLink_Button(diseaseid))
  },
  filter = "top",
  selection = list(mode = 'single', target = 'cell'),
  options = list(dom = 'ltipr'),
  rownames = FALSE,
  escape = FALSE)
  
  diseaseProxy <- dataTableProxy('diseases')
  
  observeEvent(input$diseases_cells_selected, {
    value <- input$diseases_cells_selected
    diseaseProxy %>% selectCells(NULL)
    if(length(value > 1)) {
      col <- value[,2]
      if(col == "0"){
        value[,2] <- value[,2] + 1
        dt$DiseaseId <- dt$dt_diseases[value[,1], value[,2], drop = TRUE]
        updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
      }
    }
  })
  
  output$studies <- DT::renderDataTable({
    dt$dt_studies
  }, filter = "top",
  options = list(dom = 'ltipr'),
  rownames = FALSE)
  
  output$gene_disease_summary <- DT::renderDataTable({
    dt$dt_gds %>%
      mutate(nclinicaltrials = createLink_Button(nclinicaltrials))
  }, filter = "top",
  options = list(dom = 'ltipr'),
  selection = list(mode = 'single', target = 'cell'),
  escape = FALSE,
  rownames = FALSE)
  
  observeEvent(input$gene_disease_summary_cells_selected, {
    value <- input$gene_disease_summary_cells_selected
    if(length(value)>1) {
      col <- value[,2]
      if(col == "4"){
        rows <- value[,1]
        cols <- value[,2] + 1
        dt$Symbol <- dt$dt_gds[rows,3, drop = TRUE]
        dt$DiseaseId <- dt$dt_gds[rows, 2, drop = TRUE]
        updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
      }
    }
  })
  
  output$gene_disease <- DT::renderDataTable({
    dt$dt_gd %>%
      mutate(symbol = createLink_Symbol(geneid, symbol),
             name = createLink_Name(diseaseid, name)) %>%
      select(-id, -sentenceHtml, -geneid, -diseaseid) %>%
      relocate(symbol, .after=nctid) %>%
      relocate(name, .after=symbol)
  },
  escape = FALSE,
  filter = "top",
  options = list(dom = 'ltipr'),
  rownames = FALSE) 
  
  output$publications <- DT::renderDataTable({
    dt$dt_publications
  }, filter = "top",
  options = list(dom = 'ltipr'),
  rownames = FALSE)
  
}