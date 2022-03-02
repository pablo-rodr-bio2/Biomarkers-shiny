function(input, output, session) {
  
  observeEvent(input$header_title, {
    updateNavbarPage(inputId = "navbarPage", selected = "about")
  })
  
  observeEvent(input$reload1, {
    dt_gds(loadData("gene_disease_summary"))
  })
  
  observeEvent(input$reload2, {
    dt_gd(NULL)
    dt_gd(gene_disease_loadData())
  })
  
  geneid <- reactiveVal(NULL)
  
  
  dt_genes <- loadData("genes")
  dt_diseases <- loadData("diseases")
  
  dt_gds <- reactiveVal(loadData("gene_disease_summary"))
  dt_gd <- reactiveVal(gene_disease_loadData())
  
  output$genes <- DT::renderDataTable({
    DT::datatable(dt_genes %>% 
                    mutate(symbol = createLink_Button(symbol)),
                  filter = "top",
                  options=list(scrollX = TRUE, dom = 'ltipr'),
                  rownames = FALSE,
                  selection = list(mode = 'single', target = 'cell'),
                  escape = FALSE)
  })
  
  observeEvent(input$genes_cells_selected, {
    value <- input$genes_cells_selected
    if(length(value > 1)) {
      col <- value[,2]
      if(col == "1"){
        value[,2] <- value[,2] + 1
        updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
        Symbol <- dt_genes[value]
        dt_gds(dt_gds() %>% 
          filter(symbol == Symbol))
        output$gene_disease_summary <- DT::renderDataTable({
          dt_gds() %>%
            mutate(nclinicaltrials = createLink_Button(nclinicaltrials))
        }, filter = "top",
        options = list(dom = 'ltipr'),
        selection = list(mode = 'single', target = 'cell'),
        escape = FALSE,
        rownames = FALSE)
      }
    }
  })
  
  output$diseases <- DT::renderDataTable({
    dt_diseases %>% 
      mutate(diseaseid = createLink_Button(diseaseid))
  }, filter = "top",
  selection = list(mode = 'single', target = 'cell'),
  options = list(dom = 'ltipr'),
  rownames = FALSE,
  escape = FALSE)
  
  observeEvent(input$diseases_cells_selected, {
    value <- input$diseases_cells_selected
    if(length(value > 1)) {
      col <- value[,2]
      if(col == "0"){
        value[,2] <- value[,2] + 1
        updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
        diseaseId <- dt_diseases[value]
        dt_gds(dt_gds() %>% 
          filter(diseaseid == diseaseId))
        output$gene_disease_summary <- DT::renderDataTable({
          dt_gds() %>%
            mutate(nclinicaltrials = createLink_Button(nclinicaltrials))
        }, filter = "top",
        options = list(dom = 'ltipr'),
        selection = list(mode = 'single', target = 'cell'),
        escape = FALSE,
        rownames = FALSE)
      }
    }
  })
  
  output$studies <- DT::renderDataTable({
    loadData("studies")
  }, filter = "top",
  options = list(dom = 'ltipr'),
  rownames = FALSE)
  
  output$gene_disease_summary <- DT::renderDataTable({
    dt_gds() %>%
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
        updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
        rows <- value[,1]
        cols <- value[,2] + 1
        gene_id <- dt_gds()[rows,3]
        disease_id <- dt_gds()[rows, 2]
        dt_gd( dt_gd() %>%
          filter(symbol == gene_id) %>%
          filter(diseaseid == disease_id) )
        output$gene_disease <- DT::renderDataTable({
          dt_gd() %>% 
            mutate(symbol = createLink_Symbol(geneid, symbol),
                   name = createLink_Name(diseaseid, name)) %>% 
            select(-id, -sentenceHtml, -geneid, -diseaseid) %>%
            relocate(symbol, .after=nctid) %>%
            relocate(name, .after=symbol) %>%
            DT::datatable(escape=FALSE,
                          filter = "top",
                          options = list(dom = 'ltipr'),
                          rownames = FALSE)})
      }
    }
  })
  
  output$gene_disease <- DT::renderDataTable({
    dt_gd() %>%
      mutate(symbol = createLink_Symbol(geneid, symbol),
             name = createLink_Name(diseaseid, name)) %>% 
      select(-id, -sentenceHtml, -geneid, -diseaseid) %>%
      relocate(symbol, .after=nctid) %>%
      relocate(name, .after=symbol) %>%
      DT::datatable(escape=FALSE,
                    filter = "top",
                    options = list(dom = 'ltipr'),
                    rownames = FALSE)
  }) 
  
  output$publications <- DT::renderDataTable({
    loadData("publications")
  }, filter = "top",
  options = list(dom = 'ltipr'),
  rownames = FALSE)
  
}