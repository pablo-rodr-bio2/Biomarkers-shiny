function(input, output, session) {
  
  ###############################################################
  ################# REACTIVE VALUES #############################
  ###############################################################
  
  dt <- reactiveValues(dt_genes = NULL,
                       dt_diseases = NULL,
                       dt_studies = NULL,
                       dt_gds = NULL,
                       dt_gd = NULL,
                       dt_publications = NULL,
                       Symbol = NULL,
                       DiseaseId = NULL,
                       gd_symbol = NULL,
                       gd_name = NULL)
  
  ###############################################################
  
  
  ###############################################################
  ################# MAIN QUERIES ################################
  ###############################################################
  
  ### everytime we click on a tabSet, it will query the MySQL DB
  # It will also do some pre data-treatment
  # 'gene-disease' will also be filtered by geneid and diseaseid
  # in order to decrease loading time
  
  observe({
    
    switch (req(input$navbarPage),
      "genes" = {
        dt$dt_genes <- pool %>%
        tbl("genes") %>% 
        collect() %>% 
          mutate(Gene = createLink_Symbol(geneid, symbol))
        },
      
      "diseases" = {
        dt$dt_diseases <- pool %>% 
          tbl("diseases") %>% 
          collect() %>%
          mutate( Condition = createLink_Name(diseaseid, name)) 
      },
      
      "gene_disease_summary" = {
        dt$dt_gds <- pool %>%
          tbl("gene_disease_summary") %>% 
          collect() 
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
          filter( if( !is.null(dt$gd_symbol) ) symbol == dt$gd_symbol else TRUE ) %>%
          filter( if( !is.null(dt$gd_name) ) name == dt$gd_name else TRUE )
      },
      
      "publications" = {
        dt$dt_publications <- pool %>%
          tbl("publications") %>% 
          collect() 
      }
    )
  })

  ###############################################################
  ###################### TABLES #################################
  ###############################################################
  
  
  ########################## GENES ##########################
  
  output$genes <- DT::renderDataTable({
    dt$dt_genes %>%
      rename( "type of gene" = type_of_gene,
              "protein class" = PROTEIN_CLASS_NAMES,
              "DPI" = dpi, "DSI" = dsi, "pLI"=pli,
              "year initial" = year_initial, "year final" = year_final, 
              "Num. Clin.Trials" =  nclinicaltrials, 
              "Num. Diseases" = ndiseases, "Num. Pmids" = npmids)  %>% 
      select(Gene, description, `type of gene`, DSI, DPI, pLI,`protein class`,
             `Num. Clin.Trials`, `Num. Diseases`, `Num. Pmids`,
             `year initial`, `year final`
      ) %>%  arrange(desc(`Num. Clin.Trials`) ) %>% 
      mutate(`Num. Clin.Trials` = createLink_Button(`Num. Clin.Trials`),
             `Num. Diseases`= createLink_Button(`Num. Diseases`),
             `Num. Pmids` = createLink_Button(`Num. Pmids`))
    }, 
    filter = "top",
    options = list(scrollX = TRUE, dom = 'ltipr', pageLength = 5,
                   columnDefs = list(list(className = 'dt-center', targets ="_all"))),
    rownames = FALSE,
    escape = FALSE,
    selection = list(mode = 'single', target = 'cell'),
    callback = JS("table.on('click.dt', 'tr',
                  function() {
                    data = table.rows(this).data().toArray();
                    Shiny.setInputValue('geneid', data, {priority: 'event'});
                  });")
    )
  
  geneProxy <- dataTableProxy('genes')
  
  observeEvent(req(input$geneid), {
    cell_clicked <- input$genes_cell_clicked
    geneProxy %>%  selectCells(NULL)
    if(cell_clicked$col == 0){                ###### redirects to 'Summary'
      value <- cell_clicked$value
      dt$Symbol <- dt$dt_genes %>% 
        filter(Gene == value) %>% 
        select(geneid) %>% 
        as.character
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
    }
    if(cell_clicked$col == 7){                ####### redirects to 'Measurements'
      value <- input$geneid[1]
      geneProxy %>%  selectCells(NULL)
      dt$gd_symbol <- dt$dt_genes %>% 
        filter(Gene == value) %>% 
        select(symbol) %>% 
        as.character
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
    }
    if(cell_clicked$col == 8){                ####### redirects to 'Summary'
      value <- input$geneid[1]
      geneProxy %>%  selectCells(NULL)
      dt$Symbol <- dt$dt_genes %>%
        filter(Gene == value) %>%
        select(geneid) %>%
        as.character
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
    }
    if(cell_clicked$col == 9){                ####### redirects to 'Publications'
      value <- input$geneid[1]
      geneProxy %>%  selectCells(NULL)
      dt$Symbol <- dt$dt_genes %>%
        filter(Gene == value) %>%
        select(geneid) %>%
        as.character
      updateNavbarPage(inputId = "navbarPage", selected = "publications")
    }
  })
  
  
  ########################## DISEASES ##########################
  
  output$diseases <- DT::renderDataTable({
    dt$dt_diseases %>%
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
  },
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
  
  diseaseProxy <- dataTableProxy('diseases')
  
  observeEvent(input$diseaseid, {
    cell_clicked <- input$diseases_cell_clicked
    diseaseProxy %>% selectCells(NULL)
    if(cell_clicked$col == 0){
      value <- cell_clicked$value
      dt$DiseaseId <- dt$dt_diseases %>% 
        filter(Condition == value) %>% 
        select(diseaseid) %>% 
        as.character()
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
    }
    if(cell_clicked$col == 2){
      value <- input$diseaseid[1]
      dt$DiseaseId <- dt$dt_diseases %>%
        filter(Condition == value) %>%
        select(diseaseid) %>%
        as.character()
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease_summary")
    }
    if(cell_clicked$col == 3){
      value <- input$diseaseid[1]
      dt$gd_name <- dt$dt_diseases %>% 
        filter(Condition == value) %>%
        select(name) %>%
        as.character()
      updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
    }
    if(cell_clicked$col == 4){
      value <- input$diseaseid[1]
      dt$DiseaseId <- dt$dt_diseases %>% 
        filter(Condition == value) %>%
        select(diseaseid) %>%
        as.character()
      str(dt$DiseaseId)
      updateNavbarPage(inputId = "navbarPage", selected = "publications")
    }
  })


  
  ########################## GENE-DISEASE SUMMARY ##########################
  
  output$gene_disease_summary <- DT::renderDataTable({
    dt$dt_gds %>% 
      filter( if( !is.null(dt$Symbol) ) geneid == dt$Symbol else TRUE ) %>% 
      filter( if( !is.null(dt$DiseaseId) ) diseaseid == dt$DiseaseId else TRUE ) %>%
      select(-geneid, -diseaseid, -id) %>% 
      rename( "Gene" = symbol, "Condition" = name,  
              "year initial" = year_initial, "year final" = year_final, 
              "Num. Clin.Trials" =  nclinicaltrials, 
              "Num. Pmids" = npmids)  %>% 
      arrange(desc(`Num. Clin.Trials`) ) %>% 
      select(Gene, Condition, tfidf, OddsRatio,
             `Num. Clin.Trials`,   `Num. Pmids`,
             `year initial`, `year final`) %>%
      mutate(`Num. Clin.Trials` = createLink_Button(`Num. Clin.Trials`)) 
  }, filter = "top",
  options = list(dom = 'ltipr',
                 columnDefs = list(list(className = 'dt-center', targets ="_all"))),
  selection = list(mode = 'single', target = 'cell'),
  escape = FALSE,
  rownames = FALSE,
  callback = JS("table.on('click.dt', 'tr',
                  function() {
                    var data = table.rows(this).data().toArray();
                    Shiny.setInputValue('gds_data', data, {priority: 'event'});
                  });")
  )

  gdsProxy <- dataTableProxy("gene_disease_summary")
  
  observeEvent(input$gds_data, {
    value <- input$gene_disease_summary_cell_clicked
    gdsProxy %>% selectCells(NULL)
    col <- value$col
    if(col == "4"){
      dt$gd_symbol <- input$gds_data[1]
      dt$gd_name <- input$gds_data[2]
      updateNavbarPage( inputId = "navbarPage", selected = "gene_disease")
    }
  })
  
  
  ########################## GENE-DISEASE ##########################
  
  output$gene_disease <- DT::renderDataTable({
    dt$dt_gd %>%
      mutate(symbol = createLink_Symbol(geneid, symbol),
             nctid =   createLink_NCIT(nctid),
           #  pmid =   createLink_PMID(pmid),
             name = createLink_Name(diseaseid, name)) %>%
      select(-id, -sentenceHtml, -geneid, -diseaseid) %>%
      relocate(symbol, .after=nctid) %>%
      relocate(name, .after=symbol) %>% 
      rename("Gene" = symbol, "Condition" = name, 
             "NCT ID" = nctid, "Measurement" = sentence, 
             "Num. Pmids" = npmids,
             "Biomarker Type" = bmtype ) %>%  arrange(desc(`Num. Pmids`) )
      
  },
  escape = FALSE,
  filter = "top",
  options = list(dom = 'ltipr',
                 columnDefs = list(list(className = 'dt-center', targets ="_all"))),
  rownames = FALSE)
  
  
  ########################## PUBLICATIONS ##########################
  
  output$publications <- DT::renderDataTable({
    dt$dt_publications %>%
      filter( if( !is.null(dt$Symbol) ) geneid == dt$Symbol else TRUE ) %>% 
      filter( if( !is.null(dt$DiseaseId) ) diseaseid == dt$DiseaseId else TRUE ) %>% 
      mutate(nctid =   createLink_NCIT(nctid),
              pmid =   createLink_PMID(pmid)) %>%
      select(-id) %>%
      rename("NCT ID" = nctid) %>%
      arrange(desc(year))
    
  }, filter = "top",
  options = list(dom = 'ltipr',
                 columnDefs = list(list(className = 'dt-center', targets ="_all"))),
  selection = list(mode = 'single', target = 'cell'),
  escape = FALSE,
  rownames = FALSE)  
  
  
  ###############################################################
  ############### BUTTONS FOR RELOAD DATA #######################
  ###############################################################
  
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
  
  output$plot1 <- renderPlot({
    data <- readRDS("data_for_plots.rds")
    p <- ggplot(data, aes(`Biomarker Type`, percent, fill = `Biomarker Type`)) +
      geom_col() + theme_bw() + xlab("Biomarker Type") +theme(legend.position="none")
    p
    # plotly::ggplotly(p)
  })
  
}


# LEGACY CODE

# "studies" = {
#   dt$dt_studies <- pool %>% 
#     tbl("studies") %>% 
#     collect() 
#   },

# "publications" = {
#   query <- "select p.*, g.symbol, d.name
#             from publications as p
#             left join genes as g
#             on p.geneid = g.geneid
#             left join diseases as d
#             on p.mesh = d.diseaseid"
#   dt$dt_publications <- pool %>% 
#     dbGetQuery(query) %>% 
#     collect()
# }
# 
# ########################## STUDIES ##########################
# 
# output$studies <- DT::renderDataTable({
#   dt$dt_studies %>% 
#     rename(   "NCT ID" = nctid, "Brief Title" = brief_title, 
#               "Official Title" = official_title,  
#               "Num. GDAS" = ngdas, "Num. Genes" = ngenes, "Num. Diseases" = ndiseases,
#               "Num Pmids" = npmids, "Study Type" = study_type       )  %>% 
#     select("NCT ID" , "Brief Title",  "Official Title", "Study Type", "Official Title", 
#            "Num. GDAS", "Num. Genes", "Num. Diseases", "Num Pmids") %>%
#     arrange(desc(`Num. GDAS`) ) %>%
#     mutate(`NCT ID` = createLink_NCIT(`NCT ID`))
# }, filter = "top",
# options = list(scrollX = TRUE, dom = 'ltipr', pageLength = 10,
#                columnDefs = list(list(className = 'dt-center', targets ="_all"))),
# rownames = FALSE,
# escape = FALSE,
# selection = list(mode = 'single', target = 'cell'))