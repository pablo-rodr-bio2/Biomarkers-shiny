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
                       Biomarker = NULL,
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
        query <- "select gd.*, g.symbol, d.name, s.year
            from gene_disease as gd
            left join genes as g
            on gd.geneid = g.geneid
            left join diseases as d
            on gd.diseaseid = d.diseaseid
            left join studies as s
            on gd.nctid = s.nctid"
        dt$dt_gd <- pool %>%
          dbGetQuery(query) %>%
          collect() %>% 
          filter( if( !is.null(dt$gd_symbol) ) symbol == dt$gd_symbol else TRUE ) %>%
          filter( if( !is.null(dt$gd_name) ) name == dt$gd_name else TRUE ) %>% 
          filter( if( !is.null(dt$Biomarker) ) bmtype == dt$Biomarker else TRUE )
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
    options = list(scrollX = TRUE, dom = 'ltipr', pageLength = 10,
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
      select(Gene, Condition, Fisher, OddsRatio,
             `Num. Clin.Trials`,   `Num. Pmids`,
             `year initial`, `year final`) %>%
      mutate(`Num. Clin.Trials` = createLink_Button(`Num. Clin.Trials`),
             Fisher = formatC(Fisher, format="e", digits=2, zero.print = TRUE))
  },filter = "top",
  options = list(dom = 'ltipr',
                 columnDefs = list(list(className = 'dt-center', targets ="_all"))
                 ),
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
    dt$gd_symbol <- input$gds_data[1]
    dt$gd_name <- input$gds_data[2]
    if(col == "4"){
      updateNavbarPage( inputId = "navbarPage", selected = "gene_disease")
    } 
  })
  
  #### HEATMAP ####
  output$heatmap_gds <- renderPlot({
    
    df <- dt$dt_gds %>%
      filter(nclinicaltrials> 1) %>%
      filter( if( !is.null(dt$gd_symbol) ) symbol == dt$gd_symbol else TRUE ) %>% 
      arrange(desc(nclinicaltrials)) %>%
      head(50)
    
    df <- df %>%
      mutate(disease=factor(name, levels=rev(sort(unique(name))))) %>%
      mutate(countfactor=cut(nclinicaltrials, breaks=c(  0, 1, 10, 100, 200,  max(nclinicaltrials, na.rm=T)),
                             labels=c(  "0-1", "1-10", "10-100", "100-200", ">400"))) %>%
      # change level order
      mutate(countfactor=factor(as.character(countfactor), levels=rev(levels(countfactor))))  

    textcol <- "grey40"
    
    plot <-  ggplot(df, aes(x=symbol, y=disease, fill=countfactor))+
      geom_tile(colour="white", size=0.2)+
      guides(fill=guide_legend(title="Number of\nClinical Trials"))+
      labs(x="", y="", title="Enriched liver biomarkers")+
      scale_y_discrete(expand=c(0, 0))+
      # scale_x_discrete(expand=c(0, 0), breaks=c("1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000"))+
      scale_fill_manual(values=c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#ddf1da"), na.value = "grey90")+
      #coord_fixed()+
      theme_grey(base_size=10)+
      theme(legend.position="right", legend.direction="vertical",
            legend.title=element_text(colour=textcol),
            legend.margin=margin(grid::unit(0, "cm")),
            legend.text=element_text(colour=textcol, size=7, face="bold"),
            legend.key.height=grid::unit(0.8, "cm"),
            legend.key.width=grid::unit(0.2, "cm"),
            axis.text.x=element_text(size=10, colour=textcol),
            axis.text.y=element_text(vjust=0.2, colour=textcol),
            axis.ticks=element_line(size=0.4),
            plot.background=element_blank(),
            panel.border=element_blank(),
            plot.margin=margin(0.7, 0.4, 0.1, 0.2, "cm"),
            plot.title=element_text(colour=textcol, hjust=0, size=12, face="bold")
      ) + theme_bw()
    
    # plot <-plot + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6))  
    plot <-plot + theme(axis.text.x = element_text(  size = 6))  
    
    plot
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
  rownames = FALSE,
  callback = JS("table.on('click.dt', 'tr',
                  function() {
                    var data = table.rows(this).data().toArray();
                    Shiny.setInputValue('gd_data', data, {priority: 'event'});
                  });"))
  
  gene_diseaseProxy <- dataTableProxy("gene_disease")
  
  observeEvent(req(input$gd_data), {
      value <- input$gd_data[2]
      dt$Symbol <- dt$dt_genes %>% 
        filter(Gene == value) %>% 
        select(geneid) %>% 
        as.character
  })
  
  # output$gd_plot <- renderPlot({
  #   gene <- "HBB"
  #   df <- dt$dt_gd %>% 
  #     filter(symbol == gene) %>% 
  #     unique() %>% 
  #     group_by(name) %>%
  #     mutate(MinYear = min(year, na.rm = T)) %>%
  #     select(-year) %>%
  #     unique()
  # 
  #   tt <- df %>%  group_by( diseaseid  ) %>%   summarise(nclinicaltrials = n_distinct(nctid)) %>%
  #     arrange(desc(nclinicaltrials)) %>% head(50)
  # 
  #   df <- merge(df, tt, by = c("diseaseid"))
  #   df <- df %>% select(-nctid) %>% unique() %>%  arrange(MinYear)
  # 
  #   df$id <- c(1:nrow(df))
  #   df$label <- ifelse(nchar(df$name)> 20, paste0(substring(df$name, 1,20), "..."), as.character(df$name))
  #   # mutate(disease=factor(name, levels=rev(sort(unique(name)))))
  #   p<- ggplot(df, aes(x= as.Date(ISOdate(MinYear, 1, 1)), y= id, size = nclinicaltrials, label=name)) +
  #     geom_point(aes(fill="blue", alpha =0.5),  pch=21 )   +
  #     geom_text_repel(aes(label = label),
  #                     size = 3.5, max.overlaps = 20) +theme_bw(base_size = 12) 
  #   +
  #     scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  #     ylab(paste0("Diseases for ", gene)) + xlab("Year of 1st trial")
  #   +
  #     labs(size = "N Cts")  + theme(legend.position="none") +
  #     scale_shape(guide="none") +
  #     scale_fill_discrete(guide="none")+
  #     theme(plot.title = element_text(size = 9, face = "bold"),
  #           legend.title=element_text(size=9),
  #           legend.text=element_text(size=9))
  # 
  #   p
  # })
  
  
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
  
  output$home_plot <- renderUI({
    tagList(
      h1("Biomarkers in Clinical Trials"),
      div(id="g1745",
          img(src="g1745.png")),
      p("The numbers of clinical trials are now rising in an unprecedented manner.
        The data they contain can be exploited to extract information about what human
        diseases are investigated and which are molecular biomarkers used in this massive 
        pool of studies. In this contribution, we have applied artificial intelligent 
        technologies to explore protein and gene biomarkers more frequently assessed in
        Clinical Trials data. We found over 4,300 genes measured in over 55,000 Clinical 
        Trials involving 3,000 diseases."),
      plotlyOutput("plot_summary")
    )
  })
  
  output$plot_summary <- renderPlotly({
    data <- readRDS("data_for_plots.rds")
    p <- ggplot(data, aes(`Biomarker Type`, percent, fill = factor(`Biomarker Type`))) +
      geom_col() + theme_bw() + xlab("Biomarker Type") +theme(legend.position="none")
    plotly::ggplotly(p, source="click1", tooltip = c("Biomarker Type", "percent")) %>%
      htmlwidgets::onRender(
        "function(el) {
         el.on('plotly_click', function(d) {
            console.log(d.points[0].data.name);
            var data = d.points[0].data.name;
            Shiny.setInputValue('biomarker', data, {priority: 'event'});
         })
       }"
      )
  })
  

  
  output$text <- renderPrint({
    input$gd_data
  })
  
  observeEvent(req(input$biomarker), {
    dt$Biomarker <- input$biomarker
    reloadData(gene_diseaseProxy)
    updateNavbarPage(inputId = "navbarPage", selected = "gene_disease")
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