measurementsUI <- function(id){
  ns <- NS(id)
  withLoader(DTOutput(ns("gene_disease")))
}

measurementsServer <- function(id, geneId, diseaseId, gd_gene, gd_disease){
  moduleServer(id, function(input, output, session){
    
    ### Query
    measurements <- reactive({
      query <- "select gd.*, g.symbol, d.name, s.year
            from gene_disease as gd
            left join genes as g
            on gd.geneid = g.geneid
            left join diseases as d
            on gd.diseaseid = d.diseaseid
            left join studies as s
            on gd.nctid = s.nctid"
      pool %>%
        dbGetQuery(query) %>%
        collect() 
      # %>% 
      #   filter( if( !is.null(dt$gd_symbol) ) symbol == dt$gd_symbol else TRUE ) %>%
      #   filter( if( !is.null(dt$gd_name) ) name == dt$gd_name else TRUE ) %>% 
      #   filter( if( !is.null(dt$Biomarker) ) bmtype == dt$Biomarker else TRUE )
    })
    
    ### Format data
    data <- reactive({
      measurements() %>%
        filter( if( !is.null(geneId()) ) geneid == geneId() else TRUE ) %>%
        filter( if( !is.null(diseaseId()) ) diseaseid == diseaseId() else TRUE ) %>%
        filter( if( !is.null(gd_gene()) ) symbol == gd_gene() else TRUE ) %>% 
        filter( if( !is.null(gd_disease()) ) name == gd_disease() else TRUE ) %>%
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
    })

    ### Produce table
    output$gene_disease <- renderDataTable(
      data(),
      escape = FALSE,
      filter = "top",
      options = list(dom = 'ltipr',
                     columnDefs = list(list(className = 'dt-center', targets ="_all"))),
      rownames = FALSE,
      callback = JS("table.on('click.dt', 'tr',
                        function() {
                          var data = table.rows(this).data().toArray();
                          Shiny.setInputValue('gd_data', data, {priority: 'event'});
                        });")
      )
  })
}

# 
# gene_diseaseProxy <- dataTableProxy("gene_disease")
# 
# # observeEvent(req(input$gd_data), {
# #     value <- input$gd_data[2]
# #     dt$Symbol <- dt$dt_genes %>% 
# #       filter(Gene == value) %>% 
# #       select(geneid) %>% 
# #       as.character
# # })