measurementsUI <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("reload"), "Reload Data", class="btn-primary"),
    hr(),
    fluidRow(
      column(12, uiOutput(ns("measurements_plots")))
      # column(6, 
      #        withLoader(DTOutput(ns("geneDisease")))
      # ),
      # column(6,
      #        fluidRow(
      #          column(12, plotlyOutput(ns("plot1"), height = "40vh"))
      #        ),
      #        fluidRow(
      #          column(12, plotlyOutput(ns("plot2"), height = "40vh"))
      #        )
      # )
    )
  )
}

measurementsServer <- function(id, geneId, diseaseId){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
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
    })
    
    ### Store identifiers in order to be able to be reseted
    rv <- reactiveValues(geneId = NULL, diseaseId = NULL)
    
    observe({
      rv$geneId <- geneId()
      rv$diseaseId <- diseaseId()
    })
    
    ### Conditional rendering: if there is no geneId or diseaseId coming from
    ### Biomarkers, Conditions or Summaries, then print table on full page, else 
    ### print table and heatmap
    output$measurements_plots <- renderUI({
      if(!is.null(rv$geneId) || !is.null(rv$diseaseId)){
        withLoader(DTOutput(ns("geneDisease")))
      } else {
        fluidRow(
          column(8,
                 withLoader(DTOutput(ns("geneDisease")))
          ),
          column(4,
                 fluidRow(
                   column(12, plotlyOutput(ns("plot1"), height = "40vh"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput(ns("plot2"), height = "40vh"))
                 )
                 
          )
        )
      }
    })
    
    ### Format data
    data <- reactive({
      measurements() %>%
        filter( if( !is.null(rv$geneId) ) geneid == rv$geneId else TRUE ) %>%
        filter( if( !is.null(rv$diseaseId) ) diseaseid == rv$diseaseId else TRUE ) %>%
        mutate(symbol = createLink_Symbol(geneid, symbol),
               nctid =   createLink_NCIT(nctid),
               name = createLink_Name(diseaseid, name)) %>%
      relocate(symbol, .after=nctid) %>%
      relocate(name, .after=symbol) %>%
      rename("Gene" = symbol, "Condition" = name,
             "NCT ID" = nctid, "Measurement" = sentence,
             "Num. Pmids" = npmids,
             "Biomarker Type" = bmtype ) %>%  arrange(desc(`Num. Pmids`) )
    })

    ### Produce table
    js <- sprintf(
      "table.on('click', 'td', function(){
        var cell = table.cell(this);
        var colindex = cell.index().column;
        var colname = table.column(colindex).header().innerText;
        Shiny.setInputValue('%s', colname);
        console.log(colname);
      });", session$ns("columnName")
    )
    
    output$geneDisease <- renderDataTable(
      datatable(
        data(),
        escape = FALSE,
        filter = "top",
        options = list(dom = 'ltipr',
                       columnDefs = list(list(className = 'dt-center', targets ="_all"),
                                         list(visible = FALSE, targets = c(3,4,5,8)))),
        rownames = FALSE,
        selection = list(mode = 'single', target = 'cell'),
        callback = JS(js)
      ) %>% formatStyle("Num. Pmids", backgroundColor = "yellow")
    )
    
    measurementData <- eventReactive(req(length(input$geneDisease_cell_clicked) > 0), {
      colName <- input$columnName
      geneId <- NULL
      diseaseId <- NULL
      if (colName == "Num. Pmids") {
        geneId <- data()[input$geneDisease_cell_clicked$row, "geneid", drop =TRUE]
        diseaseId <- data()[input$geneDisease_cell_clicked$row, "diseaseid", drop =TRUE]
        return(
          list(
            geneId = geneId,
            diseaseId = diseaseId
          )
        )
      } else return(NULL)
    })
    
    ### Plot
    dataPlot <- reactive({
      data <- data() %>% filter(`Biomarker Type` != "biomarker") %>%  dplyr::select(geneid,`Biomarker Type`) %>% 
        unique()  %>% 
        group_by(`Biomarker Type` ) %>% 
        dplyr::summarise(n = n_distinct(geneid)) 
      
      data$`Biomarker Type` <- gsub("_", " ",data$`Biomarker Type`)
      Ntotal <- data() %>%  select(geneid) %>%   unique() 
      
      data$percent <- round(data$n/length(Ntotal$geneid)*100, digits = 2)
      data <- data[ order(-data$percent),]
      data$`Biomarker Type` <- factor(data$`Biomarker Type`, levels = data$`Biomarker Type`)
      data
    })
    
    output$plot1 <- renderPlotly({
      req(is.null(rv$geneId) && is.null(rv$diseaseId))
      p <- ggplot(dataPlot(), aes(`Biomarker Type`, percent, fill = `Biomarker Type`)) +
        geom_col() + theme_bw() + xlab("Biomarker Type") +theme(legend.position="none")
      plotly::ggplotly(p)
      
    })
    
    output$plot2 <- renderPlotly({
      req(is.null(rv$geneId) && is.null(rv$diseaseId))
      p<- ggdotchart(dataPlot(), x = "Biomarker Type", y = "percent",
                     color = "Biomarker Type",                                # Color by groups
                     #   palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
                     sorting = "ascending",                        # Sort value in descending order
                     add = "segments",                             # Add segments from y = 0 to dots
                     rotate = TRUE,                                # Rotate vertically
                     group = "Biomarker Type",                                # Order by groups
                     dot.size = 10,                                 # Large dot size
                     label = dataPlot()$n,                        # Add mpg values as dot labels
                     font.label = list(color = "white", size = 8,
                                       vjust = 0.5),               # Adjust label parameters
                     ggtheme = theme_pubr()                        # ggplot2 theme
      ) +theme(legend.position="none")

      plotly::ggplotly(p)
    })
    
    
    
    ### Reset button
    observeEvent(input$reload, {
      rv$geneId <- NULL
      rv$diseaseId <- NULL
    })
    
    return(measurementData)
    
  })
}
