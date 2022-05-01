summaryUI <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("reload"), "Reload Data", class="btn-primary"),
    hr(),
    fluidRow(
      column(12, uiOutput(ns("summaries_plots")))
      # column(6, withLoader(DT::DTOutput(ns("geneDiseaseSummary")))),
      # column(6, plotOutput(ns("plot"), height = "70vh"))
      
    )
  )
}

summaryServer <- function(id, geneId, diseaseId) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ### Query
    summaries <- reactive({
      pool %>%
        tbl("gene_disease_summary") %>% 
        collect() 
    })
    
    ### Store identifiers in order to be able to be reseted
    rv <- reactiveValues(geneId = NULL, diseaseId = NULL)
    
    observe({
      rv$geneId <- geneId()
      rv$diseaseId <- diseaseId()
    })
    
    ### Conditional rendering: if there is no geneId or diseaseId coming from
    ### Biomarkers or Conditions, then print table on full page, else 
    ### print table and heatmap
    output$summaries_plots <- renderUI({
      if(!is.null(rv$geneId) || !is.null(rv$diseaseId)){
        fluidRow(
          column(8, withLoader(DT::DTOutput(ns("geneDiseaseSummary")))),
          column(4, plotOutput(ns("plot"), height = "70vh"))  
        )
      } else {
        column(12, withLoader(DT::DTOutput(ns("geneDiseaseSummary"))))
      }
    })
    
    ### Format data
    data <- reactive({
      summaries() %>%
        filter( if( !is.null(rv$geneId) ) geneid == rv$geneId else TRUE ) %>%
        filter( if( !is.null(rv$diseaseId) ) diseaseid == rv$diseaseId else TRUE ) %>%
        rename( "Gene" = symbol, "Condition" = name,
                "year initial" = year_initial, "year final" = year_final,
                "Num. Clin.Trials" =  nclinicaltrials,
                "Num. Pmids" = npmids)  %>%
        arrange(desc(`Num. Clin.Trials`) ) %>%
        mutate(Fisher = formatC(Fisher, format="e", digits=2, zero.print = TRUE)) %>% 
        relocate(c(`Num. Clin.Trials`, `Num. Pmids`, `year initial`, `year final`), .after = OddsRatio)
    })
    
    ### Produce table
    
    # js
    js <- sprintf(
      "table.on('click', 'td', function(){
        var cell = table.cell(this);
        var colindex = cell.index().column;
        var colname = table.column(colindex).header().innerText;
        Shiny.setInputValue('%s', colname);
        console.log(colname);
      });", session$ns("columnName")
    )
    
    # table
    output$geneDiseaseSummary <- renderDataTable(
      datatable(
        data(),
        filter = "top",
        options = list(dom = 'ltipr',
                       columnDefs = list(
                         list(className = 'dt-center', targets ="_all"),
                         list(visible = FALSE, targets=c(1,2,10))
                       )
        ),
        selection = list(mode = 'single', target = 'cell'),
        escape = FALSE,
        rownames = FALSE,
        callback = JS(js)
        ) %>%
        formatStyle("Num. Clin.Trials", backgroundColor = "purple") %>% 
        formatStyle("Num. Pmids", backgroundColor = "yellow")
    )
    
    ## data to be returned (ids for other tables)
    summaryData <- eventReactive(req(length(input$geneDiseaseSummary_cell_clicked) > 0), {
      colName <- input$columnName
      geneId <- NULL
      diseaseId <- NULL
      if (colName %in% c("Num. Clin.Trials", "Num. Pmids")) {
        geneId <- data()[input$geneDiseaseSummary_cell_clicked$row, "geneid", drop =TRUE]
        diseaseId <- data()[input$geneDiseaseSummary_cell_clicked$row, "diseaseid", drop =TRUE]
        return(
          list(
            geneId = geneId,
            diseaseId = diseaseId,
            colName = colName
          )
        )
      } else return(NULL)
    })
    
    ### Plot
    output$plot <- renderPlot({
      df <- summaries() %>%
        filter( if( !is.null(rv$geneId) ) geneid == rv$geneId else TRUE ) %>%
        filter( if( !is.null(rv$diseaseId) ) diseaseid == rv$diseaseId else TRUE ) %>%
        mutate(disease=factor(name, levels=rev(sort(unique(name))))) %>%
        mutate(countfactor=cut(nclinicaltrials, breaks=c(  0, 1, 10, 100, 200,  max(nclinicaltrials, na.rm=T)),
                               labels=c(  "0-1", "1-10", "10-100", "100-200", ">400"))) %>%
        # change level order
        mutate(countfactor=factor(as.character(countfactor), levels=rev(levels(countfactor)))) %>%
        arrange(desc(nclinicaltrials)) %>% head(50)
      
      textcol <- "grey40"
      
      if(!is.null(rv$geneId)){
        plot <- ggplot(df, aes(x=symbol, y=disease, fill=countfactor))
      }
      
      if(!is.null(rv$diseaseId)){
        plot <- ggplot(df, aes(x=disease, y=symbol, fill=countfactor))
      }
      
      plot <- plot +
        geom_tile(colour="white", size=0.2)+
        guides(fill=guide_legend(title="Number of\nClinical Trials"))+
        labs(x="", y="", title="Enriched biomarkers")+
        scale_y_discrete(expand = c(0, 0))+
        scale_fill_manual(values = c("#d53e4f", "#f46d43", "#fdae61",
                                   "#fee08b", "#e6f598", "#abdda4", "#ddf1da"),
                          na.value = "grey90")+
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
      plot <-plot + theme(axis.text.x = element_text(size = 16, face="bold"))  
      
      plot
    })
    
    ### Reset button
    observeEvent(input$reload, {
      rv$geneId <- NULL
      rv$diseaseId <- NULL
    })

    return(summaryData)

  })
}