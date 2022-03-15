aboutUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, h1(tags$b("BIOMARKERS IN CLINICAL TRIALS")))
      ),
    fluidRow(
      column(6, div(img(src="g1745.png", height="90%", width = "90%")), align = "center", offset = 6)
    ),
    p("The numbers of clinical trials are now rising in an unprecedented manner.
        The data they contain can be exploited to extract information about what human
        diseases are investigated and which are molecular biomarkers used in this massive 
        pool of studies. In this contribution, we have applied artificial intelligent 
        technologies to explore protein and gene biomarkers more frequently assessed in
        Clinical Trials data. We found over 4,300 genes measured in over 55,000 Clinical 
        Trials involving 3,000 diseases."),
    plotlyOutput(ns("plot_summary"))
  )
}

aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot_summary <- renderPlotly({
      data <- readRDS("data_for_plots.rds") %>% 
        rename("Percentage" = percent)
      p <- ggplot(data, aes(`Biomarker Type`, Percentage, fill = factor(`Biomarker Type`))) +
        geom_col() + theme_bw() + xlab("Biomarker Type") +theme(legend.position="none")
      plotly::ggplotly(p, source="click1", tooltip = c("Biomarker Type", "Percentage")) %>%
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
  })
}