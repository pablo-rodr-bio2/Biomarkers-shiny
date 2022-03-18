navbarPage(
  id = "navbarPage",
  title = actionLink("header_title","CLINICAL TRIALS BIOMARKERS", icon = icon("home")),
  header = tags$head(
    tags$style(type = "text/css", "body {padding-top: 70px;}"), ## doesn't work in custom-css.css
    tags$link(rel = "stylesheet", type = "text/css", href="custom-css.css")
  ),
  nav_spacer(),
  tabPanel("About", value = "about", aboutUI("home")),
  # tabPanel("About", value = "about", includeHTML("about.html")),
  tabPanel("Biomarkers", value = "genes", biomarkersUI("genes1")),
  tabPanel("Conditions", value = "diseases", withLoader(DTOutput("diseases"))),
  tabPanel("Summary",  value = "gene_disease_summary",
           actionButton("reload1", "Reload Data", class="btn-primary"),
           hr(),
           withLoader(DTOutput("gene_disease_summary"))
           ),
  tabPanel("Measurements",  value = "gene_disease",
           actionButton("reload2", "Reload Data", class="btn-primary"),
           hr(),
           withLoader(DTOutput("gene_disease"))
           ),
  tabPanel("Publications", value = "publications",
           actionButton("reload3", "Reload Data", class="btn-primary"),
           hr(),
          withLoader(DTOutput("publications"))),
  collapsible = TRUE,
  theme = bs_theme(bootswatch = "yeti"),
  position = "fixed-top",
  footer = textUI("text1")
)

#tabPanel("Studies",  value = "studies", withLoader(DTOutput("studies" ))),