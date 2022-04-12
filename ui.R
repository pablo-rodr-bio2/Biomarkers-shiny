navbarPage(
  id = "navbarPage",
  title = actionLink("header_title","CLINICAL TRIALS BIOMARKERS", icon = icon("home")),
  header = tags$head(
    tags$style(type = "text/css", "body {padding-top: 70px;}"), ## doesn't work in custom-css.css
    tags$link(rel = "stylesheet", type = "text/css", href="custom-css.css")
  ),
  nav_spacer(),
  tabPanel("About", value = "about", htmlOutput("homeWeb")),
  tabPanel("Biomarkers", value = "genes", biomarkersUI("genes1")),
  tabPanel("Conditions", value = "diseases", conditionsUI("diseases1")),
  tabPanel("Summary",  value = "gene_disease_summary",
           actionButton("reload1", "Reload Data", class="btn-primary"),
           hr(),
           summaryUI("summary1")
           ),
  tabPanel("Measurements",  value = "gene_disease",
           actionButton("reload2", "Reload Data", class="btn-primary"),
           hr(),
           measurementsUI("measurements1")
           ),
  tabPanel("Publications", value = "publications",
           actionButton("reload3", "Reload Data", class="btn-primary"),
           hr(),
           publicationUI("publications1")
          ),
  collapsible = TRUE,
  theme = bs_theme(bootswatch = "yeti"),
  # theme = bs_theme(version = 5),
  position = "fixed-top",
  footer = textUI("text1")
)
