navbarPage(
  id = "navbarPage",
  title = actionLink("header_title","CLINICAL BIOMARKERS", icon = icon("home")),
  header = tags$head(
    tags$style(type = "text/css", "body {padding-top: 70px;}"), ## doesn't work in custom-css.css
    tags$link(rel = "stylesheet", type = "text/css", href = "custom-css.css"),
    tags$link(rel = "stylesheet", type = "text/css", 
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
  ),
  nav_spacer(),

  tabPanel("About", value = "about", htmlOutput("homeWeb")),
  tabPanel("Biomarkers", value = "genes", biomarkersUI("genes1")),
  tabPanel("Conditions", value = "diseases", conditionsUI("diseases1")),
  tabPanel("Summary",  value = "gene_disease_summary", summaryUI("summary1")),
  tabPanel("Measurements",  value = "gene_disease", measurementsUI("measurements1")),
  tabPanel("Publications", value = "publications", publicationUI("publications1")),
  collapsible = TRUE,
  theme = bs_theme(bootswatch = "yeti"),
  # theme = bs_theme(version = 5),
  position = "fixed-top",
  footer = textUI("text1")
)
