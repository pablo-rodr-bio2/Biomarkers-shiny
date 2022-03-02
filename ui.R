navbarPage(
  id = "navbarPage",
  tags$style(HTML("
      span.disease {
        background-color: #7774D1;
        color: white;
      }
      span.disease.covid.cdisease {
        background-color: rgb(67, 69, 233);
        color: white; 
      }
      span.gene {
        background-color: #FF8000;
        color: white;
      }
      .navbar.navbar-default ul.nav.navbar-nav > li > a.active {
        background-color: #778899;
      }
      table.dataTable tr.active td, table.dataTable td.active {
        background-color: #DAA520 !important;
      }
      #header_title {
        color: #FFFFF0;
        font-weight: bold;
      }
      "
      )
  ),
  title = actionLink("header_title","CLINICAL TRIALS BIOMARKERS", icon = icon("home")),
  collapsible = TRUE,
  theme = bs_theme(bootswatch = "yeti"),
  nav_spacer(),
  tabPanel("About", value = "about", includeMarkdown("about.md")),
  tabPanel("Genes", value = "genes", withSpinner(DT::dataTableOutput("genes"))),
  tabPanel("Diseases", value = "diseases", withSpinner(DTOutput("diseases"))),
  tabPanel("Studies",  value = "studies", withSpinner(DTOutput("studies" ))),
  tabPanel("Gene-Disease Summary",  value = "gene_disease_summary",
           actionButton("reload1", "Reload Data"),
           hr(),
           withSpinner(DTOutput("gene_disease_summary"))),
  tabPanel("Gene-Disease",  value = "gene_disease",
           actionButton("reload2", "Reload Data"),
           hr(),
           withSpinner(DTOutput("gene_disease"))),
  tabPanel("Publications",  value = "publications", withSpinner(DTOutput("publications")))
)