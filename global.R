library(shiny)
library(RMariaDB)
library(DT)
library(shinycssloaders)
library(dplyr)
library(bslib)

con <- function() {
  DBI::dbConnect(
    MariaDB(),
    host = "david.prib.upf.edu",
    user = "psebastian",
    dbname = "biomarkers_2022",
    port = 3306)
}

loadData <- function(table) {
  con <- con()
  data <- tbl(con, table)
  data <- as.data.frame(data)
  dbDisconnect(con)
  return(data)
}

gene_disease_loadData <- function(){
  con <- con()
  # Construct the fetching query
  query <- "select gd.*, g.symbol, d.name
            from gene_disease as gd
            left join genes as g
            on gd.geneid = g.geneid
            left join diseases as d
            on gd.diseaseid = d.diseaseid"
  data <- dbGetQuery(con, query)
  data <- as.data.frame(data)
  dbDisconnect(con)
  return(data)
}

createLink_Button <- function(text){
  sprintf('<button type="button">%s</button>', text)
}

createLink_Symbol <- function(geneid, symbol){
  sprintf('<a href="https://www.ncbi.nlm.nih.gov/gene/%s" target="_blank">%s</a>',geneid, symbol)
}

createLink_Name <- function(diseaseid, name){
  sprintf('<a href="https://meshb.nlm.nih.gov/record/ui?ui=%s" target="_blank">%s</a>',diseaseid, name)
}