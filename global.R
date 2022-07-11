library(shiny)
library(RMariaDB)
library(DT)
library(shinycustomloader)
library(dplyr)
library(bslib)
library(pool)
library(ggplot2)
library(plotly)
library(ggrepel)
library(rvest)
library(ggpubr)

pool <- dbPool(
  drv = MariaDB(),
  host = "david.prib.upf.edu",
  username = "psebastian",
  password = "correguepardo",
  dbname = "biomarkers_2022",
  port = 3306
)
onStop(function() {
  poolClose(pool)
})


createLink_Button <- function(text){
  paste0('<button type="button" style="width: 70%;"
         class="btn btn-success action-button">', text, '</button>')
}

createLink_Symbol <- function(geneid, symbol){
  sprintf("%s <a id='%s' href='https://www.ncbi.nlm.nih.gov/gene/%s' target='_blank'>
                <i class='fa' style='font-size:24px'>&#xf14c;</i>
              </a>",
          symbol, geneid, geneid)
}

createLink_Name <- function(diseaseid, name){
  sprintf("%s <a id='%s' href='https://meshb.nlm.nih.gov/record/ui?ui=%s' target='_blank'>
                <i class='fa' style='font-size:24px'>&#xf14c;</i>
              </a>",
          name, diseaseid, diseaseid)
}


createLink_NCIT <- function(nctid){
  sprintf("%s <a href='https://clinicaltrials.gov/ct2/show/%s' target='_blank'>
                <i class='fa' style='font-size:24px'>&#xf14c;</i>
              </a>",
          nctid, nctid)
}

createLink_PMID <- function(pmid){
  sprintf("%s <a href='https://pubmed.ncbi.nlm.nih.gov/%s' target='_blank'>
                 <i class='fa' style='font-size:24px'>&#xf14c;</i>
              </a>",
          pmid, pmid)
}