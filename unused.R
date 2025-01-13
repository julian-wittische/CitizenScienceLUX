############ Working directories for the data
os <- Sys.info()
### RStudio server
if (os[1]=="Linux"){
  setwd("/home/jwittische/Data/")
}
### Windows work
if (os[1]=="Windows"&os[4]=="MC232706"){
  setwd("W:/01_Services/SCR_Informations Patrimoine Naturel/")
}
### Windows home
if (os[1]=="Windows"&os[4]!="MC232706"){
  setwd("old")
}