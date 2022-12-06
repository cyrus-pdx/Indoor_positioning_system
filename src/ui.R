#load libraries
library(shiny)
library(shinydashboard)

#Shiny Server
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Indoor Positioning System"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Introduction", tabName = "introduction"),
        menuItem("Methods", icon = icon("bar-chart-o"),
                 menuSubItem("Trilateration", tabName = "trilateration"),
                 menuSubItem("Fingerprinting", tabName = "fingerprinting"),
                 menuSubItem("KNN", tabName = "knn")
        ),
        menuItem("Coparison", tabName = "comparison"),
        menuItem("Conclusion", tabName = "conclusion"),
        menuItem("References", tabName = "references"),
        menuItem("Dashboard Source Code", icon = icon("file-code-o"),
                 href = "https://github.com/rstudio/shinydashboard/blob/gh-pages/_apps/sidebar/app.R"
        )
      )
    ),
    dashboardBody()
  )
)