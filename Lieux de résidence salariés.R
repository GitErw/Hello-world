library(readxl)
library(shiny)
library(dplyr)
library(leaflet)

setwd("//dpt01/BusinessIntelligence$/DATA MANAGEMENT/Projets/2019/Divers/Cartographie - staff pour greve")


ref <- read_excel("code-insee-postaux-geoflar.xlsx")
tab <- read_excel("Lieux de résidence salariés - 301119.xlsx")


# merge tables
mrg <- tab %>% 
inner_join(ref, by=c("Code postal" = "CODE POSTAL"))

names(mrg) <-   iconv(make.names(names(mrg)), to="ASCII//TRANSLIT//IGNORE")

#mrg %>% mutate(dept = "Toto")

str(mrg)


ui <- fluidPage(
  titlePanel(
    h1("Staff")
  ),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId="Department",
                         label="Department",
                         choices = unique(mrg$Departement)
                         ,selected= unique(mrg$Departement)
                         ),
      checkboxGroupInput(inputId="Area",
                         label="Area",
                         choices = unique(mrg$Nom.Dept)
                         ,selected= unique(mrg$Nom.Dept)
                         )
    ),
    mainPanel (
      leafletOutput("Cartograhie"), #, width = "100%", height = "100%")
      dataTableOutput("mtable")
    )
  )
)



server <- function(input,output) {
  mrg_reac <- reactive ({mrg %>% 
      filter(Departement %in% input$Department, Nom.Dept %in% input$Area) %>%
      group_by(Nom.Dept) %>% 
      summarise(lon = mean(lon), lat = mean(lat), nb = sum(Nombre.de.nom))
    
  })
  #version tabulaire
  output$mtable <- renderDataTable({
    (mrg_reac() %>% select(Nom.Dept, nb) %>% arrange(desc(nb)) )
    })
  output$Cartograhie <- renderLeaflet({
    leaflet(mrg_reac()) %>% addTiles() %>% setView(2.333333, 48.866667,  9) %>%
      addCircles(lng = ~lon, lat = ~lat, weight = 1,
                 radius = ~(nb*50), popup = ~Nom.Dept )
      
  })
}

shinyApp(ui=ui, server=server)
             