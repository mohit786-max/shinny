credentials <- data.frame(
  user = c("asim", "verma"), # mandatory
  password = c("azerty", "12345"), # mandatory
  start = c("2021-06-11"), # optional (all others)
  expire = c(NA, "2021-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single 'Shiny' applications.",
  stringsAsFactors = FALSE
)


library(shinydashboard)
library(shiny)
library(shinymanager)
library(dplyr)
library(purrr)
library(rlang)
library(stringr)

library(data.table)
library(r2d3)

library(odbc)
library(DBI)
library(config)

mtcars<- mtcars%>%mutate(car= row.names(mtcars))

# car_list <- mtcar %>%
#   collect() %>%
#   split(.$car) %>%  
#   map(~ .$row_No)

car_list<-as.list(unique(mtcars$car))
car_list[["chk"]]<- "All_cars"

ui <- dashboardPage(
  dashboardHeader(title = "Quick Example"),
  dashboardSidebar(sidebarMenu(
    selectInput(
      inputId = "car",
      label = "Cars:",
      choices = names(car_list),
      size = 15,
      selected = "All_cars",
      selectize = FALSE
    ))),
  dashboardBody(
    valueBoxOutput("avg_wt"),
    tableOutput("mtcars")
  )
)

ui <- secure_app(ui)

server <- function(input, output,session) {
  
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  
  base_data<- reactive({
    if(input$car != "All_cars" )
       {a<-mtcars%>%filter(car %in% input$car)}
       else {
         a<-mtcars
       }
  })
  output$avg_wt <- renderValueBox({
    base_data()%>%summarise(avg = mean(wt, na.rm = TRUE)) %>%
      pull() %>% round(digits = 1) %>% prettyNum(big.mark = ",") %>%
      valueBox(subtitle = "Avg_wt",icon = icon("weight"))
  })
  output$mtcars <- renderTable(base_data())
}
shinyApp(ui, server)


