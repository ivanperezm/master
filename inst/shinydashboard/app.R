# cran
library(shiny)
library(shinydashboard)
library(PKI)
library(magrittr)
library(bsplus) # hidapNetwork??
library(RMySQL)
library(digest)
library(httr) # library for http requests, used to make POST request to the server

# github
library(shinyjs) # devtools::install_github("daattali/shinyjs")
library(shinyStore) # devtools::install_github("trestletech/shinyStore")

# Llaves para encriptar las cookies: utilizado en el Remember me
privKey <- PKI.load.key(file="www/key/test.key")
pubKey <- PKI.load.key(file="www/key/test.key.pub")

source("www/js/jsCode.R", local = TRUE)

ui <- dashboardPage(
  dashboardHeader(
    title = "project name",
    tags$li(class = "dropdown", tags$a(uiOutput("help"))),
    tags$li(class = "dropdown", tags$a(uiOutput("userLoggedText"))),
    dropdownMenuOutput("menuHeader")
  ),
  dashboardSidebar(
    sidebarMenu(
      sidebarMenuOutput("menuSidebar")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
    ),
    
    # Activa el acceso a profile, authentication, etc. desde el menu del header
    tags$head(
      tags$script(
        HTML("
          var openTab = function(tabName){
          	$('a', $('.sidebar')).each(function(){
          		if(this.getAttribute('data-value') == tabName){
          			this.click()
                  };
              });
          }
        ")
      )
    ),
    
    # Funciones javascript para el manejo de cookies en general
    tags$head(
      tags$script(src = "js/js.cookies.js")
    ),

    useShinyjs(),
    extendShinyjs(text = jscode),
    
    # Inicializa la libreria ShinyStore: utilizado en el Remenber me
    initStore("store", "shinyStore-haf", privKey), # Namespace must be unique to this application!
    
    tabItems(
      # Account
      tabItem(tabName = "userProfile",div(uiOutput("uiUserProfile"))),
      tabItem(tabName = "changePass",div(uiOutput("uiChangePass"), uiOutput("mssgChngPass"))),
      
      # Sign up
      tabItem(tabName = "register", div(uiOutput("registerMsg"), uiOutput("uiRegister") )),
      
      # Forgot your password?
      tabItem(tabName = "forgotPass", div(uiOutput("uiForgotPass"),uiOutput("pass") )),
      
      # Sidebar menu
      tabItem(tabName = "home", "home body"),
      tabItem(tabName = "menuItem1", "menu 1 body"),
      tabItem(tabName = "menuItem2", "menu 2 body"),
      tabItem(tabName = "menuItem3", "menu 3 body")
    )
  )
)

server <- function(input, output, session) ({
  source("www/conn/conn.R", local = TRUE)
  source("www/login/showModal.R", local = TRUE)
  source("www/login/createAccount.R", local = TRUE)
  
  # Funcion que hace que contunue logeado
  observe({
    js$getcookie()
    if (!is.null(input$jscookie_user) &&
        input$jscookie_user != "") {
      checkCredentials(input$jscookie_user, input$jscookie_pass)
    }
  })
  
  # Login
  session$userData$logged <- F
  session$userData$userId <- NULL
  USER <- reactiveValues(Logged = FALSE, username = NULL, id = NULL, fname = NULL, lname = NULL, org=NULL, country=NULL)
})

shinyApp(ui, server)
