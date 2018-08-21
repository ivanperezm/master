# cran
library(shiny)
library(shinydashboard)
library(PKI)
library(magrittr)
library(bsplus) # hidapNetwork
library(RMySQL)
library(digest)
library(httr) # library for http requests, used to make POST request to the server

# github
library(shinyjs) # ??
library(shinyStore) # install_github("trestletech/shinyStore")

#### Llaves para encriptar las cookies: utilizado en el Remember me
privKey <- PKI.load.key(file="test.key")
pubKey <- PKI.load.key(file="test.key.pub")
####

source("www/js/jsCode.R", local = TRUE)
#

ui <- dashboardPage(
  dashboardHeader(
    title = "project name",
    tags$li(class = "dropdown", tags$a(uiOutput("help"))),
    tags$li(class = "dropdown", tags$a(uiOutput("userLoggedText"))),
    dropdownMenuOutput("menu44")
  ),
  dashboardSidebar(
    sidebarMenu(
      sidebarMenuOutput("menu")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
    ),
    
    # tags$head(
    #   tags$style(HTML(".fa { font-size: 18px; }"))
    # ),
    
    tags$head(
      tags$script(
        HTML("
             var openTab = function(tabName){
             $('a', $('.sidebar')).each(function() {
             if(this.getAttribute('data-value') == tabName) {
             this.click()
             };
             });
             }
             ")
      )
    ),
    
    tags$style(HTML("

                              .box.box-solid.box-warning>.box-header {
                                color:#000;
                                background:#f5f5f5;
                                /*padding-top:0px*/
                              }

                              .box.box-solid.box-warning>.box-body {
                                color:#000;
                              background:#f5f5f5
                              }

                              .box.box-solid.box-warning{
                              border-bottom-color:#f5f5f5;
                              border-left-color:#f5f5f5;
                              border-right-color:#f5f5f5;
                              border-top-color:#f5f5f5;
                              }



                              .box.box-solid.box-info>.box-header {
                                color:#000;
                                background:#f2dede;
                                /*padding-top:0px*/
                              }

                              .box.box-solid.box-info>.box-body {
                                color:#000;
                              background:#f2dede
                              }

                              .box.box-solid.box-info{
                              border-bottom-color:#f2dede;
                              border-left-color:#f2dede;
                              border-right-color:#f2dede;
                              border-top-color:#f2dede;
                              }


                      ")),
    
    
    tags$head(
      #### Funciones javascript para el manejo de cookies en general
      tags$script(src = "js/js.cookies.js")
      ####
    ),

    useShinyjs(),
    extendShinyjs(text = jscode),
    #### Inicializa la libreria ShinyStore: utilizado en el Remenber me
    initStore("store", "shinyStore-haf", privKey), # Namespace must be unique to this application!
    ####

    #includeCSS("www/css/custom.css"),
    
    tabItems(
      tabItem(tabName = "changePass",div(uiOutput("uiChangePass"), uiOutput("mssgChngPass"))),
      tabItem(tabName = "userProfile",div(uiOutput("uiUserProfile"))),
      tabItem(tabName = "register", div(uiOutput("registerMsg"), uiOutput("uiRegister") )),
      tabItem(tabName = "forgotPass", div(uiOutput("uiForgotPass"),uiOutput("pass") )),
      tabItem(tabName = "menu1", "menu 1 body"),
      tabItem(tabName = "home", "home body")
    )
  )
)

server <- function(input, output, session) ({
  source("www/conn/conn.R", local = TRUE)
  source("www/login/showModal.R", local = TRUE)
  source("www/login/createAccount.R", local = TRUE)
  #### Funcion que hace que contunue logeado
  observe({
    js$getcookie()
    if (!is.null(input$jscookie_user) &&
        input$jscookie_user != "") {
      checkCredentials(input$jscookie_user, input$jscookie_pass)
    }
  })
  ####

  #### Login
  session$userData$logged <- F
  session$userData$userId <- NULL

  USER <- reactiveValues(Logged = FALSE, username = NULL, id = NULL, fname = NULL, lname = NULL, org=NULL, country=NULL)
  ####

  useShinyjs()
  extendShinyjs(text = jscode)



  # USER <- reactiveValues(Logged = FALSE, username = NULL, id = NULL, fname = NULL, lname = NULL, org=NULL, country=NULL)
  #### Modulo pendiente
  dt_myMaterialList <- reactiveValues()
  ####

  #### Files necesrios para el login
  # source("www/loginModule/userMenuUi.R",local = TRUE)
  # source("www/driveModule/drive.R", local = TRUE)
  # source("www/sitesModule/sites.R", local = TRUE)
  # source("www/loginModule/login.R", local = TRUE)
  
  ####
})

shinyApp(ui, server)
