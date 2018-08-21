source("www/login/validations.R", local = TRUE)

# Muestra el modal de bienvenida cuando se lanza la aplicacion
showModal(modalDialog(
  title = "project name",
  includeHTML("www/login/modaltext.txt"),
  fluidRow(
    column(
      12, br(),
      column(6, align = "left", fluidRow(actionButton("closeModal", "Continue", class = "btn-primary"))),
      column(6, align = "right", fluidRow(actionButton("btLoginModal", "Log in")))
    )
  ),
  easyClose = FALSE,
  footer = NULL
))

# Evento que cierra el modal
observeEvent(input$closeModal, {
  removeModal()
})

# Evento que muestra el modal del login
observeEvent(input$btLoginModal, {
  showModal(loginModal())
})

# Funcion que muestra el modal de login cuando es llamado por el modal de bienvenida
loginModal <- function(message = ""){
  modalDialog(
    title = "Log in to project name",
    fluidRow(
      column(
        12,
        textInput("userName", "Email", "100%", value = getLoginInput("username")),
        passwordInput("passwd", "Password", "100%", value = getLoginInput("password")),
        checkboxInput("rememberMe","Remember me", T),
        column(6, align = "left", fluidRow(actionButton("goBackModal", "Back"))),
        column(6, align = "right", fluidRow(actionButton("checkLogin", "Log in", class = "btn-primary")))
      )
    ),
    fluidRow(
      column(
        12, align = "right", br(),
        a( "Forgot your password?", href="#shiny-tab-forgotPass","data-toggle"="tab"),
        " | ",
        a( "Sign up", href="#shiny-tab-register","data-toggle"="tab")
      )
    ),
    message,
    easyClose = FALSE,
    footer = NULL
  )
}

# Muestra nuevamente el modal de bienvenida cuando se hace clic en el boton "Back"
observeEvent(input$goBackModal, {
  showModal(modalDialog(
    title = "project name",
    includeHTML("www/login/modaltext.txt"),
    fluidRow(
      column(
        12, br(),
        column(6, align = "left", fluidRow(actionButton("closeModal", "Continue", class = "btn-primary"))),
        column(6, align = "right", fluidRow(actionButton("btLoginModal", "Log in")))
      )
    ),
    easyClose = FALSE,
    footer = NULL
  ))
})

# Funcion que muestra el modal de login cuando es llamado por el dropdownmenu
loginModalMenu <- function(message = ""){
  modalDialog(
    title = "Log in to project name",
    fluidRow(
      column(
        12,
        textInput("userName", "Email", "100%", value = getLoginInput("username")),
        passwordInput("passwd", "Password", "100%", value = getLoginInput("password")),
        checkboxInput("rememberMe","Remember me", T),
        column(6, align = "left", fluidRow(actionButton("closeModal", "Close"))),
        column(6, align = "right", fluidRow(actionButton("checkLogin", "Log in", class = "btn-primary")))
      )
    ),
    fluidRow(
      column(
        12, align = "right", br(),
        a( "Forgot your password?", href="#shiny-tab-forgotPass","data-toggle"="tab"),
        " | ",
        a( "Sign up", href="#shiny-tab-register","data-toggle"="tab")
      )
    ),
    br(),
    tags$div(id = "error", align = "center", message),
    easyClose = FALSE,
    footer = NULL
  )
}

# Codigo de cookies
getLoginInput <- function(type){
  # Thu Jul 26 13:32:14 2018 ------------------------------
  ans <- "--"
  txt <- NULL
  if(type == "username")  txt <- input$store$userName
  else if(type == "password")  txt <- input$store$passwd
  if (ssErr(txt) > 0){
    print("Encountered an error decrypting the text!")
    return("none")
  }
  else{
    ans <- txt
  }
  return(ans)
}

# check credentials when user logs in
observeEvent(input$checkLogin, {
  mssg = "Invalid Username or Password"
  val  <- validateEmail(trimws(input$userName))
  inputPass <- trimws(input$passwd)
  if (USER$Logged == FALSE && as.logical(val[1]) && nchar(inputPass) > 0) {
    
    checkCredentials(isolate(trimws(input$userName)),digest(isolate(inputPass), "sha256", serialize = FALSE))
    
    key <- NULL
    if(isolate(input$rememberMe)){
      key <- pubKey
      updateStore(session, "userName", isolate(input$userName), encrypt=key)
      updateStore(session, "passwd", isolate(input$passwd), encrypt=key)
    }
    else{
      key <- pubKey
      updateStore(session, "userName", "", encrypt=key)
      updateStore(session, "passwd", "", encrypt=key)
    }
  }
  
  if(USER$Logged == FALSE){
    showModal(loginModalMenu(mssg));
  }
  
})

# Verifica las credenciales
checkCredentials <- function(Username, Password){
  # Username <- isolate(trimws(input$userName))
  # Password <- digest(isolate(inputPass), "sha256", serialize = FALSE)
  
  mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  userc = dbSendQuery(mydb, "select id, username, password, fname, lname, country, organization from users where available = 1")
  data1 = fetch(userc, n=-1)
  dbDisconnect(mydb)
  PASSWORD <- data.frame(Brukernavn = data1[,2], Passord = data1[,3])
  
  Id.username <- which(PASSWORD$Brukernavn == Username)
  
  if (length(Id.username) == 1) {
    if (PASSWORD[Id.username, 2] == Password) {
      USER$Logged <- TRUE
      
      USER$id <- data1[Id.username, "id"]
      USER$username <- data1[Id.username, "username"]
      USER$fname <- data1[Id.username, "fname"]
      USER$lname <- data1[Id.username, "lname"]
      USER$org <- data1[Id.username, "organization"]
      USER$country <- data1[Id.username, "country"]
      
      js$setcookie(Username, Password)
    }
  }
}

# to perform when a user logs in
observe({
  if(USER$Logged == TRUE) {
    removeModal()
    
    session$userData$logged <- TRUE
    session$userData$userId <- USER$id
    
    # menu to be shown with hidap network options when the users logs in
    output$help <- renderText("Help")
    output$userLoggedText <- renderText(paste0("Account: ", USER$fname))
    
    output$menuHeader <- renderMenu({
      dropdownMenu(
        headerText = "...",
        icon = icon("user", "fa-lg"),
        badgeStatus = NULL,
        tags$li(
          class = "dropdown",
          a(
            icon("male"),
            "Profile",
            onclick = "openTab('userProfile')",
            href = NULL,
            style = "cursor: pointer;"
          )
        ),
        tags$li(
          class = "dropdown",
          a(
            icon("key"),
            "Authentication",
            onclick = "openTab('changePass')",
            href = NULL,
            style = "cursor: pointer;"
          )
        ),
        tags$li(
          class = "dropdown",
          actionLink("btLogOut", "Log Out", icon = icon("sign-out"))
        )
      )
    })
    
    output$menuSidebar <- renderMenu({
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home"), selected = TRUE),
        menuItem("Menu Item 1", tabName = "menuItem1", icon = icon("minus-square")),
        menuItem("Menu Item 2", tabName = "menuItem2", icon = icon("minus-square")),
        menuItem("Menu Item 3", tabName = "menuItem3", icon = icon("minus-square")),
        menuItem("Account", icon = icon("user"),
                 menuSubItem("Profile", tabName = "userProfile", icon = icon("angle-right")),
                 menuSubItem("Authentication", tabName = "changePass", icon = icon("angle-right"))
        )
      )
    })
    
    output$registerMsg <- renderText("")
    
    output$driveScreen <- renderUI({
      uiDriveMain()
    })
    
    output$trialScreen <- renderUI({
      uiTrialScreenMain()
    })
    
  }
  else {
    USER$id <- NULL
    USER$username <- NULL
    USER$fname <- NULL
    USER$lname <- NULL
    USER$org <- NULL
    USER$country <- NULL
    # session$user <- NULL
    session$userData$logged <- F
    session$userData$userId <- NULL
    
    hideTab(inputId = "tabs", target = "Foo")
    
    output$userLoggedText <- renderText("Guest")
    
    output$menuHeader <- renderMenu({
      dropdownMenu(
        headerText = "...",
        icon = icon("user-times", "fa-lg"),
        badgeStatus = NULL,
        tags$li(
          class = "dropdown",
          actionLink("btLogIn", "Log In", icon = icon("sign-in"))
        )
      )
    })
    
    output$menuSidebar <- renderMenu({
      sidebarMenu(
        menuItem("Home", tabName = "home", selected = TRUE)
      )
    })
    
    output$trialScreen <- renderUI({
      h4("Loading ... ")
    })
  }
})

observeEvent(input$btLogIn, {
  showModal(loginModalMenu())
})


observeEvent(input$btLogOut, {
  js$rmcookie()
  USER$Logged <- FALSE
  showModal(loginModalMenu())
})
