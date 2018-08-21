source("www/login/validations.R", local = TRUE)

# Muestra lista de paises en el formulario para crear usuarios
listCountries <- c('Aruba','Afghanistan','Angola','Anguilla','Albania','Andorra','United Arab Emirates','Argentina','Armenia','American Samoa','Antarctica','French Southern Territories','Antigua and Barbuda','Australia','Austria','Azerbaijan','Burundi','Belgium','Benin','Bonaire','Burkina Faso','Bangladesh','Bulgaria','Bahrain','Bahamas','Bosnia and Herzegowina','Belarus','Belize','Bermuda','Bolivia','Brazil','Barbados','Brunei','Bhutan','Burma','Bouvet Island','Botswana','Byelorusian SSR (Former)','Central African Republic','Canada','Cocos (Keeling) Islands','Switzerland','Chile','China','CIPHQ','Cote dIvoire','Cameroon','Congo','Congo','Cook Islands','Colombia','Comoros','Cape Verde','Costa Rica','Czechoslovakia (Former)','Cuba','Curacao','Christmas Island (Australia)','Cayman Islands','Cyprus','Czech Republic','German Democratic Republic','Germany','Djibouti','Dominica','Denmark','Dominican Republic','Algeria','Ecuador','Egypt','Eritrea','Western Sahara','Spain','Estonia','Ethiopia','Finland','Fiji','Falkland Islands (Malvinas)','France','Faroe Islands','Micronesia','Gabon','United Kingdom','Georgia','Ghana','Gibraltar','Guinea','Guadeloupe','Gambia','Guinea-Bissau','Equatorial Guinea','Greece','Grenada','Greenland','Guatemala','French Guiana','Guam','Guyana','Hong Kong','Heard and Mc Donald Islands','Honduras','Croatia','Haiti','Hungary','Indonesia','India','British Indian Ocean Territory','Ireland','Iran','Iraq','Iceland','Israel','Italy','Jamaica','Jordan','Japan','Kazakhstan','Kenya','Kyrgyzstan','Cambodia','Kiribati','Saint Kitts and Nevis','Korea','Kuwait','Lao People s Democratic Republic','Lebanon','Liberia','Libyan Arab Jamahiriya','Saint Lucia','Liechtenstein','Sri Lanka','Lesotho','Lithuania','Luxemburg','Latvia','Macau','Saint Martin (French part)','Macedonia','Morocco','Monaco','Moldova','Madagascar','Maldives','Mexico','Marshall Islands','Mali','Malta','Myanmar','Mongolia','Northern Mariana Islands','Mozambique','Mauritania','Montserrat','Martinique','Mauritius','Malawi','Malaysia','Mayotte','Namibia','New Caledonia','Niger','Norfolk Island','Nigeria','Nicaragua','Niue','Netherlands','Norway','Nepal','Nauru','Neutral Zone (Former)','New Zealand','Oman','Pakistan','Palestine','Panama','Pitcairn Islands','Peru','Philippines','Palau','Papua New Guinea','Poland','Puerto Rico','Korea','Portugal','Paraguay','French Polynesia','Qatar','Reunion','Romania','Russian Federation','Rwanda','Saudi Arabia','Serbia and Montenegro','Scotland','Sudan','Senegal','Singapore','Saint Helena','Svalbard and Jan Mayen Islands','Solomon Islands','Sierra Leone','El Salvador','San Marino','Somalia','Saint Pierre and Miquelon','Serbia','Sao Tome e Principe','Union of Soviet Socialist Republics (Former)','Surinam','Slovakia','Slovenia','Sweden','Swaziland','Seychelles','Syrian Arab Republic','Turks and Caicos Islands','Chad','Togo','Thailand','Tajikistan','Tokelau','Turkmenistan','East Timor','Tonga','Trinidad and Tobago','Tunisia','Turkey','Tuvalu','Taiwan','Tanzania','Uganda','Ukraine','United States Misc. Pacific Islands','unknown','Uruguay','United States of America','Uzbekistan','Vatican City State','Saint Vincent and the Grenadines','Venezuela','British Virgin Islands','Virgin Islands (US)','Viet Nam','Vanuatu','Wallis and Fortuna Islands_','Samoa','Yemen','Yugoslavia (Former)','South Africa','Zaire','Zambia','Zimbabwe'
)


# Renderiza el formulario para crear usuarios
output$uiRegister <- renderUI({

  if (USER$Logged == FALSE) {
    removeModal()
    fluidRow(
      box(
        title = tagList(shiny::icon("user"), "Create account"),
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        column(width = 6,
               h3("New Account"),
               p("Fields with (*) must be completed"),
               textInput("newUsrMail", "* Email Address (username): ") %>%
                 shinyInput_label_embed(
                   icon("info") %>%
                     bs_embed_tooltip(title = "Alphanumeric (lower and uppercase) and _.-+ Only",  placement = "top")
                 ),

               passwordInput("newUsrPass", "* Password (at least 8 and at most 12 characters): ") %>%
                 shinyInput_label_embed(
                   icon("info") %>%
                     bs_embed_tooltip(title = "Alphanumeric (lower and uppercase) and \n ;_-,.#@?!%&* Only")
                 ),
               passwordInput("newUsrPassRepeat", "* Re-enter Password: "),
               textInput("newUsrFName", "* Name: "),
               textInput("newUsrLName", "* Lastname: "),
               textInput("newUsrOrg", "* Organization: "),
               selectizeInput("countrySelection", choices = listCountries, label="* Country", options = list(maxOptions = 5 , selected = NULL,  placeholder = 'Select Country')),
               actionButton("btCreateUser", "Create"),
               br(), br(),
               # actionLink("ForgotPass", "Forgot your password?"),br(),
               a( "Forgot your password?", href="#shiny-tab-forgotPass","data-toggle"="tab"),
               br(),
               "Already have an account? " , actionLink("btLogIn2", "Log in "), " instead."
        ) #end column
      )#end box
    )#end
  }
})
output$registerMsg <- renderText("")

# Crea usuario
observeEvent(input$btCreateUser, {

  # print("hola")
  if (!validateUserCreateForm()) return()

  strMail  <- trimws(input$newUsrMail)
  strPass  <- digest(trimws(input$newUsrPass), "sha256", serialize = FALSE)
  strFName <- trimws(input$newUsrFName)
  strLName <- trimws(input$newUsrLName)
  strOrg   <- trimws(input$newUsrOrg)
  strCounS <- trimws(input$countrySelection)

  mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  strQry <- paste("insert into users (username, password, fname, lname, organization, country) values('",strMail,"','",strPass,"','", strFName,"','",strLName,"','",strOrg, "','",strCounS, "')", sep ="")
  qryUpdate = dbSendQuery(mydb, strQry)


  params <- list(
    dataRequest = "createUser",
    username = strMail
  )
  
  var <- POST("https://research.cip.cgiar.org/shinydashboard/createNewUser.php", body=params)
  
  code <- content(var, "text")
  if (code == "500"){
    strQry <- paste("delete from users where username = '", strMail, "'", sep ="")
    qryDel = dbSendQuery(mydb, strQry)
    showModal(modalDialog(title = "HiDAP-AGROFIMS", HTML("Problems creating account, please try again.")))
  }
  else if (code == "200") {
    showModal(modalDialog(title = "HiDAP-AGROFIMS", HTML("<h4>Yout account was successfully created, a confirmation message will be sent soon. Check your email to activate your account.</h4> <br> <h5>If you haven't received a message, please check your spam and add us to your contacts.</h5>")))
    output$uiLogin <- renderUI({

      if (USER$Logged == FALSE) {
        wellPanel(
          h3("Start a new session!"),
          textInput("userName", "Username:",getLoginInput("username")),
          passwordInput("passwd", "Password:", getLoginInput("password")),
          checkboxInput("rememberMe","Remember me", T),
          br(),
          actionButton("Login", "Log in"),
          br(),
          br(),
          actionLink("ForgotPass", "Forgot your password?"),br(),
          "Not a user yet? ", actionLink("btCreate", "Create a new account.")
        )
      }
    })
  }
  else{
    strQry <- paste("delete from users where username = '", strMail, "'", sep ="")
    qryDel = dbSendQuery(mydb, strQry)
    showModal(modalDialog(title = "HiDAP-AGROFIMS", HTML("Problems creating account, please try later.")))
  }
  dbDisconnect(mydb)
})

# muestra el profile del usuario
output$uiUserProfile <- renderUI({
  if (USER$Logged == TRUE) {
    fluidRow(
      box(
        title = tagList(shiny::icon("male"), " PROFILE"),
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        column(width = 6,
               disabled(textInput(inputId = "prfUsername", label="Email/Username", value=USER$username)),
               disabled(textInput(inputId = "prfName", label="Name", value=USER$fname)),
               disabled(textInput(inputId = "prfLname", label="Last name", value=USER$lname)),
               disabled(textInput(inputId = "prfOrg", label="Organization", value=USER$org)),
               disabled(textInput(inputId = "prfCountry", label="Country", value=USER$country))
        )
      )
    )
  }
})

# muestra formulario para cambiar de password
output$uiChangePass <- renderUI({
  if (USER$Logged == TRUE) {
    fluidRow(
      box(
        title = tagList(shiny::icon("key"), "AUTHENTICATION"),
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        column(width = 6,
               h3("Password Change"),
               passwordInput("chngPassCurrent", "Current password: "),
               passwordInput("chngPassNew", "New password (at least 8 and at most 12 characters): "),
               passwordInput("chngPassNewRep", "Re-enter new password: "),
               actionButton("btChangePass", "Update")
        )#end column
      ) #end box
    )#end fluidrow
  }
})

# muestra formulario para resetear pass
output$uiForgotPass <- renderUI({
  if (USER$Logged == FALSE) {
    removeModal()
    fluidRow(
      box(
        title = tagList(shiny::icon("lock"), "Password reset"),
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        column(width = 6,
               h3("Forgot your password?"),
               # br(),
               p("Write down your email (username) you used to create your account and a password will be sent there."),
               textInput("userMailReset", "Email (username):"),
               br(),
               actionButton("ResetPass", "Reset my password"),
               br(), br(),
               "Not a user yet? ", a( "Create an account.", href="#shiny-tab-register","data-toggle"="tab"),
               br(),
               "Already have an account? " , actionLink("btLogIn3", "Log in "), " instead."
        )#end column
      )#end box
    )#end fluidrow
  }
})

# each login link must have its own reactive function
observeEvent(input$btLogIn2, {
  showModal(loginModalMenu())
})
observeEvent(input$btLogIn3, {
  showModal(loginModalMenu())
})

###########################################################################################################
# reseting user password
###########################################################################################################
observeEvent(input$ResetPass,{
  output$pass <- renderText("")
  usermail <- trimws(input$userMailReset)
  
  validEmail <- validateEmail(usermail)
  if(!as.logical(validEmail[1])){
    output$pass <- renderText(paste("<font color='red'> <h4><b>", usermail, ": </b> ", validEmail[2], "</h4> </font>", sep=""))
    return()
  }
  
  mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  
  strQry = paste("select count(*) as cant from users where username = '", usermail, "' and available = 1", sep = "")
  res <- fetch(dbSendQuery(mydb,strQry))
  num <- (res["cant"])
  dbDisconnect(mydb)
  if (num == 1 ){
    
    params <- list(
      dataRequest = "resetPassword",
      username = usermail
    )
    
    var <- httr::POST("https://research.cip.cgiar.org/shinydashboard/resetPasswordHidap.php", body=params)
    code <- content(var, "text")
    
    if (code == "200"){
      # showModal(modalDialog(title = "HiDAP-AGROFIMS", HTML("Succesfully reset")))
      output$pass <- renderText("<h4>Password reset successful. An email has been sent with a new password </h4>")
      updateTextInput(session,"userMailReset", value="" )
    }
    else{
      # showModal(modalDialog(title = "HiDAP-AGROFIMS", HTML("Problems reseting password")))
      output$pass <- renderText("<font color='red'> <h5>Problems reseting password</h5> </font>")
    }
  }
  else{
    output$pass <- renderText(paste("<font color='red'> <h4>User <b>", usermail, "</b> is not registered</h4> </font>", sep=""))
  }
} )

###########################################################################################################

###########################################################################################################
# changing user password
###########################################################################################################
observeEvent(input$btChangePass, {
  if (!validateChangePassForm()) return()
  
  mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  
  strQry = paste0("select username, password  from users where username = '", USER$username, "' and available = 1")
  res <- data.frame(fetch(dbSendQuery(mydb,strQry)))
  userDb <- res[,1]
  if(length(userDb) == 1){
    newPass <- input$chngPassCurrent
    Password <- digest(isolate(newPass), "sha256", serialize = FALSE)
    curPass <- res[1,"password"]
    
    if(curPass == Password){
      newPass = digest(isolate(input$chngPassNew), "sha256", serialize = FALSE)
      strQry = paste0("update users set password = '",newPass,"' where username ='",USER$username, "' and password = '",curPass,"' and available = 1")
      updQry = dbSendQuery(mydb,strQry)
      params <- list(
        dataRequest = "passwordChanged",
        username = USER$username,
        fname = USER$fname,
        lname = USER$lname
      )
      
      var <- POST("https://research.cip.cgiar.org/gtdms/hidap/script/agrofims/emailPasswordChanged.php", body=params)
      code <- content(var, "text")
      output$mssgChngPass <- renderText("<font color='blue'><h3>Your password was successfully changed</h3></font>")
      
      output$uiChangePass <- renderUI({
        if (USER$Logged == TRUE) {
          wellPanel(
            h3("Password Change"),
            passwordInput("chngPassCurrent", "Current password: "),
            passwordInput("chngPassNew", "New password (at least 8 and at most 12 characters) "),
            passwordInput("chngPassNewRep", "Re-enter new password: "),
            actionButton("btChangePass", "Update")
          )
        }
      })
      
    }
    else{
      output$mssgChngPass <- renderText("<font color='red'><h3>Incorrect Password</h3></font>")
    }
  }
  else{
    output$mssgChngPass <- renderText(" <font color='red'><h3>Error while changing password. Please try again</h3></font>")
  }
  
  dbDisconnect(mydb)
})

validateChangePassForm <- function(){
  curPass <- trimws(input$chngPassCurrent)
  newPass <- trimws(input$chngPassNew)
  
  lnCurPass   <- nchar(curPass)
  lnNewPass   <- nchar(newPass)
  lnNewPRep   <- nchar(trimws(input$chngPassNewRep))
  
  lenghtValid <- lnCurPass * lnNewPass * lnNewPRep
  passwMatch  <- lnNewPass == lnNewPRep && newPass == trimws(input$chngPassNewRep)
  samePass <- newPass == curPass
  
  if(lenghtValid == 0){
    showModal(modalDialog(title = "HiDAP-AGROFIMS", HTML("Must complete all fields")))
    return(FALSE)
  }
  
  validPass <- validatePassword(curPass)
  if(!as.logical(validPass[1])){
    showModal(modalDialog(title = "HiDAP-AGROFIMS", HTML(paste("Current Password:", validPass[2]))))
    return(FALSE)
  }
  
  validPass <- validatePassword(newPass)
  if(!as.logical(validPass[1])){
    showModal(modalDialog(title = "HiDAP-AGROFIMS", HTML(paste("New Password:", validPass[2]))))
    return(FALSE)
  }
  
  if(samePass){
    showModal(modalDialog(title = "HiDAP-AGROFIMS", HTML("Old and new passwords are the same.")))
    return(FALSE)
  }
  
  if(!passwMatch ){
    showModal(modalDialog(title = "HiDAP-AGROFIMS", HTML("New Passwords don't match.")))
    return(FALSE)
  }
  
  return(TRUE)
}