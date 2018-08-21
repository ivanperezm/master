allowedCharacters  <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_.-,()?!*@%&[]{}+=$# "
allowedCharactersPass  <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_.-#@?!%&,*;"
allowedCharactersMail  <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_.-@+"

# Valida inputs
validateUserCreateForm <- function(){
  mail <- trimws(input$newUsrMail)
  fName <- trimws(input$newUsrFName)
  lName <- trimws(input$newUsrLName)
  org   <- trimws(input$newUsrOrg)
  pass <- trimws(input$newUsrPass)
  
  validEmail <- validateEmail(mail)
  
  if(!as.logical(validEmail[1])){
    updateTextInput(session, "newUsrMail",
                    label ="* Email Address (username): INVALID EMAIL ")
    output$pass <- renderText(paste("<font color='red'><h4>", validEmail[2],"</h4></font>", sep = ""))
    return(FALSE)
  }
  else{
    updateTextInput(session, "newUsrMail",
                    label ="* Email Address (username): ")
  }
  
  if(usernameIsInDb(mail)){
    output$pass <- renderText("<font color=red><h4>User is already registered</h4></font>")
    return(FALSE)
  }
  
  lnMail  <- nchar(mail)
  lnPass  <- nchar(pass)
  lnPassR <- nchar(trimws(input$newUsrPassRepeat))
  lnFName <- nchar(fName)
  lnLName <- nchar(lName)
  lnOrg   <- nchar(org)
  lnCounS <- nchar(trimws(input$countrySelection))
  
  lenghtValid <- lnMail * lnPass * lnPassR * lnFName * lnLName * lnOrg * lnCounS
  passwMatch  <- lnPass == lnPassR && pass == trimws(input$newUsrPassRepeat)
  
  if(lenghtValid == 0){
    output$pass <- renderText("<font color=red><h4>Must complete all fields with (*)</h4></font>")
    return(FALSE)
  }
  
  valPass <- validatePassword(pass)
  
  if(!as.logical(valPass[1])){
    output$pass <- renderText(paste("<font color=red><h4>", valPass[2],"</h4></font>", sep=""))
    return(FALSE)
  }
  
  if(!passwMatch ){
    output$pass <- renderText("<font color=red><h4>Passwords don't match</h4></font>")
    return(FALSE)
  }
  
  validFname <- validateInput(fName)
  if(!as.logical(validFname[1])){
    updateTextInput(session, "newUsrFName",
                    label ="* Name: INVALID STRING")
    output$pass <- renderText(paste0("<font color=red><h4>", validFname[2], "</h4></font>"))
    return(FALSE)
  }
  else{
    updateTextInput(session, "newUsrFName",
                    label ="* Name: ")
  }
  
  validLname <- validateInput(lName)
  if(!as.logical(validLname[1])){
    updateTextInput(session, "newUsrLName",
                    label ="* Lastname: INVALID STRING")
    output$pass <- renderText(paste0("<font color=red><h4>", validLname[2], "</h4></font>"))
    return(FALSE)
  }
  else{
    updateTextInput(session, "newUsrLName",
                    label ="* Lastname: ")
  }
  
  validOrg <- validateInput(org)
  if(!as.logical(validOrg[1])){
    updateTextInput(session, "newUsrOrg",
                    label ="* Organization: INVALID STRING")
    output$pass <- renderText(paste0("<font color=red><h4>", validOrg[2], "</h4></font>"))
    return(FALSE)
  }
  else{
    updateTextInput(session, "newUsrOrg",
                    label ="* Organization: ")
  }
  
  output$pass <- renderText("")
  return(TRUE)
  
}

# validate user email
validateEmail <- function(mail){
  res <- c(TRUE, "")
  if (!grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",mail, ignore.case=TRUE)){
    res <- c(FALSE, "Not a valid Email")
    return(res)
  }
  
  mail_split <- strsplit(mail, "")[[1]]
  for (letter in mail_split) {
    if (!grepl(letter, allowedCharactersMail, fixed=TRUE)){
      res <- c(FALSE, "Email contains not valid characters")
      return(res)
    }
  }
  
  return (res)
}

# valida usuario
usernameIsInDb <- function(username){
  mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  qryUser = dbSendQuery(mydb, paste("select count(*) as cont from users where username = '",username ,"'", sep=""))
  res = fetch(qryUser, n=-1)
  num <- (res["cont"])
  dbDisconnect(mydb)
  return(num == 1)
}

# valida pass
validatePassword <-function(pass){
  lnPass  <- nchar(pass)
  
  if(lnPass < 8 || lnPass > 12 ){
    res <- c(FALSE,"Your password must contain at least 8 and at most 12 characters" )
    return(res)
  }
  
  pass_split <- strsplit(pass, "")[[1]]
  # verify that pass contains only allowed characters
  for (letter in pass_split) {
    if (!grepl(letter, allowedCharactersPass, fixed=TRUE)){
      res <- c(FALSE,"Your password contains invalid characters")
      return(res)
    }
  }
  
  res <- c(TRUE,"")
  return(res)
}

# valida input
validateInput <- function(input){
  input_split <- strsplit(input, "")[[1]]
  
  if (nchar(input) > 100){
    res <- c(FALSE, "Input is too long")
    return(res)
  }
  
  if (nchar(input) < 1){
    res <- c(FALSE, "Input is missing")
    return(res)
  }
  
  # verify that input contains only allowed characters
  for (letter in input_split) {
    if (!grepl(letter, allowedCharacters, fixed=TRUE)){
      res <- c(FALSE, "Input contains not valid characters")
      return(res)
    }
  }
  res <- c(TRUE, "")
  return(res)
}