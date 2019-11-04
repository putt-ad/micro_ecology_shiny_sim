#####
#test input test script
#####

readinteger <- function()
{ 
  n <- readline(prompt="Enter an integer: ")
  n <- as.integer(n)
  if (is.na(n)){
    n <- readinteger()
  }
  return(n)
}

print(readinteger())
list_with_na <- c(1,3,7,80,NA,12,90)


readintegers <- function()
{ 
  n <- readline(prompt="Enter an integer: ")
  if(!grepl("^[0-9]+$",n))
  {
    return(readintegers())
  }
  
  return(as.integer(n))
}

print(readintegers())
