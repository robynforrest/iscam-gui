# ccamUtils.r by Chris Grandin

destroyWorkspace <- function(){
  # remove everything from the user's workspace
  rm(list=ls(all=T,envir=.GlobalEnv),envir=.GlobalEnv)
}

#rsprintf <- function(obj,formatOut="%1.2f",roundDec=2){
#  sprintf(formatOut,round(obj,roundDec))
#}

gletter <- function(i,cex=1.0,font=1){
  #Adds letters to plots (i.e., figure numbers)
  usr <- par("usr")
  inset.x <- 0.05*(usr[2]-usr[1])
  inset.y <- 0.05*(usr[4]-usr[3])
  text(usr[1]+inset.x,usr[4]-inset.y,paste("(",letters[i],")",sep=""),cex=cex,font=font)
}

rsprintf <- function(obj,
                     formatOut = "%1.4f",
                     roundDec  = 4,
                     formatThousands = FALSE){
  # foramts the obj to the sprintf format given, rounded to roundDec
  # If formatThousands is true, the result will be a decimal integer seperated by a comma
  # in the thousands range.
  if(formatThousands){
    formatC(obj, big.mark=",", format="d")
  }else{
    sprintf(formatOut,round(obj,roundDec))
  }
}
