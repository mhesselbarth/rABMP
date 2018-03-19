#' Prepare Input
#'
#' Function to prepare the input in a way that an ID coloumn exists and the data is nested
#' @param input [\code{tibble(1)}]\cr Tibble with input data
#' @param ID [\code{string(1)}]\cr String with the name of ID coloumn if already existing
#'
#' @export
Prepare.Input <- function(input, ID=NULL){

  if(is.null(ID)){
    input$ID <- 1:nrow(input)
    cat("\nCreated 'ID' coloumn \n\n")
  }

  else{
    names(input)[names(input) == ID] <- 'ID'
    cat(paste0("\n Renamed '", ID, "' to 'ID' \n\n"))
  }

  result <- tidyr::nest(input, -ID)
  return(result)

}
