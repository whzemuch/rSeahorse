#' Convert the layout of a plate from wide format to long format
#'
#' @param csvfile input by user
#'
#' @return
#' @export
#' @importFrom tidyr  %>%
#' @export
#' @examples
#' file = system.file("extdata", "layout.csv", package="rSeahorse")
#' convert_layout_to_long(csvfile = file)
convert_layout_to_long <- function(csvfile) {

  layout <- read.csv(file = csvfile, header = TRUE, check.names = FALSE)
  colnames(layout)[1] <- "Row"


  layout_long <- layout %>%
    tidyr::pivot_longer(-Row,
                        names_to = "Col",
                        values_to = "Group"
    ) %>%
    dplyr::mutate(Well = calculate_well_id(Row, Col)) %>%
    dplyr::mutate(Well = factor(Well))
  return(layout_long)
}



#' Calculate well_id based on row and col
#'
#' @param Row character A, B, C, D
#' @param Col character 1,2,3,4,5,6
#'
#' @return an integer for well_id
#'
#'
#' @examples
#'
calculate_well_id  <- function(Row, Col) {

  well_id <- (match(Row, LETTERS) - 1 ) * 6 + as.numeric(Col)
  return(well_id)
}

