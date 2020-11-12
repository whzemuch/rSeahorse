#'  An overview of the whole plate
#'
#' @param list returned by import_seahourse
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 rel
#'
#'
#'
#' @return
#' @export
#'
#'
#' @examples
#' qview_seahorse(data_min, well_label)
#'
qview_seahorse <- function(data_min, well_label) {


  # check if two dataframes exist
  # df_exist <- sum(names(list) %in% c("data_min", "well_label")) == 2
  # if (df_exist) {
  #   data_min = df$data_min
  #   well_label = df$well_label
  # } else{
  #   stop("The datafrmaes(data_min, well_label) don't exsit!")
  # }


  #filter(!(Well %in% c("6", "9", "11", "15", "18"))) %>%
  p <- ggplot2::ggplot(data=data_min, aes(minute, OCR, color=Row)) +
    geom_point() +
    geom_smooth(method = "gam", size = 1) +
    geom_text(data = well_label, aes(x=x, y=y, label=Well), hjust=1.2, vjust=1.5) +
    geom_text(data = well_label, aes(x=x, y=y, label=Group), hjust=1, vjust=2.8) +
    facet_grid(Row ~ Col) +
    ggtitle("Quick View of Seahorse  Data") +
    #theme_bw() +
    theme(
      plot.title = element_text(face = "bold",
                                size = rel(1.2),
                                hjust = 0.5),
      axis.title = element_text(face = "bold",size = rel(1)),

      legend.position = "none"
    )

  return(p)

}
