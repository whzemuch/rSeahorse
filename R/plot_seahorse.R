#' Title
#'
#' @param data_min data with minutes
#' @param wells_exclude a vector
#' @param event_label label event
#' @param group_info a named vector, name is the labels, value is the levels
#' @param size_group a number, for calculation SEM
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return
#' @export
#'
#' @examples
#' plot_seahorse(data_min, well_exclude, event_label, group_info, size_group =6 )
#'
plot_seahorse <- function(data_min=data_min, wells_exclude=wells_exclude, event_label = event_label, group_info=group_info, size_group) {


   data <- data_min %>%
    # filter unwanted samples
     dplyr::filter(!(Well %in% wells_exclude)) %>%
     dplyr::group_by(Group, minute) %>%
     dplyr::summarize(AVG_OCR = mean(OCR),
              SD = sd(OCR)/sqrt(size_group),
              .groups="drop")

    # draw a ggplot with point and line

    p1 <- data %>%
      ggplot2::ggplot(aes(x=minute, y=AVG_OCR, color=Group,  shape=Group)) +
      geom_point() +
      geom_line() +
    #geom_smooth(method = "gam", size = 1, span=0.9) +

    # add error bar
      geom_errorbar(aes(ymin=AVG_OCR - SD, ymax=AVG_OCR + SD), width = .8, position = position_dodge(.05))+
    # add the vertical lines
      geom_vline(data = event_label, aes(xintercept=minute),color="grey40", linetype="longdash", lwd=0.5 ) +

    # Change title and x, ylabs
      labs(
            title = "Seahorse Analysis of rat islets cultured on TCP VS ECMplus",
            x = "Minutes",
            y = "OCR (pMoles/min)"
       ) +
      theme_bw()

  # add annotation text
     p2 <- p1 + annotate("text", x=event_label$minute, y=Inf, label= event_label$Parameter, hjust=1.2, vjust=1.5) # +
       #theme_Publication()

    return(p2)



}
