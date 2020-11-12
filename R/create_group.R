#' Title
#'
#' @return a data frame with columns: Well, Group, Col, Row, Treatment
#' @export
#'
#' @examples
#' group_info <-  create_group()
create_group <- function() {
    Group_Info <- tibble::tibble(
    Well = factor(1:24),
    Group = rep(c("X", "Y", "Z", "F"), each = 6),
    Col = paste0("C",rep(1:6, 4 )),
    Row = paste0("R", rep(1:4, each=6)),
    Treatment =  rep(c("ECMplus", "BM-ECM", "TCP", "Fresh"), each = 6)
     )

    Group_Info$Group <- factor(Group_Info$Group, levels= c("F", "Z", "X", "Y"),
                     labels=c("Fresh", "TCP", "ECMplus", "BM-ECM" ))

    return(Group_Info)
}

