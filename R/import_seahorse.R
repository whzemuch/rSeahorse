#' Import seahorse data from am excel file , clean and organize data for viewing and plotting
#'
#' @param file an excel file with worksheet: "Rate Data", "Time Events", which is exported by the seahorse software
#' @param group_info  group names and  for each group, how many wells are included
#'
#' @return a list  {data, datta_min, event lable, well_label, sheet_names}
#' @import dplyr
#' @importFrom magrittr %>%
#' @import readxl
#' @importFrom stringr str_detect
#' @importFrom janitor convert_to_datetime
#' @export
#'
#' @examples
#'
#'
#'
#'
import_seahorse <- function(file=file, group_info=group_info) {

  # list the sheet namec
  sheet_names <- readxl::excel_sheets(file)

  #ROS data
  rate_data <-  readxl::read_xls(file, sheet="Rate Data", skip=7)

  # time for A, B, C, D
  event <- read_excel(file, sheet = "Time Events", skip=4)

  # join the group information to the rate data

  data <- rate_data %>%
    dplyr::select(4, 5, 7) %>%
    dplyr::mutate(Well=factor(Well)) %>%
    dplyr::left_join(group_info, by="Well") %>%
    dplyr::mutate(OCR = `OCR (pMoles/min)`)

  # convert time to minutes
  start_time <- min(data$Time)
  data_min <- data %>%
    dplyr::group_by(Group) %>%
    dplyr::mutate(minute = difftime(Time, start_time, units = "mins") %>%
          as.numeric() %>%
          floor()
    ) %>%
    dplyr::ungroup()

  # construct data for labeling the vertical lines
  event_label <- event %>%
    dplyr::filter(Event == "Injecting") %>%
    dplyr::filter(str_detect(Time, "1")) %>%
    dplyr::mutate(Time = janitor::convert_to_datetime(Time)) %>%
    dplyr::mutate(minute = difftime(Time, start_time, units = "mins") %>%
          as.numeric() %>%
          floor()
    ) %>%
    dplyr::mutate(treatment = c("Glucose", "Oligomycin", "FCCP", "Rote_AA"), y= Inf) %>%
    dplyr::ungroup()


  # construct data for labeling samples with the well name

  well_label <- data %>%
    dplyr::group_by(Group, Col, Row, Well) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(x = Inf, y = Inf) %>%
    dplyr::select(-n)

  return(list(data = data,
              data_min = data_min,
              event_label = event_label,
              well_label = well_label,
              sheet_names = sheet_names
              )
         )


}
