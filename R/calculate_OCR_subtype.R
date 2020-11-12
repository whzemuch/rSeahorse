#' Calculate OCR subtype
#'
#'
#' @param data_min a data frame
#'
#' @param bin_list a list
#' @param return_type df" or "plot"
#'
#'
#' @export
#' @return mito_Base_Respiration, mito_Proton_Leak, mito_ATP_Production, mito_Max_Respiration, mito_Non_Mito_Oxygen_Consumption, mito_Spare_Respiratory_Capacity

#' @examples
#' calculate_OCR_subtype <- function(data_min=data_min, bin_list=bin_list, return_type="df")
#' calculate_OCR_subtype <- function(data_min=data_min, bin_list=bin_list, return_type="plot")
#'
calculate_OCR_subtype <- function(data_min=data_min, bin_list=bin_list, return_type="df") {


  # get the last time point for each section


  last_timepoint <- data_min %>%
    dplyr::filter(!(Well %in%  wells_exclude)) %>%
    dplyr::mutate(section = cut(minute, breaks = bin_list$breaks, labels=bin_list$labels)) %>%
    dplyr::select(section, minute) %>%
    dplyr::group_by(section) %>%
    dplyr::filter(row_number() > n()-1) %>%
    dplyr::pull(minute, section) #%>% View


  data_mito <-  data_min %>%
    dplyr::filter(!(Well %in%  wells_exclude)) %>%

    dplyr::group_by(Well) %>%
    dplyr::select(Well, Group, minute, OCR) %>%
    dplyr::mutate(section = cut(minute, breaks = bin_list$breaks, labels=bin_list$labels)) %>%
    tidyr::pivot_wider(
      names_from=c("section","minute"),
      values_from = "OCR"
    ) %>%

    dplyr::rowwise() %>%
    dplyr::mutate(
      max_a = max(c_across(starts_with("A"))),
      min_b = min(c_across(starts_with("B"))),
      max_c = max(c_across(starts_with("C"))),
      min_d = max(c_across(starts_with("D")))
    ) %>%

    dplyr::select(1:2, num_range("Base_", last_timepoint[["Base"]]), starts_with("min"), starts_with("max")) %>%
    dplyr::rename(Base_Resp = 3) %>%

    dplyr::mutate(
      mito_Base_Respiration = Base_Resp - min_d,
      mito_Proton_Leak = min_b - min_d,
      mito_ATP_Production = Base_Resp - min_b,
      mito_Max_Respiration = max_c - min_d,
      mito_Non_Mito_Oxygen_Consumption = min_d,
      mito_Spare_Respiratory_Capacity = mito_Max_Respiration -  mito_Base_Respiration
    ) %>%

    dplyr::mutate(across(contains("mito"), ~if_else(.x < 0, 0, .x))) %>%
    dplyr::filter(Group != "Y") %>%
    dplyr::select(Well, Group, contains("mito")) %>%
    tidyr::pivot_longer(
      cols= starts_with("mito_"),
      names_to = "OCR_Type",
      names_prefix = "mito_",
      values_to = "OCR"
    )


  p_mito <- data_mito %>%
    ggplot(aes(x=Group, y=OCR, fill=Group )) +
    geom_boxplot(width= 0.5) +
    geom_jitter(width=0.2) +
    facet_wrap( ~ OCR_Type, scales = "free")+
    labs(x="", y="OCR(pmol/min)")+
    #theme_Publication() +
    theme(
      strip.background = element_blank(),
      legend.position="none")


  if (return_type == "df") return(data = split(data_mito, data_mito$OCR_Type)) else return(p_mito)

}



#' Tittle
#'
#' @param event_label a data frame including columns: Time, Event, Prarmeter, minutes, treatment, y
#'
#' @return a list
#' @export
#'
#' @examples
#'
make_time_bin <- function(event_label) {
    bin_list <-  list(
    breaks = c(-Inf, event_label$minute, Inf),
    treatment = c("Base", event_label$treatment),
    labels = c("Base", event_label$Parameter)
  )
}
