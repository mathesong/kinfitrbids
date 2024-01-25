#' Plot tacdata with an extra log time plot
#'
#' @param tacdata tac data
#' @param titletext text of the title
#'
#' @return plots
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' plot_tacdata_extra(tacdata, titletext)
#' }
plot_tacdata_extra <- function(tacdata, titletext) {

  if( length(unique(tacdata$Region)) > 1 ) {
    # Multiplot

    orig_time <- ggplot2::ggplot(tacdata, aes(x=t_tac, y=TAC, colour=Region)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::labs(x="Time (min)") +
      ggplot2::guides(colour="none", shape="none")

    legend <- cowplot::get_legend(orig_time)

    log_time <- orig_time +
      ggplot2::scale_x_log10() +
      ggplot2::labs(x = "Time (min, log-scaled axis)") +
      ggplot2::guides(colour="none", shape="none")

    tac_plots <- cowplot::plot_grid(orig_time, log_time, legend,
                                          ncol=3, rel_widths = c(3,3,1))

    label <- cowplot::ggdraw() +
      cowplot::draw_label(titletext)

    cowplot::plot_grid(label, tac_plots, rel_heights = c(0.1, 1),
                       nrow = 2)

  } else {

    orig_time <- ggplot2::ggplot(tacdata, aes(x=t_tac, y=TAC)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::labs(x="Time (min)") +
      ggplot2::guides(colour="none", shape="none")

    legend <- cowplot::get_legend(orig_time)

    log_time <- orig_time +
      ggplot2::scale_x_log10() +
      ggplot2::labs(x = "Time (min, log-scaled axis)") +
      ggplot2::guides(colour="none", shape="none")

    tac_plots <- cowplot::plot_grid(orig_time, log_time, legend,
                                    ncol=3, rel_widths = c(3,3,1))

    label <- cowplot::ggdraw() +
      cowplot::draw_label(titletext)

    cowplot::plot_grid(label, tac_plots, rel_heights = c(0.1, 1),
                       nrow = 2)

  }

}





#' Plot tacdata and input data with an extra log time plot
#'
#' @param tacdata tac data
#' @param input input object
#' @param titletext text of the title
#'
#' @return plots
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' plot_tacdata_extra(tacdata, titletext)
#' }
plot_tacblooddata_extra <- function(tacdata, input, titletext, tmax = NULL) {

  if( length(unique(tacdata$Region)) > 1 ) {
    # Multiplot


    plotmax <- min( max(input$AIF), max(tacdata$TAC * 1.1) )
    plotmaxtime <- ifelse(is.null(tmax),
                          yes=max(tacdata$t_tac),
                          no = tmax)

    orig_time <- ggplot2::ggplot(tacdata, aes(x=t_tac, y=TAC, colour=Region)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::labs(x="Time (min)") +
      ggplot2::guides(colour="none", shape="none") +
      geom_line(data=input, aes(x=Time, y=AIF), colour="red") +
      coord_cartesian(ylim = c(0, plotmax), xlim = c(0.01,plotmaxtime))


    legend <- cowplot::get_legend(orig_time)

    log_time <- orig_time +
      ggplot2::scale_x_log10() +
      ggplot2::labs(x = "Time (min, log-scaled axis)") +
      ggplot2::guides(colour="none", shape="none")

    tac_plots <- cowplot::plot_grid(orig_time, log_time, legend,
                                    ncol=3, rel_widths = c(3,3,1))

    label <- cowplot::ggdraw() +
      cowplot::draw_label(titletext)

    cowplot::plot_grid(label, tac_plots, rel_heights = c(0.1, 1),
                       nrow = 2)

  } else {

    orig_time <- ggplot2::ggplot(tacdata, aes(x=t_tac, y=TAC)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::labs(x="Time (min)") +
      ggplot2::guides(colour="none", shape="none") +
      geom_line(data=input, aes(x=Time, y=AIF), colour="red") +
      coord_cartesian(ylim = c(0, plotmax), xlim = c(0.01,plotmaxtime))

    if(!is.null(tmax)) {
      orig_time <- orig_time +
        xlim(c(0,5))
    }

    legend <- cowplot::get_legend(orig_time)

    log_time <- orig_time +
      ggplot2::scale_x_log10() +
      ggplot2::labs(x = "Time (min, log-scaled axis)") +
      ggplot2::guides(colour="none", shape="none")

    tac_plots <- cowplot::plot_grid(orig_time, log_time, legend,
                                    ncol=3, rel_widths = c(3,3,1))

    label <- cowplot::ggdraw() +
      cowplot::draw_label(titletext)

    cowplot::plot_grid(label, tac_plots, rel_heights = c(0.1, 1),
                       nrow = 2)

  }

}


#' Plot tacdata and input data with an extra log time plot
#'
#' @param tacdata tac data
#' @param input input object
#' @param titletext text of the title
#'
#' @return plots
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' plot_tacdata_extra(tacdata, titletext)
#' }
plot_tacblooddata <- function(tacdata, input, titletext, tmax = NULL) {

  if( length(unique(tacdata$Region)) > 1 ) {
    # Multiplot


    plotmax <- min( max(input$AIF), max(tacdata$TAC * 1.1) )
    plotmaxtime <- ifelse(is.null(tmax),
                          yes=max(tacdata$t_tac),
                          no = tmax)

    orig_time <- ggplot2::ggplot(tacdata, aes(x=t_tac, y=TAC, colour=Region)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::labs(x="Time (min)") +
      ggplot2::guides(colour="none", shape="none") +
      geom_line(data=input, aes(x=Time, y=AIF), colour="red") +
      coord_cartesian(ylim = c(0, plotmax), xlim = c(0.01,plotmaxtime))


    legend <- cowplot::get_legend(orig_time)

    out_plot <- cowplot::plot_grid(orig_time, legend,
                                    ncol=2, rel_widths = c(3,1))

    label <- cowplot::ggdraw() +
      cowplot::draw_label(titletext)

    cowplot::plot_grid(label, out_plot, rel_heights = c(0.1, 1),
                       nrow = 2)

  } else {

    orig_time <- ggplot2::ggplot(tacdata, aes(x=t_tac, y=TAC)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::labs(x="Time (min)") +
      ggplot2::guides(colour="none", shape="none") +
      geom_line(data=input, aes(x=Time, y=AIF), colour="red") +
      coord_cartesian(ylim = c(0, plotmax), xlim = c(0.01,plotmaxtime))

    if(!is.null(tmax)) {
      orig_time <- orig_time +
        xlim(c(0,5))
    }

    legend <- cowplot::get_legend(orig_time)

    out_plot <- cowplot::plot_grid(orig_time, legend,
                                   ncol=2, rel_widths = c(3,1))

    label <- cowplot::ggdraw() +
      cowplot::draw_label(titletext)

    cowplot::plot_grid(label, out_plot, rel_heights = c(0.1, 1),
                       nrow = 2)

  }

}
