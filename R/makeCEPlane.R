#' Function to make a Cost-effectiveness Plane plot from matrices of total costs and QALYs
#' @param total_costs A matrix of costs for PSA runs.
#' @param total_qalys A matrix of QALYs for PSA runs.
#' @param treatment   A vector of treatment names (must match matrix column names).
#' @return a CE-Plane plot produced in ggplot.
#' @examples
#' makeCEPlane(total_costs = m_costs,
#' total_qalys = m_qalys,
#' treatment = c("Apixaban (5mg bd)","Dabigatran (150mg bd)","Coumarin (INR 2-3)", "Edoxaban (60mg od)","Rivaroxaban (20mg od)", "No treatment"),
#' comparitor = "Coumarin (INR 2-3)")
#' @importFrom reshape2 melt ggplot2 ggplot aes geom_abline geom_line geom_vline geom_hline theme_minimal theme labs scale_color_manual scale_y_continuous stat_ellipse guides

makeCEPlane <- function(total_costs = example_TC,
                        total_qalys = example_TQ,
                        comparitor = "notreat" ,
                        treatment = "treat 1",
                        thresh = 30000,
                        show_ellipse = F
){

  ce_plane_plot = ggplot2::ggplot()  +
    # coordinate systems
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::theme_minimal()  +
    ggplot2::labs(title = "Cost-effectiveness Plane",
         subtitle = paste0("Results of Probabilistic Sensitivity Analysis:")) +
    # labels
    ggplot2::xlab("Incremental QALYs") +
    ggplot2::ylab("Incremental Costs (GBP)")

  # show threshold line, if supplied
  if(!is.null(thresh)){
    ce_plane_plot = ce_plane_plot +
      ggplot2::geom_abline(slope = thresh,linetype="dashed",intercept = 0)
    # geom_line(aes(x=c(-1,1),y=c(-thresh*10,thresh*10)),linetype="dashed")
  }

  # show incrementals, if treatment selected
  if(!is.null(treatment)){
    # total costs and total qalys are selected from model outputs.
    TQ = total_qalys
    TC = total_costs

    # color scheme
    legend_colors = c("#1B9E77","#D95F02","#7570B3","#E7298A","#5f9ea0","gray")
    names(legend_colors) = c(paste("treat",1:5),"notreat")
    legend_colors = legend_colors[names(legend_colors) %in% treatment]

    # calculate incremental COSTS and QALYs based upon selection.
    incr_C <- data.frame(TC[,treatment] - TC[,comparitor])
    names(incr_C) = treatment
    incr_Q <- data.frame(TQ[,treatment] - TQ[,comparitor])
    names(incr_Q) = treatment
    # transform to long dataframe
    incr_long = cbind(reshape2::melt(incr_C,id.vars = NULL),reshape2::melt(incr_Q,id.vars = NULL)[,"value"])
    names(incr_long) = c("Treatment","incr_C","incr_Q")
    incr_mean_Q = aggregate(incr_Q ~ Treatment,incr_long,mean)
    incr_mean_C = aggregate(incr_C ~ Treatment,incr_long,mean)
    incr_means = merge(incr_mean_Q,incr_mean_C,by="Treatment")

    # add ellipse, if selected
    if(show_ellipse){
      ce_plane_plot =
        ce_plane_plot +
        ggplot2::stat_ellipse(data= incr_long[!(incr_long$Treatment %in% comparitor),],
                              ggplot2::aes(x=incr_Q,y=incr_C,col=Treatment), type="norm", level=0.9,
                     segments =50)
    }

    # add iteration results and mean estimate to plot
    ce_plane_plot =
      ce_plane_plot +
      ggplot2::geom_point(data= incr_long, ggplot2::aes(x=incr_Q,y=incr_C,col=Treatment),alpha=0.5,size=0.7) +
      ggplot2::geom_point(data= incr_means, ggplot2::aes(x=incr_Q,y=incr_C,fill = Treatment),col="black",size=3,pch=21) +

      # symmetric axes
      ggplot2::coord_cartesian(xlim=c(-quantile(abs(incr_long$incr_Q),0.99),quantile(abs(incr_long$incr_Q),0.99)),
                      ylim=c(-quantile(abs(incr_long$incr_C),0.99),quantile(abs(incr_long$incr_C),0.99)))  +

      # apply color scheme and design legend
      ggplot2::scale_fill_manual(name=NULL,values = legend_colors,guide=F) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4,linetype=F,alpha=1))) +
      ggplot2::scale_color_manual(name=paste(comparitor,"vs."),values = legend_colors) +
      ggplot2::theme(legend.position = "top",
            legend.text = ggplot2::element_text(size=11),
            legend.title = ggplot2::element_text(size=11),
            title = ggplot2::element_text(size=11))


  }

  ce_plane_plot

  return(ce_plane_plot)
}
