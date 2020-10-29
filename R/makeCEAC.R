#' Function to make a CEAC plot from matrices of total costs and QALYs
#' @param total_costs A matrix of costs for PSA runs.
#' @param total_qalys A matrix of QALYs for PSA runs.
#' @param treatment   A vector of treatment names (must match matrix column names).
#' @return a CEAC plot produced in ggplot.
#' @examples
#' makeCEAC(total_costs = m_costs,  total_qalys = m_qalys, treatment = c("Apixaban (5mg bd)","Dabigatran (150mg bd)","Coumarin (INR 2-3)", "Edoxaban (60mg od)","Rivaroxaban (20mg od)", "No treatment"))
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal theme labs scale_color_manual scale_y_continuous

makeCEAC <- function(total_costs,
                     total_qalys,
                     treatment = c("Apixaban (5mg bd)","Dabigatran (150mg bd)","Coumarin (INR 2-3)",
                                         "Edoxaban (60mg od)","Rivaroxaban (20mg od)", "No treatment")
){

  n.treatments = length(treatment)

  # new color scheme
  legend_colors = c("#1B9E77","#D95F02","#7570B3","#E7298A","#5f9ea0","gray")
  names(legend_colors) = c("Apixaban (5mg bd)","Dabigatran (150mg bd)","Edoxaban (60mg od)","Rivaroxaban (20mg od)","Coumarin (INR 2-3)","No treatment")
  legend_colors = legend_colors[names(legend_colors) %in% treatment]

  # take what I need from the model output
  TC = total_costs[,colnames(total_costs) %in% treatment]
  TQ = total_qalys[,colnames(total_qalys) %in% treatment]

  lambdas <- c(1:50) * 1000
  INB = c()
  for(l in lambdas){
    nb_temp = TQ * l - TC
    inb_temp = apply(nb_temp,1,function(x) x == max(x))
    inb_temp = apply(inb_temp,1,sum)
    inb_df_temp = data.frame(Intervention = colnames(TQ),
                             lambda = l,
                             value = inb_temp/sum(inb_temp))
    INB = rbind(INB,inb_df_temp)
  }


  # plot
  ggplot2::ggplot(data = INB,
                  ggplot2::aes(x = lambda,y= value,col = Intervention) )+
    ggplot2::geom_line(size=1.5) +

    ggplot2::scale_y_continuous(breaks=seq(0,1,0.25),limits = c(0,1),name = "Probability most cost-effective") +
    ggplot2::xlab(label = "Willingness-to-pay (GBP)")+
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top") +
    ggplot2::labs(title = "Cost Effectiveness Acceptability Curves",
         subtitle = "The probability each preferred intervention is most cost effective against willingness to pay for each QALY threshold.") +
    # apply color scheme
    ggplot2::scale_color_manual(name="Treatment",values = legend_colors) +
    ggplot2::theme(legend.position = "top",
          legend.text = ggplot2::element_text(size=11),
          legend.title = ggplot2::element_text(size=11),
          title = ggplot2::element_text(size=11)) +
    NULL

}
