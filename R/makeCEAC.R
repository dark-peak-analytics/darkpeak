#' Function to make a CEAC plot from matrices of total costs and QALYs
#' @param total_costs A matrix of costs for PSA runs.
#' @param total_qalys A matrix of QALYs for PSA runs.
#' @param treatment   A vector of treatment names (must match matrix column names).
#' @param lambda_min  Minimum of the x axis for the lambda plot
#' @param lambda_max  Maximum of the x axis for the lambda plot
#' @return a CEAC plot produced in ggplot.
#' @examples
#' makeCEAC(total_costs = m_costs,  total_qalys = m_qalys, treatment = c("Apixaban (5mg bd)","Dabigatran (150mg bd)","Coumarin (INR 2-3)", "Edoxaban (60mg od)","Rivaroxaban (20mg od)", "No treatment"))
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal theme labs scale_color_manual scale_y_continuous testthat

makeCEAC <- function(total_costs = example_TC,
                     total_qalys = example_TQ,
                     treatment = c("treat 1","notreat"),
                     lambda_min = 0,
                     lambda_max = 50000){

  all_names = colnames(total_costs)

  # assign each treatment a colour & give name based on column
  legend_colors = rainbow(n = ncol(total_costs))
  names(legend_colors) = colnames(total_costs)

  # assign each treatment a dash
  dash_numbers = 1:ncol(total_costs)
  names(dash_numbers) = colnames(total_costs)


  # assign colours and dashes based on treatment choices
  legend_colors = legend_colors[names(legend_colors) %in% treatment]
  legend_dash   = dash_numbers[names(dash_numbers) %in% treatment]

  # Take the appropriate columns from cost and qalys matrices/df
  v_TC = total_costs[,colnames(total_costs) %in% treatment]
  v_TQ = total_qalys[,colnames(total_qalys) %in% treatment]

  # Create a vector of 100 lambda values
  lambdas <- seq(from = lambda_min,
                 to = lambda_max,
                 length.out = 100)

  # ProbCE for each Lambda - slow but reliable
  df_CEAC = c()

  for(l in lambdas){

    # get net benefit for each strategy
    nb = v_TQ * l - v_TC

    # idenfity whether each strategy is the maximum or not
    nb = apply(nb,1,function(x) x == max(x))

    # make data-frame of probability each option is CE
    nb = data.frame(Intervention = colnames(v_TC),
                    lambda = l,
                    value = apply(nb,1,mean))

    # bind to previous lambda data
    df_CEAC = rbind(df_CEAC,nb)
  }


# MAKE PLOT

  ggplot2::ggplot(data = df_CEAC,
                  ggplot2::aes(x = lambda,y= value,col = Intervention)
                  )+

    # theme
    ggplot2::theme_minimal() +

    # legend
    ggplot2::theme(legend.position = "top",
                   legend.text = ggplot2::element_text(size=11),
                   legend.title = ggplot2::element_text(size=11),
                   title = ggplot2::element_text(size=11)) +

    # lines
    ggplot2::geom_line(size=1.5) +

    # y axis
    ggplot2::scale_y_continuous(breaks=seq(0,1,0.25),limits = c(0,1),name = "Probability most cost-effective") +

    # labels
    ggplot2::xlab(label = "Willingness-to-pay (GBP)")+

    ggplot2::labs(title = "Cost Effectiveness Acceptability Curves",
         subtitle = "The probability each preferred intervention is most cost effective against willingness to pay for each QALY threshold.") +

    # apply color scheme
    ggplot2::scale_color_manual(name="Treatment",values = legend_colors) +
    NULL



}




