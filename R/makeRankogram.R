#' Function to make a Rankogram plot from matrices of total costs and QALYs
#' @param total_costs A matrix of costs for PSA runs.
#' @param total_qalys A matrix of QALYs for PSA runs.
#' @param strategy_names   A vector of strategy names (must match matrix column names).
#' @return a Rankogram produced in ggplot.
#' @examples
#' makeRankogram(total_costs = darkpeak::example_TC,
#'               total_qalys = darkpeak::example_TQ,
#'               strategy_names = c("notreat","treat 1","treat 4","treat 2", "treat 3"),
#'               lambda = 30000)
#' @importFrom reshape2 melt ggplot2 ggplot aes geom_abline geom_line geom_vline geom_hline theme_minimal theme labs scale_color_manual scale_y_continuous stat_ellipse guides


makeRankogram = function (total_costs = NULL,
                          total_qalys = NULL,
                          strategy_names = NULL,
                          lambda = 30000,
                          currency = "GBP")
{

  # get length of strategy names
  n_strategyNames = length(strategy_names)

  # calculate incremental costs and benefits
  TC =   total_costs[, strategy_names]
  TQ =   total_qalys[, strategy_names]

  # return a placeholder if there are less than three strategies.
  if(ncol(TC)<3 | ncol(TC)<3){
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::geom_label(aes(x = 1,y = 1),
                   label = "Two or fewer strategies, \n rankagram not necessary",
                   label.size = 0,
                   size = 12)
    )
  } else{

    #  =====  #
    # COLOURS #
    #  =====  #

    # define the colors based on user input
    legend_colors =  seq(from = 0.9,
                          to = 0.1,
                          length.out = ncol(TC))

    plot_cols = c()

    for (x in 1:ncol(TC)) {

      plot_cols = c(plot_cols, grDevices::rgb(0,legend_colors[x],0))

    }


    # match colors to treatments/strategies
    # names(legend_colors) = colnames(TC)
    # legend_colors = legend_colors[names(legend_colors) %in% strategy_names]

    #  =====  #
    #  CALCS  #
    #  =====  #

    # calculate incremental net benefit
    NB = TQ * lambda - TC

    # calculate rank of Net Benefit for each PSA run
    rankNB = t(apply(NB,1,FUN = function(x){rank(-x)}))

    # create empty rankogram matrix
    rankogram <- matrix(NA,
                        nrow = ncol(rankNB),
                        ncol = ncol(rankNB),
                        dimnames = list(strategy_names,
                                        c(1:n_strategyNames)))
    # fill rankogram matrix
    # for each strategy
    for(s in 1:ncol(rankogram)){
      # for each rank
      for(r in 1:ncol(rankogram)){

        # what proportion of PSA runs are each rank.
        rankogram[s,r] <- sum(rankNB[,s] == r)/length(rankNB[,s])

      }}



    #  =====  #
    #   PLOT  #
    #  =====  #

    # reshape to be long
    df <- reshape2::melt(rankogram) ; colnames(df) = c("Strategy","Rank","Prop")
    # change to be factor
    df$Rank <- factor(df$Rank)


    return(
      # ggplot with proportion on y axis and ran on x axis = color is Strategy specific
      ggplot2::ggplot(df, aes(x = Rank, y = Prop, fill = Rank)) +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.title.y = element_text(size = 14),
              axis.title.x = element_text(size = 14)) +
        # bar plot by identify
        ggplot2::geom_bar(stat = "identity") +
        # y label
        ggplot2::ylab("Proportion of PSA runs")+
        # make percent
        ggplot2::scale_y_continuous(labels = scales::percent)+
        # grid by stragegy
        ggplot2::facet_grid( ~ Strategy, switch = "x") +
        # fill is by identity
        ggplot2::scale_fill_manual(values = plot_cols)
    )

  } # end if

} # end function


