#' @param total_costs A matrix of costs for PSA runs.
#' @param total_qalys A matrix of QALYs for PSA runs.
#' @param ref_index  Location of reference strategy.
#' @param c_i  whether or not to include confidence intervals.
#' @return a ICER table.
#' @examples
#' createICERtable(total_costs = example_TC, total_qalys = example_TQ, ref_index = 1,ci = T)
#' @importFrom DT shinyjs






createICERtable <- function(total_costs = example_TC,
                            total_qalys = example_TQ,
                            ref_index = 1,
                            ci = T){


  # store mean costs and qalys from the model output
  c_f = function(x,d=2,ci=T){
    x = c(mean(x),quantile(x,probs = c(0.025,0.975)))
    x = formatC(x,digits=d,big.mark = ",",format = "f",drop0trailing = F)
    if(ci){
      x = paste0(x[1]," (",x[2],"; ",x[3],")")
    } else {
      x = paste0(x[1])
    }

    x[grepl("0 \\(", substr(x,1,3))] = "- (-, -)"
    x[grepl("0[.]0 \\(", substr(x,1,5))] = "- (-, -)"
    x[grepl("0[.]00 \\(", substr(x,1,6))] = "- (-, -)"
    x[grepl("0[.]000 \\(", substr(x,1,7))] = "- (-, -)"

    x
  }


  # store mean costs and qalys from the model output
  mean_C = apply(total_costs,2,c_f,0,ci)
  mean_Q = apply(total_qalys,2,c_f,3,ci)

  # calculate incremental vs warfarin
  inc.C = cbind(total_costs[,1:6]-total_costs[,ref_index])
  inc.C = apply(inc.C,2,c_f,0,ci)
  inc.Q = cbind(total_qalys[,1:6]-total_qalys[,ref_index])
  inc.Q = apply(inc.Q,2,c_f,3,ci)

  NB20 = total_qalys * 20000 - total_costs
  INB20 =  NB20[,] - NB20[,1]
  INB20 = apply(INB20,2,c_f,0,ci)

  NB30 = total_qalys * 30000 - total_costs
  INB30 =  NB30[,] - NB30[,1]
  INB30 = apply(INB30,2,c_f,0,ci)

  # create results table dataframe and transpose it to be horizontal format.
  res_table = as.data.frame(
    t(    data.frame( mean_C ,inc.C ,
                          mean_Q, inc.Q ,
                          INB20,
                          INB30,
                          stringsAsFactors = F)
          )
    )
  rownames(res_table) = c("Costs", "Incremental Costs",
                          "QALYs", "Incremental QALYs",
                          "INB 20,000 GBP", "INB 30,000 GBP")
  res_table$temp = c("Costs (GBP)","Costs (GBP)",
                     "QALYs","QALYs",
                     "Incremental Net Benefit (GBP)","Incremental Net Benefit (GBP)" )


  DT::datatable(data = res_table,
            class="compact cell-border",
            # caption = "Cost effectiveness of preferred licensed products for prevention of stroke in patients with no atrial fibrillation.\n \n Incremental Values are relative to warfarin international normalised ratio (INR) 2.0-3.0",
            options = list(colReorder = TRUE,       # this allows the user to reorder the columns in the table.
                           dom = 'tB',              # this allows buttons
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           columnDefs = list(list(width = '170px', targets = 0),list(visible=FALSE, targets=7)),
                           rowGroup = list(dataSrc = 7),
                           initComplete = DT::JS("function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#D3D3D3', 'color': '#000'});",
                                             "}")),
            extensions = c('Buttons','ColReorder','RowGroup'),   # this creates the buttons and column reorder capability.
                           rownames = c("Costs", "Incremental Costs",
                                        "QALYs", "Incremental QALYs",
                                        "20,000 GBP", "30,000 GBP"))

}
