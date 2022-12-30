

AEplot <- function(data) {

  print(max(na.omit(data$AENDY)))


  p <- ggplot(data) +
    geom_segment(aes(x=ASTDY, xend=AENDY , y=AETERM , yend=AETERM , color=AESEV), linetype=1, size=2) +
    scale_colour_brewer(palette = "Pastel1")+
    xlab("Study Day Start/End of AE")+
    ylab("AE Preferred Term") +
    scale_x_continuous(limits=c(0,max(na.omit(data$AENDY)))) #+
    # theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank()) + theme(aspect.ratio = .2)
  p + theme(legend.position="none")

  return(p)
  }


# aePlots <- subjectProfileIntervalPlot(
#   data = dataAE,
#   paramVar = "AETERM",
#   timeStartVar = "AESTDY",
#   timeEndVar = "AEENDY",
#   colorVar = "AESEV",
#   labelVars = labelVarsSDTM,
#   title = "Adverse events"
# )
