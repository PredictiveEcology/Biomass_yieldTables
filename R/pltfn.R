pltfn <- function(AGB, numPlots) {
  numGroups <- length(unique(AGB$gcid))
  
  if (numPlots <= 0){
    stop("numPlots needs to be a positive integer")
  } else if (numPlots > numGroups) {
    message("numPlots is greater than the number of pixel groups, ",
            "plotting all pixelgroups.")
    numPlots <- numGroups
  }
  pullOutId <- sample(unique(AGB$gcid), size = numPlots)
  id2 <- AGB[gcid %in% pullOutId]
  setnames(id2, "biomass", "AGB")
  gg <- ggplot(id2, aes(age, AGB, color = speciesCode)) + geom_line() + theme_bw() +
    facet_wrap(~gcid)
  return(invisible(gg))
}