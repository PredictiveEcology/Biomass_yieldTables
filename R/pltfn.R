pltfn <- function(AGB, sp, numPlots) {
  numGroups <- length(unique(sp$yieldPixelGroup))
  
  if (numPlots <= 0){
    stop("numPlots needs to be a positive integer")
  } else if (numPlots > numGroups) {
    message("numPlots is greater than the number of pixel groups, ",
            "plotting all pixelgroups.")
    numPlots <- numGroups
  }
  pullOutId <- sample(unique(AGB$yieldPixelGroup), size = numPlots)
  id2 <- AGB[yieldPixelGroup %in% pullOutId]
  setnames(id2, "B", "AGB")
  sp <- sp[yieldPixelGroup %in% pullOutId]
  id2 <- id2[sp, on = c("cohort_id", "yieldPixelGroup")]
  gg <- ggplot(id2, aes(age, AGB, color = speciesCode)) + geom_line() + theme_bw() +
    facet_wrap(~yieldPixelGroup)
  return(invisible(gg))
}