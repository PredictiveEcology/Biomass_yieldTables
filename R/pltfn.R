pltfn <- function(AGB, sp, numPlots) {
  pullOutId <- sample(1:max(AGB$pixelGroup), size = numPlots)
  id2 <- AGB[pixelGroup %in% pullOutId]
  setnames(id2, "B", "AGB")
  sp <- sp[pixelGroup %in% pullOutId]
  id2 <- id2[sp, on = c("cohort_id", "pixelGroup" = "pixelGroup")]
  gg <- ggplot(id2, aes(age, AGB, color = speciesCode)) + geom_line() + theme_bw() +
    facet_wrap(~pixelGroup)
  return(invisible(gg))
}