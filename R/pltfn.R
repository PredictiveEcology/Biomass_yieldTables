pltfn <- function(AGB, numPlots, numEcoregionGroups, prevalence, plain = NULL, page = 1L, nPages = 1L, onlyTables = NULL,
                  colours = NULL) {
  if (numPlots <= 0 || numEcoregionGroups <= 0) {
    stop("numPlots and numEcoregionGroups need to be positive integers")
  }
  prevalence <- prevalence[yieldTableIndex %in% unique(AGB$yieldTableIndex)]
  if (!is.null(onlyTables)) {
    prevalence <- prevalence[yieldTableIndex %in% onlyTables]
  }
  topGroups <- prevalence[, .(nPixels = sum(nPixels)), by = "ecoregionGroup"] |>
    setorderv("nPixels", order = -1L)
  topGroups <- head(topGroups$ecoregionGroup, numEcoregionGroups)
  perGroup <- max(1L, as.integer(numPlots) %/% length(topGroups))
  pick <- prevalence[ecoregionGroup %in% topGroups] |>
    setorderv(c("ecoregionGroup", "nPixels"), order = c(1L, -1L))
  pick <- pick[, head(.SD, perGroup), by = "ecoregionGroup"]
  # split the panels over `nPages` pages so each stays legible
  pick <- pick[ceiling(seq_len(.N) / ceiling(.N / nPages)) == page]
  pick[, panel := paste0(ecoregionGroup, "--pg", yieldTableIndex)]
  slim <- function(d) d[yieldTableIndex %in% pick$yieldTableIndex,
                        .(yieldTableIndex, speciesCode, age, AGB = biomass)]
  if (is.null(plain)) {
    id2 <- merge(slim(AGB), pick[, .(yieldTableIndex, panel)], by = "yieldTableIndex")
    id2[, panel := factor(panel, levels = pick$panel)]
    ncol <- NULL
  } else {
    # each yieldTableIndex gets its plain table on the left and the head-start one on the right
    both <- rbindlist(list(slim(plain)[, version := "plain"], slim(AGB)[, version := "head start"]))
    id2 <- merge(both, pick[, .(yieldTableIndex, panel)], by = "yieldTableIndex")
    id2[, panel := paste0(panel, " (", version, ")")]
    lev <- as.vector(rbind(paste0(pick$panel, " (plain)"), paste0(pick$panel, " (head start)")))
    id2[, panel := factor(panel, levels = lev)]
    ncol <- 4L
  }
  gg <- ggplot(id2, aes(age, AGB, color = speciesCode)) + geom_line() + theme_bw() +
    facet_wrap(~panel, ncol = ncol) + theme(strip.text = element_text(size = 7))
  if (length(colours)) {
    gg <- gg + scale_color_manual(values = colours, breaks = intersect(names(colours), unique(id2$speciesCode)))
  }
  return(invisible(gg))
}

## Stratified random choice of `n` multi-species yield tables: first one table per species (so every species with a
## competitor appears at least once), then random fill. Uses a local seed so the session RNG stream is untouched.
pickCompetitionTables <- function(AGB, n = 12L, seed = 1L) {
  nSpp <- AGB[, .(nSpp = uniqueN(speciesCode)), by = "yieldTableIndex"]
  mixed <- AGB[yieldTableIndex %in% nSpp[nSpp > 1L]$yieldTableIndex,
               .(yieldTableIndex, speciesCode)] |> unique()
  if (!nrow(mixed)) {
    return(integer(0))
  }
  oldSeed <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv())
  on.exit(if (is.null(oldSeed)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", oldSeed, globalenv()))
  set.seed(seed)
  chosen <- integer(0)
  for (sp in sample(unique(mixed$speciesCode))) {
    cand <- setdiff(mixed[speciesCode == sp]$yieldTableIndex, chosen)
    if (length(cand) && !any(mixed[yieldTableIndex %in% chosen & speciesCode == sp, .N])) {
      chosen <- c(chosen, cand[sample.int(length(cand), 1L)])
    }
  }
  rest <- setdiff(unique(mixed$yieldTableIndex), chosen)
  nFill <- max(0L, min(as.integer(n) - length(chosen), length(rest)))
  if (nFill) {
    chosen <- c(chosen, rest[sample.int(length(rest), nFill)])
  }
  head(chosen, max(as.integer(n), length(unique(mixed$speciesCode))))
}

## Species growing with competition: a stratified random sample of multi-species yield tables (see
## `pickCompetitionTables`), one panel each, labelled with the ecoregion group.
pltfnCompetition <- function(AGB, prevalence, n = 12L, colours = NULL, seed = 1L) {
  ids <- pickCompetitionTables(AGB, n = n, seed = seed)
  if (!length(ids)) {
    stop("No yield table has more than one species; there is no competition to plot")
  }
  lab <- unique(prevalence[yieldTableIndex %in% ids, .(yieldTableIndex, ecoregionGroup)], by = "yieldTableIndex")
  d <- AGB[yieldTableIndex %in% ids, .(yieldTableIndex, speciesCode, age, AGB = biomass)]
  d <- merge(d, lab, by = "yieldTableIndex", all.x = TRUE)
  d[, panel := factor(paste0(ecoregionGroup, "--pg", yieldTableIndex),
                      levels = unique(paste0(ecoregionGroup, "--pg", yieldTableIndex)[order(ecoregionGroup, yieldTableIndex)]))]
  gg <- ggplot(d, aes(age, AGB, color = speciesCode)) + geom_line() + theme_bw() +
    facet_wrap(~panel, ncol = 4L) + theme(strip.text = element_text(size = 7))
  if (length(colours)) {
    gg <- gg + scale_color_manual(values = colours, breaks = intersect(names(colours), unique(d$speciesCode)))
  }
  invisible(gg)
}

mapGcId <- function(yieldTablesId, rasterToMatch) {
  mapRast <- rast(rasterToMatch)
  mapRast[] <- NA
  mapRast[yieldTablesId$pixelIndex] <- yieldTablesId$yieldTableIndex
  return(mapRast)
}
