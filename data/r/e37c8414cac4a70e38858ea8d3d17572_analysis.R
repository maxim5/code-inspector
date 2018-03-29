library("doBy")
library("lattice")
library("grid")

# Helper to write chart output
chart <- function(name, FUNC, ...) {
  pdf(paste("charts/", name, ".pdf", sep=""), ...)
  FUNC()
  dev.off()
}

# Generate a filter for a table
filter.table <- function(tbl, ...) {
  criteria <- list(...)
  match <- TRUE
  for (n in names(criteria)) {
    match = match & tbl[[n]] == criteria[[n]]
  }
  match
}

uu.agg <- function(data) {
  summaryBy(BuildTime + TestTime + Coverage
            + nDCG + HLUtility + MAE + RMSE.ByRating + RMSE.ByUser
            ~ Algorithm + nnbrs + Norm + Adj + sim,
            data, FUN=mean)
}

uu.table <- function(ds) {
  get(paste("useruser", ds, "agg", sep="."))
}

# Analyze user-user results

useruser.1m <- read.csv("eval-output/useruser.ml-1m.csv")
useruser.1m.agg <- uu.agg(useruser.1m)

uu.1m.subset <- useruser.1m.agg$sim == "Cosine" | useruser.1m.agg$sim == "CosineNorm"
uu.1m.subset <- uu.1m.subset & useruser.1m.agg$Adj == "None"
uu.1m.subset <- uu.1m.subset | ((useruser.1m.agg$sim == "Pearson"
                                 | useruser.1m.agg$sim == "Spearman")
                                & useruser.1m.agg$Adj == "Weight 50")

chart("useruser-sim-nnbrs", width=7, height=3, function() {
  uu.1m.mae <- xyplot(nDCG.mean ~ nnbrs,
                      useruser.1m.agg, subset=uu.1m.subset,
                      groups=sim, auto.key=list(columns=2), type=c("l","p"))
  uu.1m.rmse <- xyplot(RMSE.ByRating.mean ~ nnbrs,
                       useruser.1m.agg, subset=uu.1m.subset,
                       groups=sim, auto.key=list(columns=2), type=c("l","p"))
  print(uu.1m.mae, position=c(0,0,0.5,1), more=TRUE)
  print(uu.1m.rmse, position=c(0.5,0,1,1))
})

uu.1m.forsim <- function(v, s) {
  sel <- uu.1m.subset & useruser.1m.agg$sim == s
  v[sel][order(useruser.1m.agg$nnbrs[sel])]
}

attach(useruser.1m.agg)
uu.mae.tbl <- data.frame(nnbrs=sort(unique(nnbrs[uu.1m.subset])),
                         Cosine=uu.1m.forsim(MAE.mean, "Cosine"),
                         CosineNorm=uu.1m.forsim(MAE.mean, "CosineNorm"),
                         Pearson=uu.1m.forsim(MAE.mean, "Pearson"),
                         Spearman=uu.1m.forsim(MAE.mean, "Spearman"))
uu.rmse.tbl <- data.frame(nnbrs=sort(unique(nnbrs[uu.1m.subset])),
                         Cosine=uu.1m.forsim(RMSE.ByRating.mean, "Cosine"),
                         CosineNorm=uu.1m.forsim(RMSE.ByRating.mean, "CosineNorm"),
                         Pearson=uu.1m.forsim(RMSE.ByRating.mean, "Pearson"),
                         Spearman=uu.1m.forsim(RMSE.ByRating.mean, "Spearman"))
uu.ndcg.tbl <- data.frame(nnbrs=sort(unique(nnbrs[uu.1m.subset])),
                         Cosine=uu.1m.forsim(nDCG.mean, "Cosine"),
                         CosineNorm=uu.1m.forsim(nDCG.mean, "CosineNorm"),
                         Pearson=uu.1m.forsim(nDCG.mean, "Pearson"),
                         Spearman=uu.1m.forsim(nDCG.mean, "Spearman"))
detach(useruser.1m.agg)

write.table(uu.mae.tbl, file="useruser-sim-nnbrs-mae.lst", row.names=FALSE)
write.table(uu.rmse.tbl, file="useruser-sim-nnbrs-rmse.lst", row.names=FALSE)
write.table(uu.ndcg.tbl, file="useruser-sim-nnbrs-ndcg.lst", row.names=FALSE)
             
itemitem.1m <- read.csv("eval-output/itemitem.ml-1m.csv")
itemitem.1m.agg <-
  summaryBy(nDCG + HLUtility + MAE + RMSE.ByRating + RMSE.ByUser + BuildTime
            ~ Adj + Norm + nnbrs + sim + Baseline,
            itemitem.1m, FUN=mean)

itemitem.1m.agg$Sim2 <- paste(itemitem.1m.agg$sim, itemitem.1m.agg$Norm,
                              sep=" - ")

ii.1m.subset <- itemitem.1m.agg$Adj == "None"

chart("itemitem-sim-nnbrs", width=7, height=3, function() {
  ii.1m.mae <- xyplot(MAE.mean ~ nnbrs,
                      itemitem.1m.agg, subset=ii.1m.subset,
                      groups=Sim2, auto.key=list(columns=2), type=c("l","p"))
  ii.1m.rmse <- xyplot(RMSE.ByRating.mean ~ nnbrs,
                       itemitem.1m.agg, subset=ii.1m.subset,
                       groups=Sim2, auto.key=list(columns=2), type=c("l","p"))
  print(ii.1m.mae, position=c(0,0,0.5,1), more=TRUE)
  print(ii.1m.rmse, position=c(0.5,0,1,1))
})

ii.1m.forsim <- function(v, s, n) {
  sel <- ii.1m.subset & itemitem.1m.agg$sim == s
  if (!is.na(n)) {
    sel <- sel & itemitem.1m.agg$Norm == n
  }
  v[sel][order(itemitem.1m.agg$nnbrs[sel])]
}

attach(itemitem.1m.agg)
ii.mae.tbl <-
  data.frame(nnbrs=sort(unique(nnbrs[ii.1m.subset])),
             "Pearson"=ii.1m.forsim(MAE.mean, "Pearson", NA),
             "Cosine"=ii.1m.forsim(MAE.mean, "Cosine", "None"),
             "Cosine (User)"=ii.1m.forsim(MAE.mean, "Cosine", "UserMean"),
             "Cosine (Item)"=ii.1m.forsim(MAE.mean, "Cosine", "ItemMean"),
             "Cosine (ItemUser)"=ii.1m.forsim(MAE.mean,
               "Cosine", "ItemUserMean"))
ii.rmse.tbl <-
  data.frame(nnbrs=sort(unique(nnbrs[ii.1m.subset])),
             "Pearson"=ii.1m.forsim(RMSE.ByRating.mean, "Pearson", NA),
             "Cosine"=ii.1m.forsim(RMSE.ByRating.mean, "Cosine", "None"),
             "Cosine (User)"=ii.1m.forsim(RMSE.ByRating.mean, "Cosine", "UserMean"),
             "Cosine (Item)"=ii.1m.forsim(RMSE.ByRating.mean, "Cosine", "ItemMean"),
             "Cosine (ItemUser)"=ii.1m.forsim(RMSE.ByRating.mean,
               "Cosine", "ItemUserMean"))
ii.ndcg.tbl <-
  data.frame(nnbrs=sort(unique(nnbrs[ii.1m.subset])),
             "Pearson"=ii.1m.forsim(nDCG.mean, "Pearson", NA),
             "Cosine"=ii.1m.forsim(nDCG.mean, "Cosine", "None"),
             "CosineUser"=ii.1m.forsim(nDCG.mean, "Cosine", "UserMean"),
             "CosineItem"=ii.1m.forsim(nDCG.mean, "Cosine", "ItemMean"),
             "CosineItemUser"=ii.1m.forsim(nDCG.mean,
               "Cosine", "ItemUserMean"))
ii.col.names <- c("n", "Pearson", "Cosine", "CosineUser",
                  "CosineItem", "CosineItemUser")
detach(itemitem.1m.agg)

write.table(ii.mae.tbl, file="itemitem-sim-nnbrs-mae.lst", row.names=FALSE,
            col.names=ii.col.names)
write.table(ii.rmse.tbl, file="itemitem-sim-nnbrs-rmse.lst", row.names=FALSE,
            col.names=ii.col.names)
write.table(ii.ndcg.tbl, file="itemitem-sim-nnbrs-ndcg.lst", row.names=FALSE,
            col.names=ii.col.names)

funksvd.100k <- read.csv("eval-output/funksvd.ml-100k.csv")
funksvd.100k.agg <-
  summaryBy(nDCG + HLUtility + MAE + RMSE.ByRating + RMSE.ByUser + BuildTime
            ~ Smoothing + Clamping + Baseline + IterationCount + k + Reg + LRate,
            funksvd.100k, FUN=mean)

funksvd.1m <- read.csv("eval-output/funksvd.ml-1m.csv")
funksvd.1m.agg <-
  summaryBy(nDCG + HLUtility + MAE + RMSE.ByRating + RMSE.ByUser + BuildTime
            ~ Smoothing + Clamping + Baseline + IterationCount + k + Reg + LRate,
            funksvd.1m, FUN=mean)

funksvd.full <-
  rbind(cbind(subset(funksvd.100k.agg,
                     funksvd.100k.agg$Clamping == "Range"
                     & funksvd.100k.agg$LRate == 0.001),
              Run="100Kl0.001"),
        cbind(subset(funksvd.100k.agg,
                     funksvd.100k.agg$Clamping == "Range"
                     & funksvd.100k.agg$LRate == 0.002),
              Run="100Kl0.002"),
        cbind(subset(funksvd.1m.agg,
                     funksvd.1m.agg$Clamping == "Range"
                     & funksvd.1m.agg$LRate == 0.001),
              Run="1M"))

chart("funk-k", width=7, height=3, function() {
  mae <- xyplot(MAE.mean ~ k,
                funksvd.full,
                groups=Run, auto.key=list(columns=2), type=c("l","p"))
  rmse <- xyplot(RMSE.ByRating.mean ~ k,
                 funksvd.full,
                 groups=Run, auto.key=list(columns=2), type=c("l","p"))
  print(mae, position=c(0,0,0.5,1), more=TRUE)
  print(rmse, position=c(0.5,0,1,1))
})

attach(funksvd.full)
funk.mae.tbl <-
  data.frame(k=sort(unique(k)),
             K100L1=MAE.mean[Run=="100Kl0.001"][order(k[Run=="100Kl0.001"])],
             K100L2=MAE.mean[Run=="100Kl0.002"][order(k[Run=="100Kl0.002"])],
             M1=MAE.mean[Run == "1M"][order(k[Run == "1M"])])
funk.rmse.tbl <-
  data.frame(k=sort(unique(k)),
             K100L1=RMSE.ByRating.mean[Run=="100Kl0.001"][order(k[Run=="100Kl0.001"])],
             K100L2=RMSE.ByRating.mean[Run=="100Kl0.002"][order(k[Run=="100Kl0.002"])],
             M1=RMSE.ByRating.mean[Run == "1M"][order(k[Run == "1M"])])
funk.col.names <- c("k", "100Kl1", "100Kl2", "1M")
detach(funksvd.full)

write.table(funk.mae.tbl, file="funk-mae.lst", row.names=FALSE,
            col.names=funk.col.names)
write.table(funk.rmse.tbl, file="funk-rmse.lst", row.names=FALSE,
            col.names=funk.col.names)


ii.sel <- (ii.1m.subset
           & itemitem.1m.agg$nnbrs == 30
           & itemitem.1m.agg$Norm == "ItemUserMean")

uu.sel <- (uu.1m.subset
           & useruser.1m.agg$nnbrs == 50
           & useruser.1m.agg$sim == "CosineNorm"
           & useruser.1m.agg$Adj == "None")

svd.sel <- (funksvd.1m.agg$Clamping == "Range"
            & funksvd.1m.agg$LRate == 0.001
            & funksvd.1m.agg$k == 30)

comparison <-
  rbind(data.frame(type="User-User",
              MAE=useruser.1m.agg$MAE.mean[uu.sel],
              RMSE=useruser.1m.agg$RMSE.ByRating.mean[uu.sel],
              nDCG=useruser.1m.agg$nDCG.mean[uu.sel]),
        data.frame(type="Item-Item",
              MAE=itemitem.1m.agg$MAE.mean[ii.sel],
              RMSE=itemitem.1m.agg$RMSE.ByRating.mean[ii.sel],
              nDCG=itemitem.1m.agg$nDCG.mean[ii.sel]),
        data.frame(type="FunkSVD",
              MAE=funksvd.1m.agg$MAE.mean[svd.sel],
              RMSE=funksvd.1m.agg$RMSE.ByRating.mean[svd.sel],
              nDCG=funksvd.1m.agg$nDCG.mean[svd.sel]))

chart("all-1m", width=7, height=3, function() {
  mae <- barchart(MAE ~ type, comparison,
                  scales=list(x=list(rot=45)))
  rmse <- barchart(RMSE ~ type, comparison,
                   scales=list(x=list(rot=45)))
  ndcg <- barchart(nDCG ~ type, comparison,
                   scales=list(x=list(rot=45)))
  print(mae, position=c(0,0,0.333,1), more=TRUE)
  print(rmse, position=c(0.333,0,0.666,1), more=TRUE)
  print(ndcg, position=c(0.666,0,1,1))
})

comp.10m <- read.csv("eval-output/largedata.ml-10m.csv")
comp.10m.agg <- summaryBy(MAE.ByRating + RMSE.ByRating + nDCG ~ Algorithm, comp.10m)
chart("all-10m", width=7, height=3, function() {
  mae <- barchart(MAE.ByRating.mean ~ Algorithm, comp.10m.agg,
                  scales=list(x=list(rot=45)),
                  ylab="MAE")
  rmse <- barchart(RMSE.ByRating.mean ~ Algorithm, comp.10m.agg,
                   scales=list(x=list(rot=45)),
                   ylab="Global RMSE")
  ndcg <- barchart(nDCG.mean ~ Algorithm, comp.10m.agg,
                   scales=list(x=list(rot=45)),
                   ylab="nDCG")
  print(mae, position=c(0,0,0.333,1), more=TRUE)
  print(rmse, position=c(0.333,0,0.666,1), more=TRUE)
  print(ndcg, position=c(0.666,0,1,1))
})

comp.ym <- read.csv("eval-output/largedata.ymusic.csv")
comp.ym.agg <- summaryBy(MAE.ByRating + RMSE.ByRating + nDCG ~ Algorithm, comp.ym)
chart("all-ym", width=7, height=3, function() {
  mae <- barchart(MAE.ByRating.mean ~ Algorithm, comp.ym.agg,
                  scales=list(x=list(rot=45)),
                  ylab="MAE")
  rmse <- barchart(RMSE.ByRating.mean ~ Algorithm, comp.ym.agg,
                   scales=list(x=list(rot=45)),
                   ylab="Global RMSE")
  ndcg <- barchart(nDCG.mean ~ Algorithm, comp.ym.agg,
                   scales=list(x=list(rot=45)),
                   ylab="nDCG")
  print(mae, position=c(0,0,0.333,1), more=TRUE)
  print(rmse, position=c(0.333,0,0.666,1), more=TRUE)
  print(ndcg, position=c(0.666,0,1,1))
})

chart("all-merged", width=11, height=2.75, function() {
  data.1m <- cbind(comparison, set="ML-1M")
  data.10m <- data.frame(MAE=comp.10m.agg$MAE.ByRating.mean,
                         RMSE=comp.10m.agg$RMSE.ByRating.mean,
                         nDCG=comp.10m.agg$nDCG.mean,
                         type=comp.10m.agg$Algorithm,
                         set="ML-10M")
  data.ym <- data.frame(MAE=comp.ym.agg$MAE.ByRating.mean,
                         RMSE=comp.ym.agg$RMSE.ByRating.mean,
                         nDCG=comp.ym.agg$nDCG.mean,
                         type=comp.ym.agg$Algorithm,
                         set="Y! Music")
  data.merged <- rbind(data.1m, data.10m, data.ym)

  trellis.par.set(layout.heights=list(
                    bottom.padding=-3,
                    top.padding=-0.5),
                  layout.widths=list(
                    right.padding=-1))

  mae <- barchart(MAE ~ type, data.merged, groups=set,
                  scales=list(x=list(rot=45)),
                  ylab="MAE")
  rmse <- barchart(RMSE ~ type, data.merged, groups=set,
                   scales=list(x=list(rot=45)),
                   ylab="Global RMSE")
  ndcg <- barchart(nDCG ~ type, data.merged, groups=set,
                   scales=list(x=list(rot=45)),
                   ylab="nDCG")

  key.height <- unit(0.15, "in")

  key.names <- levels(data.merged$set)
  key <- simpleKey(key.names, points=FALSE, rectangles=TRUE)
  key$columns <- length(key.names)

  key.vp <- viewport(x = unit(0, "npc"),
                     y = unit(1, "npc") - key.height,
                     width = unit(1, "npc"),
                     height = key.height,
                     just=c("left", "top"))
  draw.key(key, draw=T, vp=key.vp)

  margin.adj <- unit(0, "npc")
  pushViewport(viewport(x=unit(0, "npc"),
                        y=unit(0, "npc") - margin.adj,
                        width=unit(1, "npc"),
                        height=unit(1, "npc") - key.height + margin.adj,
                        just=c("left", "bottom")))
  
  pushViewport(viewport(x=unit(0, "npc"), y=unit(0, "npc"),
                        width=unit(1/3, "npc"),
                        height=unit(1, "npc"),
                        just=c("left", "bottom")))
  print(mae, newpage=FALSE, prefix="mae")
  popViewport()
  
  pushViewport(viewport(x=unit(1/3, "npc"), y=unit(0, "npc"),
                        width=unit(1/3, "npc"),
                        height=unit(1, "npc"),
                        just=c("left", "bottom")))
  print(rmse, newpage=FALSE, prefix="rmse")
  popViewport()
  
  pushViewport(viewport(x=unit(2/3, "npc"), y=unit(0, "npc"),
                        width=unit(1/3, "npc"),
                        height=unit(1, "npc"),
                        just=c("left", "bottom")))
  print(ndcg, newpage=FALSE, prefix="ndcg")
  popViewport()

  popViewport()
})

chart("ndcg-compare", width=7, height=2.8, function() {
  uu.ndcg <- xyplot(nDCG.mean ~ nnbrs,
                    useruser.1m.agg, subset=uu.1m.subset,
                    groups=sim, auto.key=list(columns=2), type=c("l","p"),
                    main="User-User")
  ii.ndcg <- xyplot(nDCG.mean ~ nnbrs,
                    itemitem.1m.agg, subset=ii.1m.subset,
                    groups=Sim2, auto.key=list(columns=2), type=c("l","p"),
                    main="Item-Item")
  print(uu.ndcg, position=c(0,0,0.5,1), more=TRUE)
  print(ii.ndcg, position=c(0.5,0,1,1))
})
