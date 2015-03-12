#do a pretty correlation on heat map on all Pimco Funds (institutional share class) that have existed longer 5 years
#symbol list obtained from
#http://investments.pimco.com/Products/pages/PlOEF.aspx?Level1=ulProducts&Center=ulProducts&Level2=liulProductsMutualFunds
#pasted into Excel and sorted by 5 yr return
#then copied and pasted transpose
#saved to csv
#and pasted in this ticker list eliminating the money fund and adding Vanguard S&P 500 for reference

library(quantmod)
library(latticeExtra)


tckrs <- c("PISIX","PSKIX","PSDIX","PSTKX","PCRIX","PFIIX","PHMIX","PFCIX","PCDIX","PTSHX","PFMIX","PLMIX","PSPTX","PCIMX","PSTIX","PNYIX","PLDTX","PLDIX","PTLDX","PAAIX","PXTIX","PHIYX","PSCSX","PAUIX","PTRIX","PGBIX","PFORX","PELBX","PDMIX","PMDRX","PEBIX","PDIIX","PRRSX","PMBIX","PTSAX","PTTRX","PIGLX","PRRIX","PFUIX","PIMIX","PIGIX","PRAIX","PLRIX","PGOVX","PEDIX","VFINX")

for (i in 1:length(tckrs)) {
  ifelse (i == 1,
          pimco <- get(getSymbols(tckrs[i],from="2000-01-01",adjust=TRUE))[,4],
          pimco <- merge(pimco,get(getSymbols(tckrs[i],get="all",from="2000-01-01",adjust=TRUE))[,4]))
}
#remove .close from each of the symbols
colnames(pimco) <- tckrs
pimco.clean <- na.omit(pimco)
pimco.roc <- ROC(pimco.clean,n=1,type="discrete")
pimco.roc[1,] <- 0

#do correlation table
ca <- cor(pimco.roc)

#get colors to use for heat map
brew <- brewer.pal(name="RdBu",n=5)
#get color ramp
cc.brew <- colorRampPalette(brew)
#apply color ramp
cc <- cc.brew(nrow(ca))
#do heatmap and sort by degree of correlation to VFINX (Vanguard S&P 500)

levelplot(
  ca[order(ca[,ncol(ca)]),order(ca[,ncol(ca)])]
  ,col.regions=cc
  ,colorkey=list(space="bottom")
  ,xlab = NULL
  ,ylab = NULL
  ,scales=list(x=list(rot=90,tck=c(1,0)),y=list(tck=c(1,0)))
)