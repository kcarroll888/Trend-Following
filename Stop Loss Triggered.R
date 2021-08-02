openPos <- c("AAPL", "ADM", "ADN.L", "ATK.L", "CBK.DE", "CNA.L", "DNLM.L", "ERM.L", "ETO.L",
             "GKN.L", "HSIC", "HP", "IBM", "IMI.L", "IPF.L", "ITRK.L", "LLOY.L", "LHA.DE", "MBLY",
             "NTG.L", "NXP", "PAG.L", "POP.MC", "RB.L", "RTO", "SYMC", "TDC", "TSCO.L", "TRV", "VWS.CO")

closeOut <- data.frame(1, 1)
colnames(closeOut) <- c("Symbol", "RtnStdDev")
index <- 1
for(symb in openPos){
    print(symb)
    sd <- returnSD(symb)
    if(sd >= 2){
        closeOut[index, 1] <- symb
        closeOut[index, 2] <- sd
        index <- index + 1
    }
}