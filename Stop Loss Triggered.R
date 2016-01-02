openPos <- c("ADBE", "ADM", "ADN.L", "AMEAS.HE", "ATK.L", "BBVA.MC", "BDEV.L",
             "BRBY.L", "BT-A.L", "CAT", "CBK.DE", "CNA.L", "COLR.BR", "CS.PA",
             "DE", "DNLM.L", "EDF.PA", "ERM.L", "ETO.L", "GKN.L", "HD", "HOME.L",
             "HSIC", "IBM", "IMI.L", "IPF.L", "ITRK.L", "JNJ", "KKR", "LLOY.L",
             "LOW", "NTG.L", "PAG.L", "PCAR", "POP.MC", "PUB.PA", "RB.L", "REE.MC",
             "RYA.L", "SGSN.VX", "SIRI", "SKY.L", "SU.PA", "SYMC", "TALK.L",
             "TCG.L", "TDC", "TRV", "TSCO.L", "UNP", "VOD.L", "VWS.CO", "WMT", "YUM")

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