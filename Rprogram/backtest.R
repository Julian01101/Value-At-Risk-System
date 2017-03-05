#############################################################################
# Back Test
# 
# histVaR - historical VaR                    (The first user-defined window)
# histVaR2 - historical VaR                   (The second user-defined window)
# gbmv - gbmVaR               unweighted      (2 windows together)
# gbme - gbmES                unweighted      (2 windows together)
# gbmv2 - gbmVaR              weighted        (2 windows together)
# gbme2 - gbmES               weighted        (2 windows together)
# paraVaR - parametric VaR    unweighted      (The first user-defined window)
# paraES - parametric ES      unweighted      (The first user-defined window)
# paraVaR2 - parametric VaR   unweighted      (The second user-defined window)
# paraES2 - parametric ES     unweighted      (The second user-defined window)
# paraVaR_e - parametric VaR  weighted        (The first user-defined window)
# paraES_e - parametric ES    weighted        (The first user-defined window)
# paraVaR_e2 - parametric VaR weighted        (The second user-defined window)
# paraES_e2 - parametric ES   weighted        (The second user-defined window)
# mcVaR - monteCarlo VaR      unweighted      (The first user-defined window)
# mcES - monteCarlo ES        unweighted      (The first user-defined window)
# mcVaR2 - monteCarlo VaR     unweighted      (The second user-defined window)
# mcES2 - monteCarlo ES       unweighted      (The second user-defined window)
##############################################################################
source("ActualLoss.R")
source("Exceptions.R")
library(qpcR)

horiz = horizon*252
actLoss <- ActualLoss(s0,horiz,portValue)

# deal with different number of rows before combine all VaRs into a dataframe
maxLen = tradedays

VaRSummary <- qpcR:::cbind.na(data.frame(df$Date[1:tradedays]),
                              data.frame(actLoss),
                              data.frame(mcVaR),     # win1
                              data.frame(histVaR), # win1
                              data.frame(gbmv[,2]), # win1
                              data.frame(gbmv2[,2]),# win1-weighted
                              data.frame(paraVaR), # win1
                              data.frame(paraVaR_e), # win1-weighted
                              data.frame(mcVaR2),     # win2
                              data.frame(histVaR2),  # win2
                              data.frame(gbmv[,3]),   # win2
                              data.frame(gbmv2[,3]),  # win2-weighted
                              data.frame(paraVaR2),   # win2
                              data.frame(paraVaR_e2)) # win2-weighted
                              
colnames(VaRSummary)<-c("Date",
            "Actual Loss",
            paste("McVaR (Norm)",win_yr[1],sep = " "),
            paste("HistVaR",win_yr[1],sep = " "),
            paste("gbmVaR",win_yr[1],sep = " "),
            paste("gbmVaR_e",win_yr[1],sep = " "),
            paste("parametricVaR",win_yr[1],sep = " "),
            paste("parametricVaR_e",win_yr[1],sep = " "),
            paste("McVaR (Norm)",win_yr[2],sep = " "),
            paste("HistVaR",win_yr[2],sep = " "),
            paste("gbmVaR",win_yr[2],sep = " "),
            paste("gbmVaR_e",win_yr[2],sep = " "),
            paste("parametricVaR",win_yr[2],sep = " "),
            paste("parametricVaR_e",win_yr[2],sep = " "))
          
  
ESSummary <- qpcR:::cbind.na(data.frame(df$Date[1:tradedays]),
                              data.frame(actLoss),
                              data.frame(mcES),  # win1
                              data.frame(histES), # win1
                              data.frame(gbme[,2]), # win1
                              data.frame(gbme2[,2]),# win1-weighted
                              data.frame(paraES), # win1
                              data.frame(paraES_e), # win1-weighted
                              data.frame(mcES2),     # win2
                              data.frame(histES2),  # win2
                              data.frame(gbme[,3]),   # win2
                              data.frame(gbme2[,3]),  # win2-weighted
                              data.frame(paraES2),   # win2
                              data.frame(paraES_e2)) # win2-weighted
                                
colnames(ESSummary)<-c("Date",
                        "Actual Loss",
                        paste("McES (Norm)",win_yr[1],sep = " "),
                        paste("HistES",win_yr[1],sep = " "),
                        paste("gbmES",win_yr[1],sep = " "),
                        paste("gbmES_e",win_yr[1],sep = " "),
                        paste("parametricES",win_yr[1],sep = " "),
                        paste("parametricES_e",win_yr[1],sep = " "),
                        paste("McES (Norm)",win_yr[2],sep = " "),
                        paste("HistES",win_yr[2],sep = " "),
                        paste("gbmES",win_yr[2],sep = " "),
                        paste("gbmES_e",win_yr[2],sep = " "),
                        paste("parametricES",win_yr[2],sep = " "),
                        paste("parametricES_e",win_yr[2],sep = " "))
                      

# Calculate Exceptions
excep1 <- apply(VaRSummary[,3:8],2,Exceptions,loss = VaRSummary[,2])
excep2 <- apply(VaRSummary[,9:dim(excep2)[2]],2,Exceptions,loss = VaRSummary[,2])
excep3 <- apply(ESSummary[,3:8],2,Exceptions,loss = ESSummary[,2])
excep4 <- apply(ESSummary[,9:dim(excep4)[2]],2,Exceptions,loss = ESSummary[,2])

