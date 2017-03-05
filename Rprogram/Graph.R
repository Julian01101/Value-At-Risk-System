# Window Set 1
VaRSummary <- VaRSummary[1:(tradedays-450),]
# VaR Plot
plotTitle <- ""
plotTitle <- paste("VaR",win_yr[1],sep=" ")
plotTitle <- paste(plotTitle,hor,sep = " ")
plotTitle <- paste(plotTitle,as.character(VaRp),sep = " ")
matplot(VaRSummary$Date,VaRSummary[,3:8],type = "l",col = 1:6,lty = 1,
        axes=F,xlab = "Date",ylab = "Value at Risk",main = plotTitle)
axis(1, at=VaRSummary$Date[seq(1,tradedays,500)],labels = VaRSummary$Date[seq(1,tradedays,500)])
axis(2, at=seq(-3000,3000,400))
legend("topright",y = colnames(VaRSummary)[3:8],cex = 0.5,lty = 1, col = 1:6)

# VaR vs Losses
plotTitle <- paste(plotTitle,"vs Losses",sep = " ")

matplot(VaRSummary$Date,VaRSummary[,2:8],type = "l",col = 1:7,lty = 1,
        axes=F,xlab = "Date",ylab = "Loss",main = plotTitle)
axis(1, at=VaRSummary$Date[seq(1,tradedays,500)],labels = VaRSummary$Date[seq(1,tradedays,500)])
axis(2, at=seq(-3000,3000,400))
legend("topright",y = colnames(VaRSummary)[2:8],cex = 0.5,lty = 1, col = 1:7)

# Exceptions
plotTitle <- ""
plotTitle <- paste("VaR",win_yr[1],sep=" ")
plotTitle <- paste(plotTitle,hor,sep = " ")
plotTitle <- paste(plotTitle,as.character(VaRp),sep = " ")
plotTitle <- paste(plotTitle,"Exceptions",sep = " ")

matplot(VaRSummary$Date[1:(dim(excep1)[1])],excep1[,1:(dim(excep1)[2]-1)],type = "l",col = 1:6,lty = 1,
        axes=F,xlab = "Date",ylab = "Number of Exceptions",main = plotTitle)
axis(1, at=VaRSummary$Date[seq(1,tradedays,500)],labels = VaRSummary$Date[seq(1,tradedays,500)])
axis(2, at=seq(0,120,20))
legend("topright",y = colnames(excep1)[1:(dim(excep1)[2]-1)],cex = 0.5,lty = 1, col = 1:6)

# VaR Window Set 2
# VaR Plot
plotTitle <- ""
plotTitle <- paste("VaR",win_yr[2],sep=" ")
plotTitle <- paste(plotTitle,hor,sep = " ")
plotTitle <- paste(plotTitle,as.character(VaRp),sep = " ")
matplot(VaRSummary$Date,VaRSummary[,9:14],type = "l",col = 1:6,lty = 1,
        axes=F,xlab = "Date",ylab = "Value at Risk",main = plotTitle)
axis(1, at=VaRSummary$Date[seq(1,tradedays,500)],labels = VaRSummary$Date[seq(1,tradedays,500)])
axis(2, at=seq(-3000,3000,400))
legend("topright",y = colnames(VaRSummary)[9:14],cex = 0.5,lty = 1, col = 1:6)

# VaR vs Losses
plotTitle <- paste(plotTitle,"vs Losses",sep = " ")

matplot(VaRSummary$Date,VaRSummary[,c(2,9:14)],type = "l",col = 1:7,lty = 1,
        axes=F,xlab = "Date",ylab = "Loss",main = plotTitle)
axis(1, at=VaRSummary$Date[seq(1,tradedays,500)],labels = VaRSummary$Date[seq(1,tradedays,500)])
axis(2, at=seq(-3000,3000,400))
legend("topright",y = colnames(VaRSummary)[c(2,9:14)],cex = 0.5,lty = 1, col = 1:7)

# Exceptions
plotTitle <- ""
plotTitle <- paste("VaR",win_yr[2],sep=" ")
plotTitle <- paste(plotTitle,hor,sep = " ")
plotTitle <- paste(plotTitle,as.character(VaRp),sep = " ")
plotTitle <- paste(plotTitle,"Exceptions",sep = " ")

matplot(VaRSummary$Date[1:(dim(excep2)[1])],excep2[,1:(dim(excep2)[2]-1)],type = "l",col = 1:6,lty = 1,
        axes=F,xlab = "Date",ylab = "Number of Exceptions",main = plotTitle)
axis(1, at=VaRSummary$Date[seq(1,tradedays,500)],labels = VaRSummary$Date[seq(1,tradedays,500)])
axis(2, at=seq(0,20,5))
legend("topright",y = colnames(excep1)[1:(dim(excep1)[2]-1)],cex = 0.5,lty = 1, col = 1:6)


ESSummary <- VaRSummary[1:(tradedays-450),]
# ES Plot
plotTitle <- ""
plotTitle <- paste("ES",win_yr[1],sep=" ")
plotTitle <- paste(plotTitle,hor,sep = " ")
plotTitle <- paste(plotTitle,as.character(VaRp),sep = " ")
matplot(VaRSummary$Date,VaRSummary[,3:8],type = "l",col = 1:6,lty = 1,
        axes=F,xlab = "Date",ylab = "Expected Shortfall",main = plotTitle)
axis(1, at=ESSummary$Date[seq(1,tradedays,500)],labels = ESSummary$Date[seq(1,tradedays,500)])
axis(2, at=seq(-3000,3000,400))
legend("topright",y = colnames(VaRSummary)[3:8],cex = 0.5,lty = 1, col = 1:6)
