# Model_Evaluation
# Evaluating the fitted regression model 


library("boot")
library(reshape)
library(ggplot2)

# Fitting a linear model on the composite variables derived.
Wealth_pred <- read.csv("Wealth_pred.csv")
Comp_Var <- read.csv("Composite_var.csv")
#plot(Comp_Var1, pch = 15, cex = 1.3, col = "blue", main = "Wealth_Actual", ylab = "Actual_wealthy (DHS)", xlab = "Wealth_predicted(CDR)")
P2 <- plot(Wealth_pred, pch = 15, cex = 1.3, col = "blue", main = "Predicted Vs Actual", ylab = "Actual_wealth", xlab = "Predicted_wealth")
fit1 <- lm()

P1 <- ggplot(Wealth_pred, aes(x=Predicted_wealth, y= Actual_wealth), main = "DHS Versus CDR")+
    geom_point(col = "blue")#+
#stat_smooth(method = "lm", col = "red")

# Function to fit the linear model
ggplotRegression <- function (fit) {
    
    require(ggplot2)
    
    ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
        geom_point(col = "blue") +
        stat_smooth(method = "lm", col = "red") +
        labs(title = paste("R2 = ",signif(summary(fit)$adj.r.squared, 3),
                           "Intercept =",signif(fit$coef[[1]], 2),
                           " Slope =",signif(fit$coef[[2]], 2),
                           " P =",signif(summary(fit)$coef[2,4], 2)))
}

TR <- ggplotRegression(lm(Wealth_Predicted~Wealth_Actual, data = Comp_Var1))

# Evaluating model for both Rural and Urban

prettyPlot <- function(x,y,weights, xlab="CDR",ylab="DHS", dotsize=1, ...){
    scale = (scale(weights)+1.2)*dotsize
    plot(x, y, cex=scale, cex.lab=1.5, xlab=xlab,ylab=ylab,
         col=rgb(77,77,255, maxColorValue=255),pch=16,...) 
    
    m1 <- lm(y~x, w=weights)
    abline(m1, col="darkRed", lwd=2)
    
    newx  <- seq(min(x)-1, max(x)+1, 0.1)
    cf    <- predict(m1, newdata=data.frame(x=newx), interval="confidence")
    #matlines(newx,cf[,c("lwr","upr")],col="darkRed",lty=2)
}


# load data
dataset          <- read.csv("districts2010.csv", sep=",", na=".",header=TRUE, stringsAsFactors=FALSE)
cstats           <- read.csv("clusters2010.csv", sep=",", na=".",header=TRUE, stringsAsFactors=FALSE)





# Figure 9 Urban

clusters.nk <- cstats[cstats$district=="Kicukiro" | cstats$district=="Nyarugenge" | cstats$district=="Gasabo",]
# clusters.nk <- cstats[cstats$district!="Kicukiro" & cstats$district!="Nyarugenge" & cstats$district!="Gasabo",]


clusters.nk.valid <- subset(clusters.nk, !is.na(cdrWealth) & !is.na(dhsWealth))

dhsVal  <- clusters.nk.valid$dhsWealth
cdrVal  <- clusters.nk.valid$cdrWealth
weights <- clusters.nk.valid$hhWeight

par(mar=c(4,4,3,0.5)+0.1, mgp=c(1.8,.5,0), mfrow=c(1,1))
prettyPlot(cdrVal, dhsVal, weights, dotsize=.75, main="Capital Kigali", cex.main=1.5,
           xlab="predicted wealth (CDR)", ylab="Actual wealth (DHS)")

corr(cbind(dhsVal,cdrVal), w=weights)



# Figure 10 Rural

clusters <- subset(cstats, !is.na(cdrWealth) & !is.na(dhsWealth))

dhsVal   <- clusters$dhsWealth
cdrVal   <- as.numeric(clusters$cdrWealth)
weights  <- clusters$hhWeight

par(mar=c(4,4,2,2)+0.1, mgp=c(1.8,.5,0), mfrow=c(1,1))
prettyPlot(cdrVal, dhsVal, weights, dotsize=.75,main="Rural", cex.main=1.5, 
           xlab="Predicted wealth (CDR)", ylab="Actual wealth (DHS)")

P1 <- ggplot(cdrVal, dhsVal, weights,  aes(xlab="Cluster wealth (CDR)", ylab="Cluster wealth (DHS)"), main = "Rural")+
    geom_point(col = "blue")+
    stat_smooth(method = "lm", col = "red")



legend("topleft", y.intersp=1.3,
       c("Dotsize corrsponds to BTS size"), cex=1,
       col=c(rgb(77,77,255, maxColorValue=255),
             pch=c(16, 16,16,NA,NA),
             pt.cex=c(max(dotscale)*.8),
             box.lwd=0))
corr(cbind(dhsVal,cdrVal), w=weights)
