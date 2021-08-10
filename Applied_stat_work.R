
#Read in Data ---------------------
nig.eco = read.csv("C:\\Users\\Porl-Timi\\Desktop\\Others work\\Lanre\\Applied Statistics\\Lanre Dataset.csv")
View(nig.eco)


# Data Visualization - Histogram and Box plot for revenue and expenditure ---------------------------
hist(nig.eco$Total.Federally.Collected.Revenue,col = 'blue', xlab = "Revenue", main = "Histogram of Federally Collected Revenue")
boxplot(nig.eco$Total.Federally.Collected.Revenue,col = 'blue', xlab = "Revenue", main = "Histogram of Federally Collected Revenue")

hist(nig.eco$Total.Expenditure,col = 'blue', xlab = "Revenue", main = "Histogram of Total Expenditure")
boxplot(nig.eco$Total.Expenditure,col = 'blue', xlab = "Revenue", main = "Histogram of Total Expenditure")

#Correlation matrix ----------------------
library(ggplot2)
library(reshape2)
library(dplyr)
nig.eco2 = rename(nig.eco, Total.Revenue = Total.Federally.Collected.Revenue)
View(nig.eco2)
cordata = nig.eco2[,c(2,3,4,5,8,9,10,11)]
cormat<-signif(cor(cordata),2)
# Function to get the upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
  }
cormat <- get_upper_tri(cormat)
View(cormat)

melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)


library(tseries)
library(timeSeries)
library(forecast)

#Time Series Analysis --------------------
differencedSeries_totFedRev <- diff(
  log(nig.eco$Total.Federally.Collected.Revenue))
summary(differencedSeries_totFedRev)
ts.plot(differencedSeries_totFedRev)


# Unit root of differenced series -----------------------------------------

diffUnitTest <- kpss.test(differencedSeries_totFedRev, null = "Trend")
diffUnitTest

diffADF <- adf.test(differencedSeries_totFedRev)

# ARIMA model -------------------------------------------------------------

ARIMA_model <- arima(differencedSeries_totFedRev, order = c(1, 1, 2))
summary(ARIMA_model)

(1-pnorm(abs(ARIMA_model$coef)/sqrt(diag(ARIMA_model$var.coef))))*2
coeftest(ARIMA_model)

#Regression Analysis-----------
EduMod <- lm(Education ~ Total.Federally.Collected.Revenue, 
              data = nig.eco)
summary(EduMod)

HealthMod <-lm(Health ~ Total.Federally.Collected.Revenue, 
            data = nig.eco)
summary(HealthMod)

agrMod <- lm(Agriculture ~ Total.Federally.Collected.Revenue, 
             data = nig.eco)
summary(agrMod)
roadMod <- lm(Road...Construction ~ Total.Federally.Collected.Revenue, 
             data = nig.eco)
summary(roadMod)


jarque.bera.test(residuals(EduMod))
jarque.bera.test(residuals(agrMod))
jarque.bera.test(residuals(roadMod))
hist(nig.eco$Road...Construction)

#Test of Normality -------------------------
qqnorm(EduMod$residuals, main =  "Normal Q-Q Plot for Residual of Education Model"); qqline(EduMod$residuals,col=2)
qqnorm(HealthMod$residuals, main =  "Normal Q-Q Plot for Residual of Health Model"); qqline(HealthMod$residuals,col=2)
qqnorm(agrMod$residuals, main = "Normal Q-Q Plot for Residual of Agriculture Model"); qqline(agrMod$residuals,col=2)
qqnorm(roadMod$residuals, main = "Normal Q-Q Plot for Residual of Road and Construction Model"); qqline(roadMod$residuals,col=2)




