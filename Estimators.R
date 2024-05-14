
### data is a column with the data

mle<-egamma(data,method="mle")
shape<-mle$parameters[1]
scale<-mle$parameters[2]
lambda<-1/scale
alpha<-shape


#equation 2
jarz<- -log(mean(exp(-data)))
jarz
#equation 3
FlucDiss<- mean(data)-(var(data)/2)
FlucDiss
#our proposal bade on gamma distribution
alpha*log((lambda+1)/lambda)



library(ggplot2)
df <- data.frame(PF = data)
ggplot(df, aes(x = PF)) +
  geom_histogram(aes(y =..density..),
                 breaks = seq(0, 1000, by = 100),
                 colour = "black",
                 fill = "white") +
  stat_function(fun = dgamma, args = list(shape=mle$parameters[1],scale=mle$parameters[2]),colour="blue")+
  geom_density(lwd = 0.8, colour = 2)+
  xlab("data")


library(fitdistrplus)
fg <- fitdist(data, "gamma")
par(mfrow = c(2, 2))
plot.legend <- c( "Gamma")
denscomp(fg, legendtext = plot.legend)
qqcomp(fg, legendtext = plot.legend)
cdfcomp(fg, legendtext = plot.legend)
ppcomp(fg, legendtext = plot.legend)

gofTest(data,test="ks",distribution = "gamma")


#we compute the asimptotic variance

gprima<-function(alfa,lambda) c(log((lambda+1)/lambda),-alfa/((lambda+1)*lambda))
FIsher<-function(alfa,lambda)
{
  sig11<-trigamma(alfa)
  sig22<-alfa/(lambda*lambda)
  sig12<-  -1/lambda
  cbind(c(sig11,sig12),c(sig12,sig22))
}

varianza<-function(alfa,lambda)  t(gprima(alfa,lambda))%*% solve(FIsher(alfa,lambda))%*%(gprima(alfa,lambda))

gprima(alpha,lambda)
solve(FIsher(alpha,lambda))
#asimptotic variance
varianza(alpha,lambda)/length(Datos$Wadhkt)
