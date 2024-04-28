list.of.packages <- c("pracma")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(pracma)
library(ggplot2)

## Formel für Dichtefunktion von V und Verteilungsfunktion von V bzw. P(Gesamtrendite \in  (l,h)). Herleitung siehe Uebungsblatt 13.

Density_fun <- function(v){
  
  term_1 = -13 + 2*v
  term_2 = -16 + 2*v
  
  sum_all = pnorm(term_1) - pnorm(term_2)
  return(2/3 * sum_all)
  
}

x <- seq(4.5,10.5,by=0.05)
y <- Density_fun(x)
plot(x, y, type ="l")




Prob_dist <- function(l=-1000,h){
  
  ah = -13+2*h
  al = -13+2*l
  bh = -16+2*h
  bl = -16+2*l
  
  term_ah = ah * pnorm(ah) + exp(-(ah^2)/2)/sqrt(2*pi)
  term_al = al * pnorm(al) + exp(-(al^2)/2)/sqrt(2*pi)
  term_bh = bh * pnorm(bh) + exp(-(bh^2)/2)/sqrt(2*pi)
  term_bl = bl * pnorm(bl) + exp(-(bl^2)/2)/sqrt(2*pi)
  
  sum_all = term_ah - term_al - term_bh + term_bl
  return(1/3 * sum_all)
}

# Plots zum veranschaulichen des Unterschieds zwischen Verteilung gemäß Faltung und Normalverteilung

x=seq(4,10,by=0.05)

df <- data.frame(v=x, y = Prob_dist(h=x), z =  pnorm(x,mean= 7.25,sd = sqrt(7/16)) )

ggplot(df, aes(x=v))+
  geom_line(aes(y=y),color = "blue")+ #Verteilungsfunktion von V
  geom_line(aes(y=z),color = "red")   #Verteilungsfunktion der Normalverteilung mit \mu = E(V) und \sigma^2 = Var(V)
  
df2 <- data.frame(v=x, y = Density_fun(v=x), z = dnorm(x,mean= 7.25,sd = sqrt(7/16)) )

ggplot(df2, aes(x=v))+
  geom_line(aes(y=y),color = "blue")+  #Dichtefunktion von V
  geom_line(aes(y=z),color = "red")    #Dichtefunktion der Normalverteilung mit \mu = E(V) und \sigma^2 = Var(V)



x <- runif(100000, min = 0.06, max = 0.08)

y <- rnorm(100000, mean = 0.08, sd = 0.02)

z <- ((150 * (1+x) + 50 * (1+y)))/200 - 1

df <- data.frame(x1=seq(0.04,0.10,length = 100000), y1 = z, y2= rnorm(100000,mean=0.0725,sd=0.0066))

ggplot(df) +                    # basic graphical object
  geom_density(aes(x=y1), colour="red") +  # first layer
  geom_density(aes(x=y2), colour="green")  


plot(density(z))
plot(density(rnorm(1000000,mean=0.725,sd=0.066)))


a <- ecdf(z/200 - 1)

Rendite <- function(l, h){
  a(h) - a(l) 
}












## Gleiche Verteilungen/Dichten, bloss mit erf-function berechnet.

Density_fun_erf <- function(v){
  
  term_1 = sqrt(2)*(-6.5 + v)
  term_2 = sqrt(2)*(-8 + v)
  
  sum_all = erf(term_1)- erf(term_2)
  return(1/3 * sum_all)
  
}



Prob_dist_erf <- function(l=-1000,h){
  
  ah = sqrt(2)*(-6.5 + h)
  al = sqrt(2)*(-6.5 + l)
  bh = sqrt(2)*(-8 + h)
  bl = sqrt(2)*(-8 + l)
  
  term_ah = ah * erf(ah) + exp(-ah^2)/sqrt(pi)
  term_al = al * erf(al) + exp(-al^2)/sqrt(pi)
  term_bh = bh * erf(bh) + exp(-bh^2)/sqrt(pi)
  term_bl = bl * erf(bl) + exp(-bl^2)/sqrt(pi)
  
  sum_all = term_ah - term_al - term_bh + term_bl
  return(1/3 * 1/sqrt(2) * sum_all)
}





















