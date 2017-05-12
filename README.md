# practica-20
#buscar generar una serie de tiempo estacional (min 15 datos) y aplicar el pronostico en los dos metodos 
#de holt wintr y ademas apliquen el pronostico de tendencia exponencial y graficar para comparar


install.packages("fpp")
require(fpp)


s3<-read.csv(file.choose())
ts3<-ts(s3, start=2000, frequency=4)
ts3
plot(ts3)

c<-ts3[1:16,]
tc<-ts(c, start=2000,  frequency=4)
tc


#holt lineal exponencial
mod1<- holt(tc, alpha=.8, beta=.2, initial="simple", exponential=TRUE, h=4)
mod1
plot(mod1)

#holt winter
mod2<- hw(tc, seasonal="additive",h=4)
mod2

mod3<- hw(tc, seasonal="multiplicative",h=4)
mod3

x11()
plot(ts3, main= "Pronostico s3",  type="o", pch=17)
lines(mod1$mean, col="blue", type="o", pch=3)
lines(mod2$mean, col="red", type="o", pch=8)
lines(mod3$mean, col="orange", type="o", pch=18)
legend("topleft", lty=1, col=c(1,"blue","red","orange"), pch=c(17,3,8,18),
       legend=c("Reales",  "Exponencial", "Aditivo", "Multiplicativo")) 

#se aplica un modelo aditivo por que los cambios son brucos y se adapta mejor
