 # library(haven)
 library(data.table)
 library(ggplot2)
 # library(dplyr)
 #install.packages("seasonal")
 library(seasonal)
 # library("x13binary")


#setwd("H:/Unidades compartidas/CEP/Empleo sectorial - Chubut/input")

# Cantidad mm3

#base<-fread("produccin-gas-sesco-tight-y-shale-captulo-iv-por-yacimiento_chubut.csv", header = TRUE, sep = ",",integer64 = "numeric")
base <- data.table::fread(r'(C:\Users\Usuario\Documents\Cep Pedidos\Pedidos Puntuales\paneles_mectra\paneles_rates.csv)')
base <- base[-(1:11)] 

p <- ts(base$entry_rate, start=2008, frequency = 12)
plot(p)
p <- seas(p)
final(p)
plot(p)
summary(p)

h <-as.data.frame(p)
base$date=h$date
ggplot() + 
  geom_line(data = h, aes(x=date, y = seasonaladj), color = "red")+
  geom_line(data = base, aes(x=date, y = entry_rate), color = "green")+
  geom_line(data = h, aes(x=date, y = trend), color = "black")+ theme_minimal()

write.csv(p,"produccin-gas-sesco-tight-y-shale-captulo-iv-por-yacimiento_chubut_des.csv")
