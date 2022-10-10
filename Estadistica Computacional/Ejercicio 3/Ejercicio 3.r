# Problema 1.

## A Señale el tipo de variable aleatoria y la distribución que sigue.
# discreta binominal

## B Determine la función de probabilidad de masa.
# n = Total de los elemententos seleccionados (3)
# x = Toma el valor de 0, 1, 2 o 3
# P = Probabilidad de exito (0.8)

funcionMasa= function(n, x, P){
  (factorial(n)/(factorial(x) * (factorial(n - x)))) * (P**x) * (1-P)**(n-x)
}
funcionMasa(3, 0, 0.8) + funcionMasa(3, 1, 0.8) + funcionMasa(3, 2, 0.8) + funcionMasa(3, 3, 0.8)

## C Grafique la distribución.
# Datos
rango = seq(0,3)
distribucion = dbinom(rango, size = 3,prob = 0.8)
datos=data.frame(rango,distribucion)

### Gafico
library("ggplot2")
grafico = ggplot(data=datos,aes(x=rango,y=distribucion))
grafico = grafico + geom_bar(stat="identity",fill="lightblue3")
grafico = grafico + theme_bw() + ggtitle("Distribución de probabilidades")
grafico = grafico + xlab("Rango") + ylab("Probabilidad")
plot(grafico)

#--------------------------------------------------------------------------------------------------------------------------

#Problema 2

## A Señale el tipo de variable aleatoria y la distribución que sigue.
# Tomando en cuenta el problema y lo que nos piden, tendriamos una Variable aleatoria discreta con distribución binomial negativa

## B
# ¿Cuál es la probabilidad de que cuatro o más evaluaciones deban ser efectuadas para detectar a dos personas portadoras del gen?
# x = numero de personas analizadas


#p (x >= 4) = 1 - (P(x=3) + P(x=2))
1 - (dnbinom(3-2, size=2, prob=0.1) + dnbinom(x=2-2, size=2, prob=0.1))

## C
# ¿Cuál es el número esperado de evaluaciones que debo realizar para detectar dos personas portadoras del gen?
# E(x)= size/prob
2/0.1

## D 
# Grafique la distribución.
### Datos
rango = seq(0,20)
intentos_fallidos=2
distribucion = dnbinom(x=rango, size=intentos_fallidos, prob=0.1)
datos=data.frame(rango,distribucion)

### Grafico                  
library("ggplot2")
grafico = ggplot(data=datos,aes(x=rango,y=distribucion))
grafico = grafico + geom_bar(stat="identity",fill="lightblue3")
grafico = grafico + theme_bw() + ggtitle("Distribución de
probabilidades")
grafico = grafico + xlab("rango") + ylab("Probabilidad")
plot(grafico)
#--------------------------------------------------------------------------------------------------------------------------

# Problema 3

## A 
# analizando el enunciado y los ejercicios este presenta una variable aletaria discreta con distibución hipergeométrica

## B Si a 10 hombres de la empresa se les hace la prueba del marcador en este cromosoma, ¿cuál es la probabilidad de que exactamente 1 hombre tenga el marcador?
# (x=1)
# dhyper(x, m, n, k)
dhyper(1, 800*0.3, 800 - (800*0.3), 10)

## C Si a 10 hombres de la empresa se les hace la prueba del marcador en este cromosoma, ¿cuál es la probabilidad de que más de 1 tenga el marcador?
# (x>1)

1 - (dhyper(1, 800*0.3, 800 - (800*0.3), 10)) - (dhyper(0, 800*0.3, 800 - (800*0.3), 10))

## D Grafique la distribución
exitos=seq(0:10) 
distribucion = dhyper(x=exitos, 800*0.3, 800 - (800*0.3), 10)
datos=data.frame(exitos,distribucion)

library("ggplot2")
grafico = ggplot(data=datos,aes(x=exitos,y=distribucion))
grafico = grafico + geom_bar(stat="identity",fill="lightblue3")
grafico = grafico + theme_bw() + ggtitle("Distribución de
probabilidades")
grafico = grafico + xlab("Num Personas") + ylab("Probabilidad")
plot(grafico)
#--------------------------------------------------------------------------------------------------------------------------

# Problema 4
## A Señale el tipo de variable aleatoria y la distribución que sigue.
# variable aleatoria discreta con distribucion de possion 

## B ¿Cuál es la probabilidad de que haya exactamente cinco llamadas en una hora?
# k = 5 numero de llamadas en 1 hora
# lambda = 8 promdio de llamadas por hora
dpois(k,lambda)

## C ¿Cuál es la probabilidad de que haya tres llamadas o menos en una hora?
(dpois(0,8)) + (dpois(1,8)) + (dpois(2,8)) + (dpois(3,8))
#--------------------------------------------------------------------------------------------------------------------------
# Problema 5
## A ¿cuáles son la media y la varianza del tiempo total para completar estas cirugías?
# Se tiene a la media = 129 * 10 y la varianza = (14**2) * 10

#--------------------------------------------------------------------------------------------------------------------------
# Problema 6
## Problema 1
### z = (x - media)/desviacion
z = ((3 + 0.5) -(3*0.8))/(sqrt(3*0.8*(1-0.8)))
pnorm(z)

#podemos ver que es una aproximacion erronea, ya que se ve que 3*0.8 y 3*(1-0.8) son menores a 5.

## Problema 4
### 1
z = (5-8)/sqrt(8)
pnorm(z)

### 2
(pnorm((0-8)/sqrt(8))) + (pnorm((1-8)/sqrt(8))) + (pnorm((2-8)/sqrt(8))) + (pnorm((3-8)/sqrt(8)))

# viendo lamba mayor a 5, se concluye buena aproximación.
