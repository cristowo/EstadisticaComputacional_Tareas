(library(gtools))
help(permutations)
help(combinations)

##A
#permutaciones CON
14**4 
nrow(permutations(14, 4, 1:14, TRUE, TRUE))

#permutaciones SIN 
factorial(14)/factorial(14-4)
nrow(permutations(14, 4, 1:14, TRUE, FALSE))

##B
#combinacion CON
factorial(3+3-1)/(factorial(3)*(factorial(3-1)))
nrow(combinations(3, 3, 1:3, TRUE, TRUE))

#combinacion SIN
factorial(3)/(factorial(3)*factorial(3-3))
nrow(combinations(3, 3, 1:3, TRUE, FALSE))

##C
#permutaciones SIN 
factorial(30)/factorial(30-2)
nrow(permutations(30, 2, 1:30, TRUE, FALSE))

#combinacion SIN
factorial(30)/(factorial(2)*factorial(30-2))
nrow(combinations(30, 2, 1:30, TRUE, FALSE))

###2
##A
#caso de bayes
#PB = probabilidad de que ocurra B (ejemplo: de todas las personas tengan un título)
#PA = probabilidad de que ocurra A (ejemplo: de todas las personas, las que sean mujeres)
#PBA = probabilidad de que ocurra B con respecto de A (ejemplo: de las personas que tengan un título, que sean mujeres)

PAB=function(PA, PB, PBA){  
  (PBA * PA)/PB
}

##B
##1
pasajeros = (499/1316)*100
tripulacion= (212/885)*100

##2
#suponinedo una interpretacion: 
#cual es la probabilidad de elegir a una mujer de la tripulacion 
#y que esta sobreviva

MVT= PAB((23/2201), (711/2201), (20/23))

#probabilidad de que los tripulantes supervivientes sea mujer
20/212

##3
"suponinedo, cual es la probailidad de elegir a un niño de tercera clase "
NV3ra= PAB((79/2201), (711/2201), (27/79))

#niños
(29+28)/(29+28+35+17)
#mujeres
(316)/(316+109)
#hombres
338/(1329+338)

##5
clase = sample(names(apply(Titanic,c(1),sum)),1)
clase

if(clase == "1st"){
  if (((5+1)/(5+1)) > (57/(118+57))  & ( 140/(140+4)) > (57/(118+57))){
    cat("Cumple con el código")
  } else {
    cat("no cumple con el código")
  }
}
if (clase == "2nd"){
  if ((((11+13)/(11+13)) > (14/(154+14)) & ( 80/(80+13)) > (14/(154+14)))){
    cat("Cumple con el código")
  } else {
    cat("no cumple con el código")
  }
}
if(clase == "3rd"){
  if ((((13+14)/(35+17+13+14)) > (75/(387+75)) & ( 89/(89+76)) > (75/(387+75)))){
    cat("Cumple con el código")
  } else {
    cat("no cumple con el código")
  }
}
if(clase == "Crew"){
  if ((20/(20+3)) > (192/(192+670))){
    cat("Cumple con el código")
  } else {
    cat("no cumple con el código")
  }
}
