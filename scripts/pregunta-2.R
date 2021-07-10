# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, tidyverse)
setwd(this.path::this.dir())

source('./lib/transactions.R')
source('./lib/rules.R')
# ------------------------------------------------------------------------------
#
#
#
#
# ------------------------------------------------------------------------------
# Pregunta 2
# ------------------------------------------------------------------------------
# Para las canciones que ingresaron al TOP 10 durante el período 2018-2020: 
#
# ¿Cuáles son los niveles de los atributos y los términos del vocabulario 
# frecuente de las letras que más inciden en la permanencia de las canciones
# en el puesto número 1 respecto de aquellas que nunca lo alcanzan?
# ------------------------------------------------------------------------------
#
#
#
# Niveles de discretizacion usados en cada variable:
#
# Danceability Levels:
# 	- Names: low, medium, high
# 	- Values: 0, 0.5, 0.75, 1
# Energy Levels:
# 	- Names: low, medium, high
# 	- Values: 0, 0.52, 0.7, 1
# Liveness Levels:
# 	- Names: low, medium, high
# 	- Values: 0, 0.5, 0.75, 1
# Speechiness Levels:
# 	- Names: low, medium, high
# 	- Values: 0, 0.5, 0.75, 1
# Valence Levels:
# 	- Names: low, medium, high
# 	- Values: 0, 0.52, 0.7, 1
# Position Levels:
# 	- Names: high, medium, low
# 	- Values: 0.5, 1.5, 4.5, 10.5
#
#
#
trans <- load()
rules = generate_rules(trans, support=0.08, confidence=0.1)
plot_rules(rules, interactive=FALSE)
#
#
#
#
# ------------------------------------------------------------------------------
# 1. Cuales son los features y niveles encontratos para los terminos top1=yes.
# ------------------------------------------------------------------------------
result1 <- arules::subset(
  rules, 
  subset = 
    (lhs %pin% "top1=yes") 
    
    & (!rhs %pin% "term_") 
    & (!rhs %pin% "best_album")
    & (!rhs %pin% "top1")
    & (!rhs %pin% "position=")
    
    & lift>=1
    & lift<=1.4
  
    & confidence > 0.5
)
show_rules(result1, top = 500)
plot_rules(result1, interactive=FALSE)
# ------------------------------------------------------------------------------
# Respuesta
# ------------------------------------------------------------------------------
# rhs               support   confidence coverage  lift     count
# {liveness=low}    0.1520468 1          0.1520468 1.005882 26   
# {speechiness=low} 0.1520468 1          0.1520468 1.000000 26   
# ------------------------------------------------------------------------------
#
#
#
#
#
# ------------------------------------------------------------------------------
# 2. Cuales son los terminos encontrados para los features y niveles encontrados 
#    previamente para top1=yes.
# ------------------------------------------------------------------------------
#
#
#
#
# 2.1. liveness=low
#
result2 <- arules::subset(
  rules, 
  subset = 
    (lhs %in% c("top1=yes")) & (lhs %in% c("liveness=low")) 
  
    & (rhs %pin% "term_") 
    & (!rhs %pin% "best_album")
    & (!rhs %pin% "top1")
    & (!rhs %pin% "=")
    
    & lift>=1
    & lift<=2
)
show_rules(result2, top = 500)
plot_rules(result2, interactive=FALSE)
# ------------------------------------------------------------------------------
# Respuesta
# ------------------------------------------------------------------------------
# lhs                                                      rhs         support   confidence coverage  lift     count
# [1]  {liveness=low,top1=yes}                               => {term_just} 0.1111111 0.7307692  0.1520468 1.125780 19   
# [2]  {liveness=low,position=high,top1=yes}                 => {term_just} 0.1111111 0.7307692  0.1520468 1.125780 19   
# [3]  {liveness=low,speechiness=low,top1=yes}               => {term_just} 0.1111111 0.7307692  0.1520468 1.125780 19   
# [4]  {liveness=low,position=high,speechiness=low,top1=yes} => {term_just} 0.1111111 0.7307692  0.1520468 1.125780 19   
# [5]  {liveness=low,top1=yes}                               => {term_know} 0.1169591 0.7692308  0.1520468 1.105365 20   
# [6]  {liveness=low,position=high,top1=yes}                 => {term_know} 0.1169591 0.7692308  0.1520468 1.105365 20   
# [7]  {liveness=low,speechiness=low,top1=yes}               => {term_know} 0.1169591 0.7692308  0.1520468 1.105365 20   
# [8]  {liveness=low,position=high,speechiness=low,top1=yes} => {term_know} 0.1169591 0.7692308  0.1520468 1.105365 20   
# [9]  {liveness=low,top1=yes}                               => {term_yeah} 0.1111111 0.7307692  0.1520468 1.041346 19   
# [10] {liveness=low,position=high,top1=yes}                 => {term_yeah} 0.1111111 0.7307692  0.1520468 1.041346 19   
# [11] {liveness=low,speechiness=low,top1=yes}               => {term_yeah} 0.1111111 0.7307692  0.1520468 1.041346 19   
# [12] {liveness=low,position=high,speechiness=low,top1=yes} => {term_yeah} 0.1111111 0.7307692  0.1520468 1.041346 19
# ------------------------------------------------------------------------------
#
#
#
#
# 2.2. speechiness=low
#
result3 <- arules::subset(
  rules, 
  subset = 
    (lhs %in% c("top1=yes")) & (lhs %in% c("speechiness=low"))

    & (rhs %pin% "term_") 
    & (!rhs %pin% "best_album")
    & (!rhs %pin% "top1")
    & (!rhs %pin% "=")
    
    & lift>=1
    & lift<=2
)
show_rules(result3, top = 500)
plot_rules(result3, interactive=FALSE)
# ------------------------------------------------------------------------------
# Respuesta
# ------------------------------------------------------------------------------
# lhs                                                      rhs         support   confidence coverage  lift     count
# [1]  {speechiness=low,top1=yes}                            => {term_just} 0.1111111 0.7307692  0.1520468 1.125780 19   
# [2]  {position=high,speechiness=low,top1=yes}              => {term_just} 0.1111111 0.7307692  0.1520468 1.125780 19   
# [3]  {liveness=low,speechiness=low,top1=yes}               => {term_just} 0.1111111 0.7307692  0.1520468 1.125780 19   
# [4]  {liveness=low,position=high,speechiness=low,top1=yes} => {term_just} 0.1111111 0.7307692  0.1520468 1.125780 19   
# [5]  {speechiness=low,top1=yes}                            => {term_know} 0.1169591 0.7692308  0.1520468 1.105365 20   
# [6]  {position=high,speechiness=low,top1=yes}              => {term_know} 0.1169591 0.7692308  0.1520468 1.105365 20   
# [7]  {liveness=low,speechiness=low,top1=yes}               => {term_know} 0.1169591 0.7692308  0.1520468 1.105365 20   
# [8]  {liveness=low,position=high,speechiness=low,top1=yes} => {term_know} 0.1169591 0.7692308  0.1520468 1.105365 20   
# [9]  {speechiness=low,top1=yes}                            => {term_yeah} 0.1111111 0.7307692  0.1520468 1.041346 19   
# [10] {position=high,speechiness=low,top1=yes}              => {term_yeah} 0.1111111 0.7307692  0.1520468 1.041346 19   
# [11] {liveness=low,speechiness=low,top1=yes}               => {term_yeah} 0.1111111 0.7307692  0.1520468 1.041346 19   
# [12] {liveness=low,position=high,speechiness=low,top1=yes} => {term_yeah} 0.1111111 0.7307692  0.1520468 1.041346 19
# ------------------------------------------------------------------------------
#
#
#
#
#
# ------------------------------------------------------------------------------
# 3. Cuales son los features y niveles encontratos para los terminos top1=no.
# ------------------------------------------------------------------------------
result4 <- arules::subset(
  rules, 
  subset = 
    (lhs %pin% "top1=no") 
  
  & (!rhs %pin% "term_") 
  & (!rhs %pin% "best_album")
  & (!rhs %pin% "top1")
  & (!rhs %pin% "position=")
  
  & lift>=1
  & lift<=1.5
  
  & confidence > 0.5
)
show_rules(result4, top = 50)
plot_rules(result4, interactive=FALSE)
# ------------------------------------------------------------------------------
# Respuesta
# ------------------------------------------------------------------------------
# rhs                   support   confidence coverage  lift     count
# {valence=low}         0.1111111 0.8636364  0.1286550 1.491736 19   
# {danceability=medium} 0.1052632 0.7826087  0.1345029 1.486957 18   
# {energy=high}         0.1052632 0.5294118  0.1988304 1.484089 18   
# {danceability=high}   0.1052632 0.5806452  0.1812865 1.438990 18   
# {energy=medium}       0.1052632 0.5806452  0.1812865 1.438990 18   
# ------------------------------------------------------------------------------
#
#
#
#
#
# ------------------------------------------------------------------------------
# 4. Cuales son los terminos encontrados para los features y niveles encontrados 
#    previamente para top1=no.
# ------------------------------------------------------------------------------
#
#
#
#
# 4.1. valence=low
#
result5 <- arules::subset(
  rules, 
  subset = 
    (lhs %in% c("top1=no")) & (lhs %in% c("valence=low")) 
  
    & (rhs %pin% "term_") 
    & (!rhs %pin% "best_album")
    & (!rhs %pin% "top1")
    & (!rhs %pin% "=")
    
    & lift>=1
    & lift<=2
)
show_rules(result5, top = 50)
plot_rules(result5, interactive=FALSE)
# ------------------------------------------------------------------------------
# Respuesta
# ------------------------------------------------------------------------------
# lhs                                                                                rhs         support   confidence coverage  lift     count
# [1]  {liveness=low,term_just,term_say,term_yeah,top1=no,valence=low}                 => {term_aint} 0.1052632 0.7826087  0.1345029 1.968031 18   
# [2]  {liveness=low,speechiness=low,term_just,term_say,term_yeah,top1=no,valence=low} => {term_aint} 0.1052632 0.7826087  0.1345029 1.968031 18   
# [3]  {term_get,term_see,top1=no,valence=low}                                         => {term_can}  0.1228070 0.8400000  0.1461988 1.915200 21   
# [4]  {liveness=low,term_get,term_see,top1=no,valence=low}                            => {term_can}  0.1228070 0.8400000  0.1461988 1.915200 21   
# [5]  {speechiness=low,term_get,term_see,top1=no,valence=low}                         => {term_can}  0.1228070 0.8400000  0.1461988 1.915200 21   
# [6]  {liveness=low,speechiness=low,term_get,term_see,top1=no,valence=low}            => {term_can}  0.1228070 0.8400000  0.1461988 1.915200 21   
# [7]  {term_know,term_say,top1=no,valence=low}                                        => {term_back} 0.1052632 0.6666667  0.1578947 1.900000 18   
# [8]  {liveness=low,term_just,term_say,top1=no,valence=low}                           => {term_back} 0.1052632 0.6666667  0.1578947 1.900000 18   
# [9]  {liveness=low,term_know,term_say,top1=no,valence=low}                           => {term_back} 0.1052632 0.6666667  0.1578947 1.900000 18   
# [10] {speechiness=low,term_know,term_say,top1=no,valence=low}                        => {term_back} 0.1052632 0.6666667  0.1578947 1.900000 18   
# [11] {liveness=low,term_say,term_yeah,top1=no,valence=low}                           => {term_back} 0.1052632 0.6666667  0.1578947 1.900000 18   
# [12] {liveness=low,speechiness=low,term_just,term_say,top1=no,valence=low}           => {term_back} 0.1052632 0.6666667  0.1578947 1.900000 18   
# [13] {liveness=low,speechiness=low,term_know,term_say,top1=no,valence=low}           => {term_back} 0.1052632 0.6666667  0.1578947 1.900000 18   
# [14] {liveness=low,speechiness=low,term_say,term_yeah,top1=no,valence=low}           => {term_back} 0.1052632 0.6666667  0.1578947 1.900000 18   
# [15] {term_want,term_yeah,top1=no,valence=low}                                       => {term_aint} 0.1052632 0.7500000  0.1403509 1.886029 18   
# [16] {liveness=low,term_want,term_yeah,top1=no,valence=low}                          => {term_aint} 0.1052632 0.7500000  0.1403509 1.886029 18   
# [17] {speechiness=low,term_want,term_yeah,top1=no,valence=low}                       => {term_aint} 0.1052632 0.7500000  0.1403509 1.886029 18   
# [18] {term_just,term_say,term_yeah,top1=no,valence=low}                              => {term_aint} 0.1052632 0.7500000  0.1403509 1.886029 18   
# [19] {liveness=low,term_make,term_yeah,top1=no,valence=low}                          => {term_aint} 0.1052632 0.7500000  0.1403509 1.886029 18   
# [20] {liveness=low,speechiness=low,term_want,term_yeah,top1=no,valence=low}          => {term_aint} 0.1052632 0.7500000  0.1403509 1.886029 18  
# ------------------------------------------------------------------------------
#
#
#
#
# 4.2. danceability=medium
#
result6 <- arules::subset(
  rules, 
  subset = 
    (lhs %in% c("top1=no")) & (lhs %in% c("danceability=medium")) 
  
    & (rhs %pin% "term_") 
    & (!rhs %pin% "best_album")
    & (!rhs %pin% "top1")
    & (!rhs %pin% "=")
    
    & lift>=1
    & lift<=2
)
show_rules(result6, top = 50)
plot_rules(result6, interactive=FALSE)
# ------------------------------------------------------------------------------
# Respuesta
# ------------------------------------------------------------------------------
# lhs                                                                                          rhs        support   confidence coverage  lift     count
# [1]  {danceability=medium,term_know,term_way,top1=no}                                          => {term_can} 0.1052632 0.7826087  0.1345029 1.784348 18   
# [2]  {danceability=medium,liveness=low,term_know,term_way,top1=no}                             => {term_can} 0.1052632 0.7826087  0.1345029 1.784348 18   
# [3]  {danceability=medium,speechiness=low,term_know,term_way,top1=no}                          => {term_can} 0.1052632 0.7826087  0.1345029 1.784348 18   
# [4]  {danceability=medium,liveness=low,speechiness=low,term_know,term_way,top1=no}             => {term_can} 0.1052632 0.7826087  0.1345029 1.784348 18   
# [5]  {danceability=medium,term_get,term_know,top1=no,valence=low}                              => {term_can} 0.1169591 0.7692308  0.1520468 1.753846 20   
# [6]  {danceability=medium,liveness=low,term_get,term_know,top1=no,valence=low}                 => {term_can} 0.1169591 0.7692308  0.1520468 1.753846 20   
# [7]  {danceability=medium,speechiness=low,term_get,term_know,top1=no,valence=low}              => {term_can} 0.1169591 0.7692308  0.1520468 1.753846 20   
# [8]  {danceability=medium,liveness=low,speechiness=low,term_get,term_know,top1=no,valence=low} => {term_can} 0.1169591 0.7692308  0.1520468 1.753846 20   
# [9]  {danceability=medium,term_get,term_know,term_yeah,top1=no}                                => {term_can} 0.1052632 0.7500000  0.1403509 1.710000 18   
# [10] {danceability=medium,liveness=low,term_get,term_know,term_yeah,top1=no}                   => {term_can} 0.1052632 0.7500000  0.1403509 1.710000 18   
# [11] {danceability=medium,speechiness=low,term_get,term_know,term_yeah,top1=no}                => {term_can} 0.1052632 0.7500000  0.1403509 1.710000 18   
# [12] {danceability=medium,liveness=low,speechiness=low,term_get,term_know,term_yeah,top1=no}   => {term_can} 0.1052632 0.7500000  0.1403509 1.710000 18   
# [13] {danceability=medium,position=low,term_just,top1=no}                                      => {term_say} 0.1052632 0.6923077  0.1520468 1.691209 18   
# [14] {danceability=medium,liveness=low,position=low,term_just,top1=no}                         => {term_say} 0.1052632 0.6923077  0.1520468 1.691209 18   
# [15] {danceability=medium,position=low,speechiness=low,term_just,top1=no}                      => {term_say} 0.1052632 0.6923077  0.1520468 1.691209 18   
# [16] {danceability=medium,liveness=low,position=low,speechiness=low,term_just,top1=no}         => {term_say} 0.1052632 0.6923077  0.1520468 1.691209 18   
# [17] {danceability=medium,term_get,term_know,top1=no}                                          => {term_can} 0.1403509 0.7272727  0.1929825 1.658182 24   
# [18] {danceability=medium,liveness=low,term_get,term_know,top1=no}                             => {term_can} 0.1403509 0.7272727  0.1929825 1.658182 24   
# [19] {danceability=medium,speechiness=low,term_get,term_know,top1=no}                          => {term_can} 0.1403509 0.7272727  0.1929825 1.658182 24   
# [20] {danceability=medium,liveness=low,speechiness=low,term_get,term_know,top1=no}             => {term_can} 0.1403509 0.7272727  0.1929825 1.658182 24 
# ------------------------------------------------------------------------------
#
#
#
#
# 4.3. danceability=high
#
result7 <- arules::subset(
  rules, 
  subset = 
    (lhs %in% c("top1=no")) & (lhs %in% c("danceability=high")) 
  
    & (rhs %pin% "term_") 
    & (!rhs %pin% "best_album")
    & (!rhs %pin% "top1")
    & (!rhs %pin% "=")
    
    & lift>=1
    & lift<=2
)
show_rules(result7, top = 50)
plot_rules(result7, interactive=FALSE)
# ------------------------------------------------------------------------------
# Respuesta
# ------------------------------------------------------------------------------
# [1]  {danceability=high,term_come,top1=no}                               => {term_get}  0.1052632 1.0000000  0.1052632 1.762887 18   
# [2]  {danceability=high,liveness=low,term_come,top1=no}                  => {term_get}  0.1052632 1.0000000  0.1052632 1.762887 18   
# [3]  {danceability=high,speechiness=low,term_come,top1=no}               => {term_get}  0.1052632 1.0000000  0.1052632 1.762887 18   
# [4]  {danceability=high,liveness=low,speechiness=low,term_come,top1=no}  => {term_get}  0.1052632 1.0000000  0.1052632 1.762887 18   
# [5]  {danceability=high,term_get,top1=no}                                => {term_come} 0.1052632 0.4500000  0.2339181 1.748864 18   
# [6]  {danceability=high,liveness=low,term_get,top1=no}                   => {term_come} 0.1052632 0.4500000  0.2339181 1.748864 18   
# [7]  {danceability=high,speechiness=low,term_get,top1=no}                => {term_come} 0.1052632 0.4500000  0.2339181 1.748864 18   
# [8]  {danceability=high,liveness=low,speechiness=low,term_get,top1=no}   => {term_come} 0.1052632 0.4500000  0.2339181 1.748864 18   
# [9]  {danceability=high,term_know,top1=no}                               => {term_need} 0.1169591 0.5555556  0.2105263 1.696429 20   
# [10] {danceability=high,liveness=low,term_know,top1=no}                  => {term_need} 0.1169591 0.5555556  0.2105263 1.696429 20   
# [11] {danceability=high,speechiness=low,term_know,top1=no}               => {term_need} 0.1169591 0.5555556  0.2105263 1.696429 20   
# [12] {danceability=high,liveness=low,speechiness=low,term_know,top1=no}  => {term_need} 0.1169591 0.5555556  0.2105263 1.696429 20   
# [13] {danceability=high,liveness=low,term_make,top1=no}                  => {term_get}  0.1169591 0.9523810  0.1228070 1.678940 20   
# [14] {danceability=high,liveness=low,speechiness=low,term_make,top1=no}  => {term_get}  0.1169591 0.9523810  0.1228070 1.678940 20   
# [15] {danceability=high,liveness=low,term_wanna,top1=no}                 => {term_get}  0.1111111 0.9500000  0.1169591 1.674742 19   
# [16] {danceability=high,liveness=low,speechiness=low,term_wanna,top1=no} => {term_get}  0.1111111 0.9500000  0.1169591 1.674742 19   
# [17] {danceability=high,term_right,top1=no}                              => {term_get}  0.1052632 0.9473684  0.1111111 1.670103 18   
# [18] {danceability=high,liveness=low,term_right,top1=no}                 => {term_get}  0.1052632 0.9473684  0.1111111 1.670103 18   
# [19] {danceability=high,speechiness=low,term_right,top1=no}              => {term_get}  0.1052632 0.9473684  0.1111111 1.670103 18   
# [20] {danceability=high,liveness=low,speechiness=low,term_right,top1=no} => {term_get}  0.1052632 0.9473684  0.1111111 1.670103 18
# ------------------------------------------------------------------------------
#
#
#
#
# 4.4. energy=medium
#
result8 <- arules::subset(
  rules, 
  subset = 
    (lhs %in% c("top1=no")) & (lhs %in% c("energy=medium"))  
  
    & (rhs %pin% "term_") 
    & (!rhs %pin% "best_album")
    & (!rhs %pin% "top1")
    & (!rhs %pin% "=")
    
    & lift>=1
    & lift<=2
)
show_rules(result8, top = 50)
plot_rules(result8, interactive=FALSE)
# ------------------------------------------------------------------------------
# Respuesta
# ------------------------------------------------------------------------------
# lhs                                                                          rhs          support   confidence coverage  lift     count
# [1]  {energy=medium,term_know,term_now,top1=no}                                => {term_cause} 0.1111111 0.8260870  0.1345029 1.743961 19   
# [2]  {energy=medium,liveness=low,term_know,term_now,top1=no}                   => {term_cause} 0.1111111 0.8260870  0.1345029 1.743961 19   
# [3]  {energy=medium,speechiness=low,term_know,term_now,top1=no}                => {term_cause} 0.1111111 0.8260870  0.1345029 1.743961 19   
# [4]  {energy=medium,liveness=low,speechiness=low,term_know,term_now,top1=no}   => {term_cause} 0.1111111 0.8260870  0.1345029 1.743961 19   
# [5]  {energy=medium,term_get,top1=no}                                          => {term_need}  0.1052632 0.5142857  0.2046784 1.570408 18   
# [6]  {energy=medium,liveness=low,term_get,top1=no}                             => {term_need}  0.1052632 0.5142857  0.2046784 1.570408 18   
# [7]  {energy=medium,speechiness=low,term_get,top1=no}                          => {term_need}  0.1052632 0.5142857  0.2046784 1.570408 18   
# [8]  {energy=medium,liveness=low,speechiness=low,term_get,top1=no}             => {term_need}  0.1052632 0.5142857  0.2046784 1.570408 18   
# [9]  {energy=medium,term_can,top1=no}                                          => {term_aint}  0.1052632 0.6206897  0.1695906 1.560852 18   
# [10] {energy=medium,liveness=low,term_can,top1=no}                             => {term_aint}  0.1052632 0.6206897  0.1695906 1.560852 18   
# [11] {energy=medium,speechiness=low,term_can,top1=no}                          => {term_aint}  0.1052632 0.6206897  0.1695906 1.560852 18   
# [12] {energy=medium,liveness=low,speechiness=low,term_can,top1=no}             => {term_aint}  0.1052632 0.6206897  0.1695906 1.560852 18   
# [13] {energy=medium,term_cause,term_yeah,top1=no}                              => {term_now}   0.1052632 0.7826087  0.1345029 1.556117 18   
# [14] {energy=medium,liveness=low,term_cause,term_yeah,top1=no}                 => {term_now}   0.1052632 0.7826087  0.1345029 1.556117 18   
# [15] {energy=medium,speechiness=low,term_cause,term_yeah,top1=no}              => {term_now}   0.1052632 0.7826087  0.1345029 1.556117 18   
# [16] {energy=medium,liveness=low,speechiness=low,term_cause,term_yeah,top1=no} => {term_now}   0.1052632 0.7826087  0.1345029 1.556117 18   
# [17] {energy=medium,term_make,top1=no}                                         => {term_get}   0.1169591 0.8695652  0.1345029 1.532945 20   
# [18] {energy=medium,liveness=low,term_make,top1=no}                            => {term_get}   0.1169591 0.8695652  0.1345029 1.532945 20   
# [19] {energy=medium,speechiness=low,term_make,top1=no}                         => {term_get}   0.1169591 0.8695652  0.1345029 1.532945 20   
# [20] {energy=medium,liveness=low,speechiness=low,term_make,top1=no}            => {term_get}   0.1169591 0.8695652  0.1345029 1.532945 20
# ------------------------------------------------------------------------------
#
#
#
#
# 4.5. energy=high
#
result9 <- arules::subset(
  rules, 
  subset = 
    (lhs %in% c("top1=no")) & (lhs %in% c("energy=high"))  
  
    & (rhs %pin% "term_") 
    & (!rhs %pin% "best_album")
    & (!rhs %pin% "top1")
    & (!rhs %pin% "=")
    
    & lift>=1
    & lift<=2
)
show_rules(result9, top = 50)
plot_rules(result9, interactive=FALSE)
# ------------------------------------------------------------------------------
# Respuesta
# ------------------------------------------------------------------------------
# lhs                                                                           rhs          support   confidence coverage  lift     count
# [1]  {energy=high,term_love,top1=no}                                            => {term_one}   0.1228070 0.7500000  0.1637427 1.914179 21   
# [2]  {best_album=no,energy=high,term_love,top1=no}                              => {term_one}   0.1052632 0.7500000  0.1403509 1.914179 18   
# [3]  {energy=high,liveness=low,term_love,top1=no}                               => {term_one}   0.1228070 0.7500000  0.1637427 1.914179 21   
# [4]  {energy=high,speechiness=low,term_love,top1=no}                            => {term_one}   0.1228070 0.7500000  0.1637427 1.914179 21   
# [5]  {best_album=no,energy=high,liveness=low,term_love,top1=no}                 => {term_one}   0.1052632 0.7500000  0.1403509 1.914179 18   
# [6]  {best_album=no,energy=high,speechiness=low,term_love,top1=no}              => {term_one}   0.1052632 0.7500000  0.1403509 1.914179 18   
# [7]  {energy=high,liveness=low,speechiness=low,term_love,top1=no}               => {term_one}   0.1228070 0.7500000  0.1637427 1.914179 21   
# [8]  {best_album=no,energy=high,liveness=low,speechiness=low,term_love,top1=no} => {term_one}   0.1052632 0.7500000  0.1403509 1.914179 18   
# [9]  {energy=high,term_one,top1=no}                                             => {term_love}  0.1228070 0.8750000  0.1403509 1.847222 21   
# [10] {energy=highliveness=low,term_one,top1=no}                                => {term_love}  0.1228070 0.8750000  0.1403509 1.847222 21   
# [11] {energy=high,speechiness=low,term_one,top1=no}                             => {term_love}  0.1228070 0.8750000  0.1403509 1.847222 21   
# [12] {energy=high,liveness=low,speechiness=low,term_one,top1=no}                => {term_love}  0.1228070 0.8750000  0.1403509 1.847222 21   
# [13] {energy=high,term_one,term_yeah,top1=no}                                   => {term_love}  0.1111111 0.8636364  0.1286550 1.823232 19   
# [14] {energy=high,liveness=low,term_one,term_yeah,top1=no}                      => {term_love}  0.1111111 0.8636364  0.1286550 1.823232 19   
# [15] {energy=high,speechiness=low,term_one,term_yeah,top1=no}                   => {term_love}  0.1111111 0.8636364  0.1286550 1.823232 19   
# [16] {energy=high,liveness=low,speechiness=low,term_one,term_yeah,top1=no}      => {term_love}  0.1111111 0.8636364  0.1286550 1.823232 19   
# [17] {energy=high,term_just,top1=no}                                            => {term_right} 0.1052632 0.5294118  0.1988304 1.810588 18   
# [18] {energy=high,liveness=low,term_just,top1=no}                               => {term_right} 0.1052632 0.5294118  0.1988304 1.810588 18   
# [19] {energy=high,speechiness=low,term_just,top1=no}                            => {term_right} 0.1052632 0.5294118  0.1988304 1.810588 18   
# [20] {energy=high,liveness=low,speechiness=low,term_just,top1=no}               => {term_right} 0.1052632 0.5294118  0.1988304 1.810588 18 
# ------------------------------------------------------------------------------



