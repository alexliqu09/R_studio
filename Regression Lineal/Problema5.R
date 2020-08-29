#Adaptando las funciones creadas en R de la pregunta 6 para resolver la pregunta
#5 de la PD5.
####################################################################################################################
#5)
#Utilizamos la data del probkema
datos_x<-c(17.4,18.9,39.6,34.4,19.6,33.7,37.2,43.4,41.7,27.5,24.1,39.6,12.2,25.5,22.1,29.3,21.1,23.8,43.2,24.4)

#implementamos algunas funciones ya programadas
#en anteriores códigos
media<-function(x){
  mean(x)
}
S_i<-function(x,n){
  suma<-sum((x-media(x))^2)
  S<-sqrt(suma/(n-1))
}
#en la pregunta 6 nos dan para la diferencia de medias,entonces para adaptar el código
# ala pregunta 5 se cambio la funcion estadistico de prueba , de tal manera que hallemos
#para la media

PH_MP_d<-function(x,u,alfa){
 n<-length(x)
 S<-sqrt(S_i(x,n)/(n-1)) 
 x_media<-media(x)
 t<-Estadistico_de_prueba(x,u,n,S)
 t_alf<-qt(alfa/2,n-2)
 if(t<t_alf){
  print("Se rechaza la hipotesis nula")
  print(" no se puede concluir que la pesca media ha disminuido")
 }else{
    print("Se acepta la hipotesis nula")
    print("podemos concluir que la pesca media a disminuido")
   }

}
#cambio de función estadistico de prueba
Estadistico_de_prueba<-function(x,u,n1,S){
  num<-(media(x)-u)
  num<-num/(S/sqrt(n1))
}
###############################################################################################
#probar con datos_x y 30.31, y 0.05 como indica en el problema
Imprimir_resultado<-function(datos_x,u,alfa){
  print("Problema 5")
  PH_MP_d(datos_x,u,alfa)
  
}