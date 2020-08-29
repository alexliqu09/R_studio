#Implemnetación del Código de la PD6 en el problema 7 asignado
#Dado que el problema 7 en algunos items no se acoplan a lo pedido 
#en los items de R se optop por variar el código para poder implementarlo
data_x<-c(29.4,39.2,49.0,58.8,68.6,78.4)
data_y<-c(4.25,5.25,6.50,7.85,8.75,10.00)
############################################################################################################################################################
#7a)
#Nos piden los parametros B0 y B1,,entonces nos apoyamos en la función ya creada
#B0(x,y)y B1(x,y)
media<-function(x){
  mean(x)
}

S_a_b<-function(a,b){
  sum((a-media(a))*(b-media(b)))
}

B1<-function(x,y){
  S_a_b(x,y)/S_a_b(x,x)
}
B0<-function(x,y){
  media(y)-B1(x,y)*media(x)
}

############################################################################################################################################################3#
#7b)
#Nos apoyamos en la función intervalo de confianza para poder hallar
# los limites inferiores y superiores para  la pendiente  o  B1

IC_modelo_lineal<-function(x,y,x0,alfa){
  tamaño_datos<-length(x)
  b1<-B1(x,y)
  t<-qt(alfa/2,tamaño_datos-2,lower.tail = FALSE)
  s<-sqrt(S_square(x,y))
  lim_in<-b1-t*s*sqrt(C11(data_x))
  lim_sup<-b1+t*s*sqrt(C11(data_x))
  intervalo<-c(lim_in,lim_sup)
}

C11<-function(x){
  1/S_a_b(x,x)
}


#####################################################################################################################################################################################
#7c)
#nos apoyamos en la funcion Prueba de hipotesis lineal   
PH_linear<-function(X,Y,alfa){
  #H:B0==0,hA:B0 !=0 modelo lineal
  tamaño<-length(X)
  b0<-B0(X,Y) 
  s<-sqrt(S_square(X,Y))
  T<-abs(b0/(s*sqrt(C00(X))))
  t_alfa_2<-qt((alfa/2),tamaño-2,lower.tail = FALSE)
  if(T>t_alfa_2 ){
    print("Esta en el intervalo de rechazo,por ende se rechaza la hipotesis nula,T= ")
    cat(T,"con ",tamaño-2,"grados de libertad")
  }else{
    print("No esta en el intervalo de rechazo,por ende se acepta la hipotesis nula,T= ")
    print(T)
  }
  
}

C00<-function(x){
  n<-length(x)
  suma<- sum(x^2)
  S<-suma/(S_a_b(x,x)*n)
}

S_a_b<-function(a,b){
  sum((a-media(a))*(b-media(b)))
}

"Para poder estimar el 
valor de nivel de significancia alcanzado, hacemos una prueba de hipotesis con 
alpha = 0.01, y alpha = 0.02
obtenemos que con 0.01, la hipotesis nula no se rechaza.
y con 0.02 si se rechaza.
Entonces el p-valor debe estar en este intervalo. 
y asi corroboramos la validez de las cotas 0.01 y 0.02 
para el nivel de significancia alcanzado
"
###################################################################################
##correr esta función para tener todos los resultados de los 3 items del problema 7

Imprimir_resultado<-function(x,y,alfa){
  print("problema 7a")
  print("Los parametros B0 y B1")
  cat("Y","=",B0(x,y),"+",B1(x,y),"X")
  print("\n")
  print("problema  7b")
  print("El intervalo de confianza para un prueba de 95% para la linea pendiente")
  print(IC_modelo_lineal(data_x,data_y,0,0.05))
  print("\n")
  print("problema 7c")
  PH_linear(data_x,data_y,alfa)
}