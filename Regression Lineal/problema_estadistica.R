#############################################################################################
#1)
#creamos una función media 
media<-function(x){
  mean(x)
}
#Creamos una función  Sx_y 
#Estamos generalizando el caso S(x,x), S(y,y) en una función
#Para evitar crear las mismas funciónes.
S_a_b<-function(a,b){
  sum((a-media(a))*(b-media(b)))
}
#Se crea una función calcular los parametros
B1<-function(x,y){
  S_a_b(x,y)/S_a_b(x,x)
}
B0<-function(x,y){
   media(y)-B1(x,y)*media(x)
}
################################################################################################
#2)
#Se crea Sum Square Error y nos apoyamos en las funciones ya programadas 

SSE<-function(x,y){
    sum((y-(B0(x,y)+B1(x,y)*x))^2)
}

# creamos una función estimador de la varianza de e ,en la cual nos apoyamos
#Sum Square Error   

S_square<-function(x,y){
  n<-length(x)
  SSE(x,y)*(1/(n-2))
}
##################################################################################################
#3)
#intervalo de confianza para E(Y)
#Para ello,utilizamos las funciones B0,B1,la función para el cuantil qt
# la varianza y expresion que es 1/n + un x0 menos su media al cuadrado entre el S(x,x)
#como queremos los limites inferiores y superiores lo que debemos retornar es un vector
#en este caso intervalo.
IC_modelo_lineal<-function(x,y,x0,alfa){
  tamaño_datos<-length(x)
  b0<-B0(x,y)
  b1<-B1(x,y)
  t<-qt(alfa/2,tamaño_datos-2,lower.tail = FALSE)
  s<-sqrt(S_square(x,y))
  expresion<-sqrt(x_expresion(x0,x,tamaño_datos))
  lim_in<-b0+b1*x0-t*s*expresion
  lim_sup<-b0+b1*x0+t*s*expresion
  intervalo<-c(lim_in,lim_sup)
}
x_expresion<-function(x0,x,n){
   
  (1/n) +((x0-media(x))^2)/S_a_b(x,x)
}
#################################################################################################
#4)
#intervalo de predicción para Y en un punto
#Aqui reutilizamos las funciones unimos en el anterior item
#cambiando expresion agruegandole 1
IC_prediccion<-function(x,y,x0,alfa){
    tamaño_datos<-length(x)
    b0<-B0(x,y)
    b1<-B1(x,y)
    t<-qt(alfa/2,tamaño_datos-2,lower.tail = FALSE)
    s<-sqrt(S_square(x,y))
    expresion<-sqrt(expresion1(x0,x,tamaño_datos))
    lim_in<-b0+b1*x0-t*s*expresion
    lim_sup<-b0+b1*x0+t*s*expresion
    intervalo<-c(lim_in,lim_sup)
}

expresion1<-function(x0,x,n){
  1+(1/n) +((x0-media(x))^2)/S_a_b(x,x)
}


###############################################################################################3#
#5)
#programar una prueba de hipotesis para Hipotesis nula y B1=0, hipotesis alternativa
#Ha: B!=0,uyilizando las funciones anteriores y en el usando una RR de dos colas
#si T>t_alfa2 se rechaza la H0 y T <t_alfa se acepta.

PH_linear<-function(X,Y,alfa){
  #H:B1==0,hA:B1 !=0 modelo lineal
  tamaño<-length(X)
  b1<-B1(X,Y) 
  s<-sqrt(S_square(X,Y))
  T<-abs(b1/(s*sqrt(C11(X))))
  t_alfa_2<-qt((alfa/2),tamaño-2)
  if(T>t_alfa_2 ){
     print("Esta en el intervalo de rechazo,T= ")
     print(T)
  }else{
    print("No esta en el intervalo de rechazo,T= ")
    print(T)
  }
  
}

C11<-function(x){
  1/S_a_b(x,x)
}
