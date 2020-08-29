#Función prueba de hipotesis para la diferencia de dos muestras pequeñas
#H0:u1-u2==D0
#Ha:u1-u2!=D0

PH_MP_d<-function(x,y,D0,alfa){
  #Inicializamos las variables n1,n2 que son los tamaños de la muestra
  #Tambien calculamos la varianza de las muestras S1,S2
  n1<-length(x)
  n2<-length(y)
  Y1<-media(x)
  Y2<-media(y)
  S1<-S_i(x,n1)
  S2<-S_i(y,n2)
  #Calcula,os el estadistico de Prueba y quantil  
  T<-abs(Estadistico_de_prueba(Y1,Y2,D0,n1,n2,S1,S2))
  t_alf<-qt(alfa/2,n1+n2-2)
  #Revisamos las condiciones Si para RR  de dos colas
  #Si T>t_alfa se rechaza la hipotesis nuña y se acepta la hipotesis alternativa  
  if(T>t_alf){
    print("se rechaza la hipotesis nula y se acepta la hipotesis alternativa")
    print(T)
  }else{
    print("se acepta la hipotesis nula")
    print(T)
  }
}
#Reutilizamos alfunas funciones ya implementadas en la PD6 como la función media
media<-function(x){
  mean(x)
}

#Se crea la función Estadistico de Prueba,utilizando los datos inicializados en la
#función prueba de hipotesis
Estadistico_de_prueba<-function(Y1,Y2,D0,n1,n2,S1,S2){
  num<-(Y1-Y2-D0)
  sp<-S_p(n1,n2,S1,S2)
  raiz<-sqrt((1/n1)+(1/n2))
  num/(sp*raiz)
}
#Escrbimos las funcionea Sp y Si donde Si es una función general para hallar S1,S2  
#y Sp es la función que implementa S1 y S2
S_p<-function(n1,n2,S1,S2){
  num<-(n1-1)*(S1^2)+(n2-1)*(S2^2)
  den<-n1+n2-2
  sp<-sqrt(num/den)
}

S_i<-function(x,n){
  suma<-sum((x-media(x))^2)
  S<-sqrt(suma/(n-1))
}
