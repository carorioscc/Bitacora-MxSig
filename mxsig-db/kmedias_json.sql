-- Function: kmedias_json(text, integer, integer)

-- DROP FUNCTION kmedias_json(text, integer, integer);

CREATE OR REPLACE FUNCTION kmedias_json(
    sql text,
    estratos integer,
    intervalos integer)
  RETURNS json AS
$BODY$

####Rupturas naturales (Jenks)####
#rm(list=ls(all=TRUE))
#library(foreign)
#library(stratification)
library(jsonlite)
datos<-pg.spi.exec(sql)
x<-datos
x<-x[order(x[,2]),]
L<-estratos
options(digits = 12)
f<-factor(x[,2])
tf<-tabulate(f)
moda<-as.numeric(levels(f)[tf==max(tf)])
if(length(moda)>1){moda<-"NA"}else
{
  moda<-as.character(moda)}
if(length(table(x[,2]))>1)
 {
 if(length(table(x[,2]))>L)
  {
    cla<-kmeans(x[,2],estratos)
    estrato<-cbind(cla$cluster,0)
    centroides<-cbind(c(1:length(cla$centers)),cla$centers)
    #print(centroides)
    centroides<-centroides[order(centroides[,2]),]
    #print(centroides)
    for(i in 1:length(cla$centers))
      {
       estrato[estrato[,1]==centroides[i],2]<-i
      }
    estrato.c<-cbind(x,estrato[,2])
freq<-table(estrato.c[,3])
freq<-as.matrix(freq)
sumafreq<-array()
for(i in 1:estratos)
  {
   if(i==1)
    {
     sumafreq[i]<-freq[i]+1
    }
   if(i!=1)
    {
     sumafreq[i]<-sumafreq[i-1]+freq[i]
    }
  }
 e<-list()
 for(i in 1:estratos)
   {
    e2<-matrix(rep(0,length(x)),nrow=length(x))
    if(i==1)
     {
      e2<-as.matrix(estrato.c[1:freq[i],2])
      e[[i]]<-e2
     }
    if(i!=1)
     {
      e2<-as.matrix(estrato.c[sumafreq[i-1]:(sumafreq[i]-1),2])
      e[[i]]<-e2
     }
 }
 promedio<-list()
 for(i in 1:estratos)
 {
   promedio[[i]]<-mean(e[[i]])
 }
em<-c(rep(0,estratos))
for(i in 1:estratos)
  {
   em[i]<-max(e[[i]])
}
emin<-c(rep(0,estratos))
for(i in 1:estratos)
{
  emin[i]<-min(e[[i]])
}
IC<-matrix(rep(0,5*estratos),ncol=5)
colnames(IC)<-c("Restricci?n","Lim.Inf","Coma","Lim.Sup","Restricci?n")
w<-c(min(x[,2]),em)
contador<-0
if(estratos>1)
{
  for(i in 2:(length(w)-1))
  {
    if(w[i]==w[i+1])
    {
      contador<-1+contador;
    }
  }
}else{
  for(i in 1:(length(w)-1))
  {
    if(w[i]==w[i+1])
    {
      contador<-1+contador;
    }
  }
}
if(length(table(x[,2]))>=L & contador==0)
 {
  for(i in 1:estratos)
    {
     if(i==1)
      {
     	 IC[i,1]<-","
     	 IC[i,2]<-emin[i]
       IC[i,3]<-";"
       IC[i,4]<-em[i]
       IC[i,5]<-";"
      }
     else
     	{
   	 IC[i,1]<-","
    	 IC[i,2]<-emin[i]
       IC[i,3]<-";"
       IC[i,4]<-em[i]
       IC[i,5]<-";"
      }
    }
  IC<-data.frame(IC)
  IC1<-IC
  freq<-table(estrato.c[,3])
  freq<-as.matrix(freq)
  coma<-rep(";",nrow(freq))
  IC<-cbind(IC,freq,coma)
  #IC1<-cbind(IC1,coma)
  nr<-nrow(IC)
  nc<-ncol(IC)
  temp.dos<-" "
  for(i in 1:nr)
    {
     for(j in 2:nc)
       {
        temp.dos<-paste(temp.dos,IC1[i,j],"",sep="")
       }
     }
 temp1<-list()
 for(i in 1:estratos)
   {
    e1<-matrix(rep(0,length(x)),nrow=length(x))
    if(i==1)
     {
      e1<-as.character(estrato.c[1:freq[i],1])
      temp1[[i]]<-e1
     }
    if(i!=1)
     {
      e1<-as.character(estrato.c[sumafreq[i-1]:(sumafreq[i]-1),1])
      temp1[[i]]<-e1
     }
   }
  z<-as.numeric(as.matrix(x[,2]))
  rango<-strsplit(temp.dos,split=";")
  rangomax<-list()
  rangomin<-list()
  if(intervalos==1)
  {
    for(i in 1:estratos)
    {
      rangomin[[i]]<-rango[[1]][(2*i)-1]
      rangomax[[i]]<-rango[[1]][2*i]
    }
    for(i in 1:estratos)
    {
      if(length(e[[i]])==1)
      {
        rangomin[[i]]<-rangomax[[i]]
      }
    }
  }else{
    for(i in 1:estratos)
    {
      if(i==1)
      {
        rangomin[[i]]<-rango[[1]][(2*i)-1]
        rangomax[[i]]<-rango[[1]][2*i]
      }else{
        rangomin[[i]]<-rango[[1]][2*(i-1)]
        rangomax[[i]]<-rango[[1]][2*i]
      }
    }
  }
}
 }else{
   cla<-0
   temp1<-list()
   rangomin<-""
   rangomax<-""
   promedio<-0
   freq<-0
   contador<-1
   z<-as.matrix(0)
   media<-round(mean(z),digits=3)
   mediana<-round(median(z),digits=3)
   moda<-moda
   devestandar<-round(sd(z),digits=3)
   maximo<-round(max(z),digits=3)
   minimo<-round(min(z),digits=3)
   no.datos<-nrow(datos)
}
}
if(length(table(x[,2]))==1){
  cla<-1
  estratos<-1
  contador<-0
  min<-min(x[,2])
  max<-max(x[,2])
  freq<-as.matrix(table(x[,2]))
  freq<-as.numeric(freq)
  IC<-matrix(rep(0,5*estratos),ncol=5)
  IC[1,2]<-min;
  IC[1,4]<-max;
  IC[1,1]<-",";
  IC[1,5]<-";";
  IC[1,3]<-";";
  promedio[[1]]<-mean(x[,2])
  temp1<-list()
  for(i in 1:estratos)
  {
    e<-matrix(rep(0,length(x)),nrow=length(x))
    e<-as.character(x[1:length(x[,2]),1])
    temp1[[i]]<-e
  }
  nr<-nrow(IC);
  nc<-ncol(IC);
  temp.dos<-""
  for(i in 1:nr)
  {
    for (j in 2:nc)
    {
      temp.dos<-paste(temp.dos,IC[i,j],"",sep="")
    }
  }
  z<-as.numeric(as.matrix(x[,2]))
  rango<-strsplit(temp.dos,split=";")
  rangomin<-list()
  rangomax<-list()
  rangomin[[1]]<-rango[[1]][1]
  rangomax[[1]]<-rango[[1]][2]
}
if(contador!=0)
{
 temp1<-list()
 rangomin<-""
 rangomax<-""
 promedio<-0
 freq<-0
 contador<-1
 z<-as.matrix(0)
 media<-round(mean(z),digits=3)
 mediana<-round(median(z),digits=3)
 moda<-moda
 devestandar<-round(sd(z),digits=3)
 maximo<-round(max(z),digits=3)
 minimo<-round(min(z),digits=3)
 no.datos<-nrow(datos)
}
# Create the first quadrant class
#
# This is used to represent a coordinate in the first quadrant.
Resultado <- setClass(
        # Set the name for the class
        "Resultado",

        # Define the slots
        slots = c(
                frecuencia = "numeric",
                rangomin = "character",
                rangomax = "character",
                entEstratos = "list",
                promedio = "numeric"
                ),

        # Set the default values for the slots. (optional)
        prototype=list(
                frecuencia = 0.0,
                rangomin = "",
                rangomax = ""
                )

        )
# create a method to assign the value of a coordinate
setGeneric(name="setRangomin",
           def=function(theObject,rangominVal)
           {
             standardGeneric("setRangomin")
           }
)

setMethod(f="setRangomin",
          signature="Resultado",
          definition=function(theObject,rangominVal)
          {
            theObject@rangomin <- rangominVal
            return(theObject)
          }
)
setGeneric(name="setRangomax",
           def=function(theObject,rangomaxVal)
           {
             standardGeneric("setRangomax")
           }
)

setMethod(f="setRangomax",
          signature="Resultado",
          definition=function(theObject,rangomaxVal)
          {
            theObject@rangomax <- rangomaxVal
            return(theObject)
          }
)
setGeneric(name="setFrecuencia",
                       def=function(theObject,frecuenciaVal)
                       {
                               standardGeneric("setFrecuencia")
                       }
                       )

setMethod(f="setFrecuencia",
                      signature="Resultado",
                      definition=function(theObject,frecuenciaVal)
                      {
                              theObject@frecuencia <- frecuenciaVal
                              return(theObject)
                      }
                      )
setGeneric(name="setentEstratos",
                       def=function(theObject,entestratosVal)
                       {
                               standardGeneric("setentEstratos")
                       }
                       )

setMethod(f="setentEstratos",
                      signature="Resultado",
                      definition=function(theObject,entestratosVal)
                      {
                              theObject@entEstratos <- as.list(unlist(entestratosVal))
                              return(theObject)
                      }
                      )
setGeneric(name="setPromedio",
           def=function(theObject,promedioVal)
           {
             standardGeneric("setPromedio")
           }
)

setMethod(f="setPromedio",
          signature="Resultado",
          definition=function(theObject,promedioVal)
          {
            theObject@promedio <- promedioVal
            return(theObject)
          }
)

xb<-list()
if(contador==0)
{
if(length(table(datos[,2]))>1)
{
  for(i in 1:estratos)
  {
    x1<- Resultado()
    x1<-setRangomin(x1,rangomin[[i]])
    x1<-setRangomax(x1,rangomax[[i]])
    x1<-setFrecuencia(x1,freq[i])
    x1<-setentEstratos(x1,temp1[i])
    x1<-setPromedio(x1,promedio[[i]])
    xb[[i]]<-x1
  }
  media<-round(mean(z),digits=3)
  mediana<-round(median(z),digits=3)
  moda<-moda
  devestandar<-round(sd(z),digits=3)
  maximo<-round(max(z),digits=3)
  minimo<-round(min(z),digits=3)
  no.datos<-nrow(datos)
}
if(length(table(datos[,2]))==1)
{
  x1<- Resultado()
  x1<-setRangomax(x1,rangomax[[1]])
  x1<-setRangomin(x1,rangomin[[1]])
  x1<-setFrecuencia(x1,freq)
  x1<-setentEstratos(x1,temp1)
  x1<-setPromedio(x1,promedio[[1]])
  xb[[1]]<-x1
  media<-round(mean(z),digits=3)
  mediana<-round(median(z),digits=3)
  moda<-moda
  devestandar<-0
  maximo<-round(max(z),digits=3)
  minimo<-round(min(z),digits=3)
  no.datos<-length(datos[,2])
}
}
if(contador!=0)
{
  x1<- Resultado()
  x1<-setRangomax(x1,rangomax[[1]])
  x1<-setRangomin(x1,rangomin[[1]])
  x1<-setFrecuencia(x1,freq)
  x1<-setentEstratos(x1,temp1)
  x1<-setPromedio(x1,promedio[[1]])
  xb[[1]]<-x1
}
# Create the first quadrant class
#
# This is used to represent a coordinate in the first quadrant.
Resultados <- setClass(
        # Set the name for the class
        "Resultados",

        # Define the slots
        slots = c(
                datos = "list",
                media = "numeric",
                mediana = "numeric",
                moda = "character",
                desEstandar = "numeric",
                maximo = "numeric",
                minimo = "numeric",
                noDatos = "numeric",
                valid = "character",
                validrango = "character"
                ),

        # Set the default values for the slots. (optional)
        prototype=list(
                media = 0.0,
                mediana = 0.0,
                moda = "",
                desEstandar = 0.0,
                maximo = 0.0,
                minimo = 0.0,
                noDatos = 0.0,
                valid = "",
                validrango = ""
                )

        )
setGeneric(name="setDatos",
                       def=function(theObject,datosVal)
                       {
                               standardGeneric("setDatos")
                       }
                       )

setMethod(f="setDatos",
                      signature="Resultados",
                      definition=function(theObject,datosVal)
                      {
                              theObject@datos <- datosVal
                              return(theObject)
                      }
                      )
setGeneric(name="setMedia",
                       def=function(theObject,mediaVal)
                       {
                               standardGeneric("setMedia")
                       }
                       )

setMethod(f="setMedia",
                      signature="Resultados",
                      definition=function(theObject,mediaVal)
                      {
                              theObject@media <- mediaVal
                              return(theObject)
                      }
                      )
setGeneric(name="setMediana",
                       def=function(theObject,medianaVal)
                       {
                               standardGeneric("setMediana")
                       }
                       )

setMethod(f="setMediana",
                      signature="Resultados",
                      definition=function(theObject,medianaVal)
                      {
                              theObject@mediana <- medianaVal
                              return(theObject)
                      }
                      )
setGeneric(name="setModa",
           def=function(theObject,modaVal)
           {
             standardGeneric("setModa")
           }
)

setMethod(f="setModa",
          signature="Resultados",
          definition=function(theObject,modaVal)
          {
            theObject@moda <- modaVal
            return(theObject)
          }
)
setGeneric(name="setdesEstandar",
                       def=function(theObject,desestandarVal)
                       {
                               standardGeneric("setdesEstandar")
                       }
                       )

setMethod(f="setdesEstandar",
                      signature="Resultados",
                      definition=function(theObject,desestandarVal)
                      {
                              theObject@desEstandar <- desestandarVal
                              return(theObject)
                      }
                      )
setGeneric(name="setMaximo",
                       def=function(theObject,maximoVal)
                       {
                               standardGeneric("setMaximo")
                       }
                       )

setMethod(f="setMaximo",
                      signature="Resultados",
                      definition=function(theObject,maximoVal)
                      {
                              theObject@maximo <- maximoVal
                              return(theObject)
                      }
                      )
setGeneric(name="setMinimo",
                       def=function(theObject,minimoVal)
                       {
                               standardGeneric("setMinimo")
                       }
                       )

setMethod(f="setMinimo",
                      signature="Resultados",
                      definition=function(theObject,minimoVal)
                      {
                              theObject@minimo <- minimoVal
                              return(theObject)
                      }
                      )
setGeneric(name="setnoDatos",
                       def=function(theObject,nodatosVal)
                       {
                               standardGeneric("setnoDatos")
                       }
                       )

setMethod(f="setnoDatos",
                      signature="Resultados",
                      definition=function(theObject,nodatosVal)
                      {
                              theObject@noDatos <- nodatosVal
                              return(theObject)
                      }
                      )
setGeneric(name="setValid",
                       def=function(theObject,validVal)
                       {
                               standardGeneric("setValid")
                       }
                       )
setMethod(f="setValid",
                      signature="Resultados",
                      definition=function(theObject,validVal)
                      {
                              theObject@valid <- validVal
                              return(theObject)
                      }
                      )
setGeneric(name="setValidrango",
           def=function(theObject,validrangoVal)
           {
             standardGeneric("setValidrango")
           }
)
setMethod(f="setValidrango",
          signature="Resultados",
          definition=function(theObject,validrangoVal)
          {
            theObject@validrango <- validrangoVal
            return(theObject)
          }
)
temp<-Resultados()
temp<-setDatos(temp,xb)
temp<-setMedia(temp,media)
temp<-setMediana(temp,mediana)
temp<-setModa(temp,moda)
if(length(table(x[,2]))>1){temp<-setdesEstandar(temp,devestandar)}
if(length(table(x[,2]))<=1){temp<-setdesEstandar(temp,0)}
if(length(table(x[,2]))>1){temp<-setMaximo(temp,maximo)}
if(length(table(x[,2]))<=1){temp<-setMaximo(temp,maximo*5)}
temp<-setMinimo(temp,minimo)
temp<-setnoDatos(temp,no.datos)
if(contador==0){temp<-setValid(temp,"true")}
if(contador!=0){temp<-setValid(temp,"false")}
if(length(freq)==estratos){
  temp<-setValidrango(temp,"true")
}else{
  temp<-setValidrango(temp,"false")}
temp<-RJSONIO::toJSON(temp,digits = 12)
temp<-gsub("\\s","",temp,FALSE,TRUE)

return(temp);
$BODY$
  LANGUAGE plr VOLATILE
  COST 100;
ALTER FUNCTION kmedias_json(text, integer, integer)
  OWNER TO postgres;
GRANT EXECUTE ON FUNCTION kmedias_json(text, integer, integer) TO postgres;
