-- Function: neir_json(text, integer)

-- DROP FUNCTION neir_json(text, integer);

CREATE OR REPLACE FUNCTION neir_json(
    sql text,
    estratos integer)
  RETURNS json AS
$BODY$

####Rupturas naturales (Jenks)####
#rm(list=ls(all=TRUE))
#library(foreign)
#library(stratification)
library(jsonlite)
datos<-pg.spi.exec(sql)
datos<-datos[order(datos[,2]),]
L<-estratos
x<-datos[,2];
options(digits = 12)
f<-factor(x)
tf<-tabulate(f)
moda<-as.numeric(levels(f)[tf==max(tf)])
if(length(moda)>1){moda<-"NA"}else
{
  moda<-as.character(moda)}
#filtro por valor constante#    
if(length(table(x))>1)
{
  if(length(table(x))>=L)
  {
    contador<-0
  }else{
    contador<-1
  }
  if(contador==0)
  {
    n.obs<-nrow(as.matrix(x))
    k<-n.obs/estratos
    elementos<-vector(length=estratos)
    diferencia<-vector(length=estratos)
    for(i in 1:estratos)
    { elementos[i]<-floor(i*k)
    }
    lim.inf<-vector(length=estratos);
    lim.sup<-vector(length=estratos);
    for (i in 1:estratos)
    { if(i==1)
    {  lim.inf[i]<-1;
    lim.sup[i]<-elementos[i];
    
    }
      else
      {   lim.inf[i]<-elementos[i-1]+1;
      lim.sup[i]<-elementos[i]}
    }
    factor.c <- vector(length=nrow(as.matrix(x)))
    for (i in 1:estratos)
    {
      factor.c[lim.inf[i]:lim.sup[i]]<-i
    }
    estrato.c<-cbind(datos,factor.c)
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
    if(length(freq)==estratos)
    {
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
      e1<-list()
      for(i in 1:estratos)
      {
        e2<-matrix(rep(0,length(x)),nrow=length(x))
        if(i==1)
        {
          e2<-as.matrix(estrato.c[1:freq[i],2])
          e1[[i]]<-e2
        }
        if(i!=1)
        {
          e2<-as.matrix(estrato.c[sumafreq[i-1]:(sumafreq[i]-1),2])
          e1[[i]]<-e2
        }
      }
      eco<-e1
    }
    promedio<-list()
    for(i in 1:estratos)
    {
      promedio[[i]]<-mean(eco[[i]])
    }
    datos<-data.frame(cbind(datos,factor.c))
    #------------------------------------------------------------------------------#
    # cosntruimos los IC con los id geograficos para cada observación              #
    #------------------------------------------------------------------------------#
    temp.m<-matrix(rep(0,estratos*4),ncol=4);
    for(i in 1:estratos)
    { 
      estrato.i<-datos[datos[,3]==i,]
      temp.m[i,1]<-min(estrato.i[,2]);temp.m[i,3]<-max(estrato.i[,2]);temp.m[i,2]<-",";temp.m[i,4]<-";"; 
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
    temp.dos<-"";
    for(i in 1:nrow(temp.m))
    {
      for(j in 1:ncol(temp.m))
      {
        temp.dos<-paste(temp.dos,temp.m[i,j],sep="")
      }
    }
    rango<-strsplit(temp.dos,split=";")
    rangomin<-list()
    rangomax<-list()
    rangomin<-"NA"
    rangomax<-"NA"
    contador<-0
    cla<-estratos
    z <- table(as.matrix(datos[,2]))
    numerico<-datos[,2]
  }else{
    numerico<-datos[,2]
    temp1<-list()
    rangomin<-list()
    rangomax<-list()
    rangomin<-""
    rangomax<-""
    promedio<-0
    freq<-0
    cla<-estratos
    contador<-1
    z<-as.matrix(0)
    media<-round(mean(0),digits=3)
    mediana<-round(median(0),digits=3)
    moda<-moda
    devestandar<-round(sd(0),digits=3)
    maximo<-round(max(numerico),digits=3)
    minimo<-round(min(numerico),digits=3)
    no.datos<-nrow(datos)
  }
}else{                                   #termina filtro por valor constante#
  if(length(table(x))==1 && estratos>1)
  {
    numerico<-datos[,2]
    temp1<-list()
    rangomin<-list()
    rangomax<-list()
    rangomin<-""
    rangomax<-""
    promedio<-0
    freq<-0
    cla<-estratos
    contador<-1
    z<-as.matrix(0)
    media<-round(mean(numerico),digits=3)
    mediana<-round(median(numerico),digits=3)
    moda<-moda
    devestandar<-round(sd(numerico),digits=3)
    maximo<-round(max(numerico),digits=3)
    minimo<-round(min(numerico),digits=3)
    no.datos<-nrow(datos)
  }
  if(length(table(x))==1 && estratos==1)
  {
    minimo<-min(x)
    maximo<-max(x)
    cla<-estratos
    contador<-0
    c<-matrix(rep(1,length(x)),ncol=1)
    datos<-cbind(datos,c)
    estrato.c<-cbind(datos,c)
    freq<-table(estrato.c[,3])
    freq<-as.matrix(freq)
    promedio<-list()
    promedio[[1]]<-mean(x)
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
    IC<-matrix(rep(0,estratos*4),ncol=4);
    for(i in 1:estratos)
    {
      estrato.i<-datos[datos[,3]==i,]
      IC[i,1]<-min(estrato.i[,2]);IC[i,3]<-max(estrato.i[,2]);IC[i,2]<-",";IC[i,4]<-";";
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
    nr<-nrow(IC)              
    nc<-ncol(IC)              
    temp.dos<-""
    for(i in 1:nr)
    {
      for (j in 1:nc)
      {
        temp.dos<-paste(temp.dos,IC[i,j],"",sep="")
      }
    }
    rango<-strsplit(temp.dos,split=";")
    rangomin<-list()
    rangomax<-list()
    rangomin<-"NA"
    rangomax<-"NA"
    z<-table(as.matrix(datos[,2]))
    numerico<-datos[,2]
  }
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
    rango = ""
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
if(length(table(x))>1)
{
  for(i in 1:estratos)
  {
    x1<- Resultado()
    x1<-setRangomin(x1,"NA")
    x1<-setRangomax(x1,"NA")
    x1<-setFrecuencia(x1,freq[i])
    x1<-setentEstratos(x1,temp1[i])
    x1<-setPromedio(x1,promedio[[i]])
    xb[[i]]<-x1
  }
}
if(length(table(x))<=1)
{
  i<-1
  x1<-Resultado()
  x1<-setRangomin(x1,"NA")
  x1<-setRangomax(x1,"NA")
  x1<-setFrecuencia(x1,freq)
  x1<-setentEstratos(x1,temp1)
  x1<-setPromedio(x1,promedio[[1]])
  xb[[1]]<-x1
  media<-round(mean(numerico),digits=3)
  mediana<-round(median(numerico),digits=3)
  moda<-moda
  devestandar<-round(sd(numerico),digits=3)
  maximo<-round(max(numerico),digits=3)
  minimo<-round(min(numerico),digits=3)
  no.datos<-nrow(datos)
}
if(contador==0)
{
  media<-round(mean(numerico),digits=3)
  mediana<-round(median(numerico),digits=3)
  moda<-moda
  devestandar<-round(sd(numerico),digits=3)
  maximo<-round(max(numerico),digits=3)
  minimo<-round(min(numerico),digits=3)
  no.datos<-nrow(datos)
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
if(length(x)>1){temp<-setdesEstandar(temp,devestandar)}
if(length(x)<=1){temp<-setdesEstandar(temp,0)}
if(length(x)>1){temp<-setMaximo(temp,maximo)}
if(length(x)<=1){temp<-setMaximo(temp,maximo*5)}
temp<-setMinimo(temp,minimo)
temp<-setnoDatos(temp,no.datos)
if(contador==0){temp<-setValid(temp,"true")}
if(contador!=0){temp<-setValid(temp,"false")}
if(cla==estratos){
  temp<-setValidrango(temp,"true")
}else{
  temp<-setValidrango(temp,"false")}
temp<-RJSONIO::toJSON(temp,digits = 12)
temp<-gsub("\\s","",temp,FALSE,TRUE)

return(temp);
$BODY$
  LANGUAGE plr VOLATILE
  COST 100;
ALTER FUNCTION neir_json(text, integer)
  OWNER TO postgres;
GRANT EXECUTE ON FUNCTION neir_json(text, integer) TO postgres;
