-- Function: jenks_json(text, integer)

-- DROP FUNCTION jenks_json(text, integer);

CREATE OR REPLACE FUNCTION jenks_json(
    sql text,
    estratos integer)
  RETURNS json AS
$BODY$

####Rupturas naturales (Jenks)####
#rm(list=ls(all=TRUE))
#library(foreign)
#library(stratification)
library(jsonlite)
library(BAMMtools)
datos<-pg.spi.exec(sql)
L<-estratos
datos<-datos[order(datos[,2]),]
x<-datos[,2]
options(digits = 12)
xx<-array()
xx[datos[,2]==0]<-1
xx[datos[,2]!=0]<-0
tx<-xx[1]
f<-factor(x)
tf<-tabulate(f)
moda<-as.numeric(levels(f)[tf==max(tf)])
if(length(moda)>1){moda<-"NA"}else
{
  moda<-as.character(moda)}
if(length(table(x))>1)
{
  if(estratos>1)
  {
    if(tx!=0)
    {
     freqceros<-as.matrix(table(x))
     guarda<-datos[1:freqceros[1],]
     clasificacion<-class(guarda)
     if(clasificacion=="matrix"){
       x1<-guarda[,2]}else{
         x1<-guarda[2]
       }
     guarda1<-datos[(freqceros[1]+1):length(x),]
     x2<-guarda1[,2]
     estratos<-estratos-1
    }
    cla2<-getJenksBreaks(x, estratos+1, subset = NULL)
    cla2[1]<-NA
    cla2<-na.omit(cla2)
    factor.c1<-vector(length=nrow(as.matrix(x)))
    if(tx==1)
    {
      factor.c11<-as.matrix(x2)
      factor.c1.1<-as.matrix(x1)
      for(i in 1:estratos)
      {
        if(i==1)
        {
          factor.c11[x2<=cla2[i]]<-i+1
        }
        else
        {
          factor.c11[x2>cla2[i-1] & x2<=cla2[i]]<-i+1
        }
      }
      factor.c1.1[x1<=0]<-1
      factor.c1<-rbind(factor.c1.1,factor.c11)
      estratos<-estratos+1
    }else{
      for(i in 1:estratos)
      {
        if(i==1)
        {
          factor.c1[x<=cla2[i]]<-i
        }
        else
        {
          factor.c1[x>cla2[i-1] & x<=cla2[i]]<-i
        }
      }
    }
    if(length(table(factor.c1))!=estratos)
    {
      reduccion<-1000000000000000000000000000
      contador<-1
    }
    estrato.c2<-cbind(datos,factor.c1)
    freq1<-table(estrato.c2[,3])
    freq1<-as.matrix(freq1)
    if(length(freq1)==estratos)
    {
      sumafreq1<-array()
      for(i in 1:estratos)
      {
        if(i==1)
        {
          sumafreq1[i]<-freq1[i]+1
        }
        if(i!=1)
        {
          sumafreq1[i]<-sumafreq1[i-1]+freq1[i]
        }
      }
      e1<-list()
      for(i in 1:estratos)
      {
        e2<-matrix(rep(0,length(x)),nrow=length(x))
        if(i==1)
        {
          e2<-as.matrix(estrato.c2[1:freq1[i],2])
          e1[[i]]<-e2
        }
        if(i!=1)
        {
          e2<-as.matrix(estrato.c2[sumafreq1[i-1]:(sumafreq1[i]-1),2])
          e1[[i]]<-e2
        }
      }
      e<-e1
      promedio<-list()
      for(i in 1:estratos)
      {
        promedio[[i]]<-mean(e[[i]])
      }
      estrato.c<-estrato.c2
      freq<-freq1
      sumafreq<-sumafreq1
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
      colnames(IC)<-c("Restriccion","Lim.Inf","Coma","Lim.Sup","Restriccion")
      w<-c(min(x),em)
      w<-as.matrix(w)
      w<-na.omit(w)
      contador<-0
      for(i in 2:(length(w)-1))
      {
        if(w[i]==w[i+1]){ contador<-1+contador;print(contador);}
      }
      if(length(table(x))>=L & contador==0)
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
        nr<-nrow(IC)
        nc<-ncol(IC)
        temp.tres<-" "
        for(i in 1:nr)
        {
          for (j in 2:nc)
          {
            temp.tres<-paste(temp.tres,IC[i,j],"",sep="")
          }
        }
        z<-as.numeric(as.matrix(x))
        rango<-strsplit(temp.tres,split=";")
        rangomax<-list()
        rangomin<-list()
        for(i in 1:estratos)
        {
          rangomin[[i]]<-rango[[1]][(2*i)-1]
          rangomax[[i]]<-rango[[1]][2*i]
        }
        clas<-estratos
        for(i in 1:estratos)
        {
          if(length(e[[i]])==1)
          {
            rangomin[[i]]<-rangomax[[i]]
          }
        }
      }
    }
  }
  if(estratos==1)
  {
    cla2<-1
    clas<-1
    min<-min(x)
    max<-max(x)
    freq<-length(x)
    IC<-matrix(rep(0,5*estratos),ncol=5)
    colnames(IC)<-c("Restriccion","Lim.Inf","Coma","Lim.Sup","Restriccion")
    IC[1,1]<-","
    IC[1,2]<-min
    IC[1,3]<-";"
    IC[1,4]<-max
    IC[1,5]<-";"
    promedio<-list()
    promedio[[1]]<-mean(x)
    IC<-data.frame(IC)
    nr<-nrow(IC)
    nc<-ncol(IC)
    temp.tres<-" "
    for(i in 1:nr)
    {
      for (j in 2:nc)
      {
        temp.tres<-paste(temp.tres,IC[i,j],"",sep="")
      }
    }
    z<-as.numeric(as.matrix(x))
    reduccion<-1;reduccion1<-1;
    rango<-strsplit(temp.tres,split=";")
    rangomax<-list()
    rangomin<-list()
    for(i in 1:estratos)
    {
      rangomin[[i]]<-rango[[1]][(2*i)-1]
      rangomax[[i]]<-rango[[1]][2*i]
    }
    contador<-0
  }
  if(contador!=0)
  {
    rangomin<-list()
    rangomax<-list()
    rangomin<-""
    rangomax<-""
    promedio<-0
    freq<-0
    clas<-0
    contador<-1
    z<-as.matrix(0)
    media<-round(mean(z),digits = 3)
    mediana<-round(median(z),digits = 3)
    moda<-moda
    devestandar<-round(sd(z),digits = 3)
    maximo<-round(max(z),digits = 3)
    minimo<-round(min(z),digits = 3)
    no.datos<-nrow(datos)
  }
 }
  if(length(table(x))<=1)
  {
    cla2<-1
    clas<-1
    freq<-1
    IC<-matrix(rep(0,5*1),ncol=5)
    colnames(IC)<-c("Restriccion","Lim.Inf","Coma","Lim.Sup","Restriccion")
    IC[1,1]<-","
    IC[1,2]<-0
    IC[1,3]<-";"
    IC[1,4]<-max(x)
    IC[1,5]<-";"
    promedio<-list()
    promedio[[1]]<-mean(x)
    IC<-data.frame(IC)
    nr<-nrow(IC)
    nc<-ncol(IC)
    temp.tres<-""
    for(i in 1:nr)
    {
      for (j in 2:nc)
      {
        temp.tres<-paste(temp.tres,IC[i,j],"",sep="")
      }
    }
    rango<-strsplit(temp.tres,split=";")
    rangomin<-list()
    rangomax<-list()
    rangomin[[1]]<-rango[[1]][1]
    rangomax[[1]]<-rango[[1]][2]
    z<-as.numeric(as.matrix(x))
    contador<-0
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
  if(length(table(x))>1)
  {
    for(i in 1:estratos)
    {
      x1<- Resultado()
      x1<-setRangomin(x1,rangomin[[i]])
      x1<-setRangomax(x1,rangomax[[i]])
      x1<-setFrecuencia(x1,freq[i])
      x1<-setPromedio(x1,promedio[[i]])
      xb[[i]]<-x1
    }
  }
    media<-round(mean(z),digits = 3)
    mediana<-round(median(z),digits = 3)
    moda<-moda
    devestandar<-round(sd(z),digits = 3)
    maximo<-round(max(z),digits = 3)
    minimo<-round(min(z),digits = 3)
    no.datos<-nrow(datos)
  if(length(table(x))<=1)
  {
    x1<-Resultado()
    x1<-setRangomax(x1,rangomax[[1]])
    x1<-setRangomin(x1,rangomin[[1]])
    x1<-setFrecuencia(x1,freq)
    x1<-setPromedio(x1,promedio[[1]])
    xb[[1]]<-x1
    media<-round(mean(z),digits = 3)
    mediana<-round(median(z),digits = 3)
    moda<-moda
    devestandar<-round(sd(z),digits = 3)
    maximo<-round(max(z),digits = 3)
    minimo<-round(min(z),digits = 3)
    no.datos<-nrow(datos)
  }
  }
  if(contador!=0)
  {
    x1<-Resultado()
    x1<-setRangomax(x1,rangomax[[1]])
    x1<-setRangomin(x1,rangomin[[1]])
    x1<-setFrecuencia(x1,freq)
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
  if(length(x)>1){temp<-setdesEstandar(temp,devestandar)}
  if(length(x)<=1){temp<-setdesEstandar(temp,0)}
  if(length(x)>1){temp<-setMaximo(temp,maximo)}
  if(length(x)<=1){temp<-setMaximo(temp,maximo*5)}
  temp<-setMinimo(temp,minimo)
  temp<-setnoDatos(temp,no.datos)
  if(contador==0){temp<-setValid(temp,"true")}
  if(contador!=0){temp<-setValid(temp,"false")}
  if(clas==estratos){
    temp<-setValidrango(temp,"true")
  }else{
    temp<-setValidrango(temp,"false")}
  temp<-RJSONIO::toJSON(temp,digits=12)
  temp<-gsub("\\s","",temp,FALSE,TRUE)

return(temp);
$BODY$
  LANGUAGE plr VOLATILE
  COST 100;
ALTER FUNCTION jenks_json(text, integer)
  OWNER TO postgres;
GRANT EXECUTE ON FUNCTION jenks_json(text, integer) TO postgres;


-- Function: kmedias_json(text, integer)

-- DROP FUNCTION kmedias_json(text, integer);

CREATE OR REPLACE FUNCTION kmedias_json(
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
ALTER FUNCTION kmedias_json(text, integer)
  OWNER TO postgres;
GRANT EXECUTE ON FUNCTION kmedias_json(text, integer) TO postgres;


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



-- Function: cuantiles2r_json(text, integer)

-- DROP FUNCTION cuantiles2r_json(text, integer);

CREATE OR REPLACE FUNCTION cuantiles2r_json(
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
#filtro por valor constante#
if(length(table(x[,2]))>1)
 {
    probs<-c(rep(0,estratos))
    for(j in 1:estratos)
      {
       probs[j]<-j*1/estratos
      }
    IC<-matrix(rep(0,5*estratos),ncol=5)
    cla<-c(quantile(x[,2],probs= probs,type=6))
    estrato.c<-vector(length=nrow(as.matrix(x[,2])))
    #Agregamos la condicion de numero de niveles de frecuencia de la variable#
    #-verificamos la conformidad de los estratos-#
    contador<-0;
    y<-c(min(x[,2]),cla);
    for(i in 1:(length(y)-1))
      {
       if(y[i]==y[i+1])
        {
         contador<-1+contador
        }
      }
    #--------------------------------------------#
    if(length(table(x[,2]))>=L & contador==0)
     {
      for(i in 1:estratos)
        {
         if(i==1)
          {
           estrato.c[x[,2]<=cla[i]]<-i
          }
         else
          {
           estrato.c[x[,2]>cla[i-1] & x[,2]<=cla[i]]<-i
          }
        }
      estrato<-cbind(x,estrato.c)
      retorno<-list(QUANTILES=cla,ESTRATOS=estrato)
      #Convertimos los IC en un data.frame#
      colnames(IC)<-c("Restricción","Lim.Inf","Coma","Lim.Sup","Restricción");
      IC<-data.frame(IC);
      IC1<-IC
      #------------------------------------------------------------------------------#
      freq<-table(estrato[,3]);
      freq<-as.matrix(freq)
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
          e2<-matrix(rep(0,length(x[,2])),nrow=length(x[,2]))
          if(i==1)
          {
            e2<-as.matrix(estrato[1:freq[i],2])
            e1[[i]]<-e2
          }
          if(i!=1)
          {
            e2<-as.matrix(estrato[sumafreq[i-1]:(sumafreq[i]-1),2])
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
      emax<-c(rep(0,estratos))
      for(i in 1:estratos)
      {
        emax[i]<-max(eco[[i]])
      }
      emin<-c(rep(0,estratos))
      for(i in 1:estratos)
      {
        emin[i]<-min(eco[[i]])
      }
      for(i in 1:estratos)
      {
        if(i==1)
        {
          IC[i,2]<-emin[i];
          IC[i,4]<-emax[i];
          IC[i,1]<-",";
          IC[i,5]<-";";
          IC[i,3]<-";";
        }
        else
        {
          IC[i,2]<-emin[i];
          IC[i,4]<-emax[i];
          IC[i,1]<-",";
          IC[i,5]<-";";
          IC[i,3]<-";";
        }
      }
      IC1<-IC
      coma<-rep(";",nrow(freq));
      IC<-cbind(IC,freq,coma);
      #IC1<-cbind(IC1,coma)
      nr<-nrow(IC);
      nc<-ncol(IC);
      temp.dos<-" "
      for(i in 1:nr)
        {
         for (j in 2:nc)
           {
            temp.dos<-paste(temp.dos,IC1[i,j],"",sep="")
           }
        }
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
      temp1<-list()
      for(i in 1:estratos)
        {
         e<-matrix(rep(0,length(x)),nrow=length(x))
         if(i==1)
          {
           e<-as.character(estrato[1:freq[i],1])
           temp1[[i]]<-e
          }
         if(i!=1)
          {
           e<-as.character(estrato[sumafreq[i-1]:(sumafreq[i]-1),1])
           temp1[[i]]<-e
          }
        }
      z<-as.numeric(as.matrix(x[,2]))
      rango<-strsplit(temp.dos,split=";")
      rangomax<-list()
      rangomin<-list()
      for(i in 1:estratos)
      {
        rangomin[[i]]<-rango[[1]][(2*i)-1]
        rangomax[[i]]<-rango[[1]][2*i]
      }
    for(i in 1:estratos)
    {
      if(length(eco[[i]])==1)
      {
        rangomin[[i]]<-rangomax[[i]]
      }
    }
    }
}else{
   cla<-1
   estrato<-1
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
   promedio<-list()
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
   rango<-(strsplit(temp.dos,split=";"))
   rangomin<-list()
   rangomax<-list()
   rangomin[[1]]<-rango[[1]][1]
   rangomax[[1]]<-rango[[1]][2]
   z<-as.numeric(as.matrix(x))
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
 no.datos<-length(x[,2])
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
if(length(table(x[,2]))>1)
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
  no.datos<-length(x[,2])
}
if(length(table(x[,2]))==1)
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
  devestandar<-round(sd(z),digits=3)
  maximo<-round(max(z),digits=3)
  minimo<-round(min(z),digits=3)
  no.datos<-length(x[,2])
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
if(length(x[,2])>1){temp<-setdesEstandar(temp,devestandar)}
if(length(x[,2])<=1){temp<-setdesEstandar(temp,0)}
if(length(x[,2])>1){temp<-setMaximo(temp,maximo)}
if(length(x[,2])<=1){temp<-setMaximo(temp,maximo*5)}
temp<-setMinimo(temp,minimo)
temp<-setnoDatos(temp,no.datos)
if(contador==0){temp<-setValid(temp,"true")}
if(contador!=0){temp<-setValid(temp,"false")}
if(length(x[,2])<=1){temp<-setValidrango(temp,"true")}else{
if(length(cla)==estratos & length(x[,2])>estratos){
  temp<-setValidrango(temp,"true")
}else{
  temp<-setValidrango(temp,"false")}
}
temp<-RJSONIO::toJSON(temp,digits = 12)
temp<-gsub("\\s","",temp,FALSE,TRUE)

return(temp);
$BODY$
  LANGUAGE plr VOLATILE
  COST 100;
ALTER FUNCTION cuantiles2r_json(text, integer)
  OWNER TO postgres;
GRANT EXECUTE ON FUNCTION cuantiles2r_json(text, integer) TO postgres;



-- Function: daleniushodge2r_json(text, integer)

-- DROP FUNCTION daleniushodge2r_json(text, integer);

CREATE OR REPLACE FUNCTION daleniushodge2r_json(
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
options(digits = 12)
f<-factor(datos[,2])
tf<-tabulate(f)
moda<-as.numeric(levels(f)[tf==max(tf)])
if(length(moda)>1){moda<-"NA"}else
{
  moda<-as.character(moda)}
if(L!=1)
 {
  if(length(table(datos[,2]))>1)
   {
    #filtro 1#
    if(length(table(datos[,2]))>L)
     {
      x<-sort(datos[,2])
      L<-estratos
      nclass<-min(L*10,length(x))
      cla<-x[1]+(x[length(x)]-x[1])*((0:nclass)/nclass)
      cla[nclass+1]<-cla[nclass+1]+1
      factor.c<-vector(length = length(x))
      for(i in 1:nclass)
        {
         factor.c[x>=cla[i] & x<cla[i+1]]<-i
        }
      freq.c<-rep(0, nclass)
      pres.c<-tapply(x, factor.c, length)
      freq.c[as.numeric(names(pres.c))]<-as.vector(pres.c)
      csfreq.c<-cumsum(sqrt(freq.c))
      but<-csfreq.c[nclass]/L
      nclass.temp<-vector(length=2^(L-1)-1)
      csfreqh.temp<-rep(0, 2^(L-1))
      nclassh<-ssfreqh<-matrix(0, L, 2^(L-1))
      sous<-0
      k<-1
      for(i in 1:(L-1))
        {
         k1<-k
         for(j in 1:length(sous))
           {
            a<-csfreq.c - sous[j]
            b<-a[a > 0 & a < but]
            nclass.temp[k]<-length(b)
            k<-k+1
           }
         nclassh[i,]<-rep(c(t(cbind(nclass.temp[k1:(k-1)],nclass.temp[k1:(k-1)]+1))),each=(2^(L-2))/(k-k1))
         cumnclass<-colSums(nclassh[1:i, , drop = FALSE])
         cumss<-csfreqh.temp
         csfreqh.temp[cumnclass!=0]<-csfreq.c[cumnclass]
         csfreqh.temp[cumnclass==0]<-0
         ssfreqh[i, ]<-csfreqh.temp-cumss
         pos<-seq(1, 2^(L-1), (2^(L-2))/(k-k1))
         sous<-colSums(ssfreqh)[pos]
        }
      nclassh[L,]<-nclass-cumnclass
      ssfreqh[L,]<-csfreq.c[nclass]-sous
      out<-apply(nclassh<=0, 2, any)
      very<-(ssfreqh[,!out]-but)
      #segundo filtro#
      if(is.matrix(very)==TRUE)
       {
        nrgr<-order(colSums((ssfreqh[,!out]-but)^2))[1]
        bhfull<-cla[cumsum(c(1, nclassh[,!out][,nrgr]))]
        IC<-matrix(rep(0, 4*L), ncol=4)
        for(j in 1:L)
          {
           IC[j,1]<-bhfull[j]
           IC[j,2]<-";"
           IC[j,3]<-bhfull[j+1]
           IC[j,4]<-";"
          }
        contador<-0;
        for(i in 1:nrow(IC))
          {
           if(IC[i,1]==IC[i,2])
            {
             contador<-1+contador
            }
          }
        factor.c<-vector(length=length(x))
        for(i in 1:L)
          {
           factor.c[x>=bhfull[i] & x<bhfull[i+1]]<-i
          }
        estrato.c<-cbind(datos,factor.c)
        freq<-table(factor.c)
        freq<-as.matrix(freq)
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
        emax<-c(rep(0,estratos))
        for(i in 1:estratos)
        {
          emax[i]<-max(eco[[i]])
        }
        emin<-c(rep(0,estratos))
        for(i in 1:estratos)
        {
          emin[i]<-min(eco[[i]])
        }
        for(j in 1:L)
        {
          IC[j,1]<-emin[j]
          IC[j,2]<-";"
          IC[j,3]<-emax[j]
          IC[j,4]<-";"
        }
        nr<-nrow(IC)              
        nc<-ncol(IC)              
        temp.dos<-" "
        for(i in 1:nr)
        {
          for (j in 1:nc)
          {
            temp.dos<-paste(temp.dos,IC[i,j],sep="")
          }
        }
        rango<-strsplit(temp.dos,split=";")
        rangomax<-list()
        rangomin<-list()
        for(i in 1:estratos)
        {
          rangomin[[i]]<-rango[[1]][(2*i)-1]
          rangomax[[i]]<-rango[[1]][2*i]
        }
        for(i in 1:estratos)
        {
          if(length(eco[[i]])==1)
          {
            rangomin[[i]]<-rangomax[[i]]
          }
        }
        #Tercer filtro#
        if(length(table(factor.c))==L)
         {
          limites<-cbind(IC,table(factor.c))
          colnames(limites)<-c("Lim.Inf", "coma", "Lim.Sup", "coma", "Frecuencia")
          freq<-table(factor.c)
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
          temp1<-list()
          for(i in 1:estratos)
            {
             e<-matrix(rep(0,length(x)),nrow=length(x))
             if(i==1)
              {
               e<-as.character(estrato.c[1:freq[i],1])
               temp1[[i]]<-e
              }
             if(i!=1)
              {
               e<-as.character(estrato.c[sumafreq[i-1]:(sumafreq[i]-1),1])
               temp1[[i]]<-e
              }
            }
          z<-as.numeric(as.matrix(x))
         }
      }else{contador<-1}
  }else{
    cla<-1
    z<-datos[,2]
    x<-datos[,2]
    temp1<-list()
    rangomax<-""
    rangomin<-""
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
    no.datos<-length(x)
  }
  }
  if(length(table(datos[,2]))<=1){
    x<-sort(datos[,2])
    cla<-1
    estratos<-1
    L<-estratos
    factor.c<-L
    contador<-0
    min<-min(x)
    max<-max(x)
    freq<-as.matrix(table(x))
    freq<-as.numeric(freq)
    IC<-matrix(rep(0,5*estratos),ncol=5)
    IC[1,2]<-min;
    IC[1,4]<-max;
    IC[1,1]<-",";
    IC[1,5]<-";";
    IC[1,3]<-";";
    promedio<-list()
    promedio[[1]]<-mean(x)
    temp1<-list()
    for(i in 1:estratos)
    {
      e<-matrix(rep(0,length(x)),nrow=length(x))
      e<-as.character(datos[1:length(x),1])
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
    z<-as.numeric(as.matrix(datos[,2]))
    rango<-strsplit(temp.dos,split=";")
    rangomin<-list()
    rangomax<-list()
    rangomin[[1]]<-rango[[1]][1]
    rangomax[[1]]<-rango[[1]][2]
     }
   #}
 }
if(estratos==1)
 {
  cla<-1
  contador<-0
  x<-sort(datos[,2])
  pru<-min(datos[,2])
  pru1<-max(datos[,2])
  IC<-matrix(rep(0, 4*L), ncol=4)
  for(j in 1:L)
    {
     IC[j,1]<-pru
     IC[j,2]<-";"
     IC[j,3]<-pru1
     IC[j,4]<-";"
  }
  promedio<-list()
  promedio[[1]]<-mean(x)
  nr<-nrow(IC)              
  nc<-ncol(IC)              
  temp.dos<-" "
  for(i in 1:nr)
    {
     for (j in 1:nc)
       {
        temp.dos<-paste(temp.dos,IC[i,j],sep="")
       }
    }
  rango<-strsplit(temp.dos,split=";")
  rangomin<-list()
  rangomax<-list()
  rangomin[[1]]<-rango[[1]][1]
  rangomax[[1]]<-rango[[1]][2]
  factor.c<-vector(length=length(x))
  for(i in 1:L)
    {
     factor.c[x<=pru1]<-i
    }
  estrato.c<-cbind(datos,factor.c)
  if(length(table(factor.c))==L)
   {
    limites<-cbind(IC,table(factor.c))
    colnames(limites)<-c("Lim.Inf", "coma", "Lim.Sup", "coma","Frecuencia")
    freq<-table(factor.c)
    freq<-as.numeric(as.matrix(freq))
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
    temp1<-list()
    for(i in 1:estratos)
      {
       e<-matrix(rep(0,length(x)),nrow=length(x))
       if(i==1)
        {
         e<-as.character(estrato.c[1:freq[i],1])
         temp1[[i]]<-e
        }
       if(i!=1)
        {
         e<-as.character(estrato.c[sumafreq[i-1]:(sumafreq[i]-1),1])
         temp1[[i]]<-e
        }
      }
    z<-as.numeric(as.matrix(x))
   }
 }
if(contador!=0 || (length(table(factor.c))!=L))
{
 temp1<-list()
 rangomin<-""
 rangomax<-""
 promedio<-0
 freq<-0
 cla<-1
 contador<-1
 z<-as.matrix(0)
 media<-round(mean(z),digits=3)
 mediana<-round(median(z),digits=3)
 moda<-moda
 devestandar<-round(sd(z),digits=3)
 maximo<-round(max(z),digits=3)
 minimo<-round(min(z),digits=3)
 no.datos<-length(x)
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
 cla<-rep(0,estratos)
}
}
  media<-round(mean(z),digits=3)
  mediana<-round(median(z),digits=3)
  moda<-moda
  devestandar<-round(sd(z),digits=3)
  maximo<-round(max(z),digits=3)
  minimo<-round(min(z),digits=3)
  no.datos<-length(x)
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
if(length(table(x))>1){temp<-setdesEstandar(temp,devestandar)}
if(length(table(x))<=1){temp<-setdesEstandar(temp,0)}
if(length(table(x))>1){temp<-setMaximo(temp,maximo)}
if(length(table(x))<=1){temp<-setMaximo(temp,maximo*5)}
temp<-setMinimo(temp,minimo)
temp<-setnoDatos(temp,no.datos)
if(contador==0){temp<-setValid(temp,"true")}
if(contador!=0){temp<-setValid(temp,"false")}
if(length(cla)==estratos & length(table(x))>estratos){
  temp<-setValidrango(temp,"true")
}else{
  temp<-setValidrango(temp,"false")}
temp<-RJSONIO::toJSON(temp,digits = 12)
temp<-gsub("\\s","",temp,FALSE,TRUE)

return(temp);
$BODY$
  LANGUAGE plr VOLATILE
  COST 100;
ALTER FUNCTION daleniushodge2r_json(text, integer)
  OWNER TO postgres;
GRANT EXECUTE ON FUNCTION daleniushodge2r_json(text, integer) TO postgres;