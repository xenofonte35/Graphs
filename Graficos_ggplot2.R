library(readr)
PELIS <- read_csv("iCloud Drive (archivo)/R/PELIS.csv")
View(PELIS)
head(PELIS)
tail(PELIS)#cambiamos el nombre
colnames(PELIS)<-c("Pelicula","Genero","Ratingcriticos","Ratingsaudiencia","Presupuestomillones","Año")
head(PELIS)
str(PELIS)
summary(PELIS)
names(PELIS)
factor(PELIS$Año)#lo convertimos a factor
PELIS$Año<-factor(PELIS$Año)
summary(PELIS)#me dice cuantas peliculas se lanzaron en cada uno de los años
library(ggplot2)#el presupuesto en tamaño
ggplot(data = PELIS,aes(x=Ratingcriticos,y=Ratingsaudiencia,color=Genero, size=Presupuestomillones))+geom_point()
#es nuestro primer entregable
#asignamos el codigo anterior a un objeto pero sin el +geom_point()
p<-ggplot(data = PELIS,aes(x=Ratingcriticos,y=Ratingsaudiencia,color=Genero, size=Presupuestomillones))
p+geom_line()#nos muestra lineas
p+geom_line()+geom_point()
p+geom_point(aes(size=Ratingcriticos))# el tamaño se incrementa conforme nos movemos a la derecha
p+geom_point(aes(color=Presupuestomillones))
p+geom_point(aes(x=Presupuestomillones))#nos muestra otra forma destacando el presupuesto
#un mayor presupuesto no afecta en los ratings de la audiencia
#le podemos cambiar el nombre al eje de las x´s
p+geom_point(aes(x=Presupuestomillones))+xlab("Presupuesto Millones")
p+geom_line()+geom_point()# se ven puras lineas; vamos a cambias el tamaño de las lineas
p+geom_line(size=1)+geom_point()
#mapear vs establecer (settings)
#definimos un nuevo objeto
r<-ggplot(data=PELIS,aes(x=Ratingcriticos ,y=Ratingsaudiencia))
r+geom_point()
#para agregar color hay dos maneras
#1 mapeando, determinas el color de acuerdo a la variable
r+geom_point(aes(color=Genero))
r+geom_point(aes(size=Presupuestomillones))
#2 estableciendo; solo determinas el color
r+geom_point(color="Darkgreen")

#histogramas y diagramas de densidad
s<-ggplot(data=PELIS,aes(x=Presupuestomillones))
s+geom_histogram(binwidth = 10)
#agregamos color
s+geom_histogram(binwidth = 10,fill="green")
#ahora mapeamos
s+geom_histogram(binwidth = 10,aes(fill=Genero))
#para distinguir los colores en el histograma ponemos una raya negra
s+geom_histogram(binwidth = 10,aes(fill=Genero),color="black")
#densidad; solo se ve una linea
s+geom_density()
#le ponemos color al diagrama de densidad
s+geom_density(aes(fill=Genero))
#para distinguir las areas
s+geom_density(aes(fill=Genero),position = "stack")
#para capas
t<-ggplot(data=PELIS,aes(x=Ratingsaudiencia))
t+geom_histogram(binwidth = 10,fill="white",color="blue")
#otra manera
t2<-ggplot(data=PELIS)
t2+geom_histogram(aes(Ratingsaudiencia),binwidth = 10,fill="white",color="blue")
# podemos usar las estructuras para cambiar variables
#usando el smooth
u<-ggplot(data = PELIS,aes(x=Ratingcriticos ,y=Ratingsaudiencia, color=Genero))
u + geom_point() + geom_smooth(fill=NA)
u2<-ggplot(data=PELIS,aes(x=Genero,y=Ratingsaudiencia,color=Genero))
u2 + geom_boxplot()+ geom_jitter()#boxplot
#otra apreciacion para transparentar los datos
u2 + geom_jitter() + geom_boxplot(size=1.2,alpha=0.5)
#histogramas
v<-ggplot(data=PELIS,aes(Presupuestomillones))
#para presentar histogramas de distintas variables en una sola
v + geom_histogram(binwidth = 10,aes(fill=Genero),color="black") + facet_grid(Genero~.)
#para escalas individuales, aplicamos escala
v + geom_histogram(binwidth = 10,aes(fill=Genero),color="black") + facet_grid(Genero~.,scales = "free")
#diagramas de dispersion con facetas y por año
w<-ggplot(data = PELIS, aes(x=Ratingcriticos,y=Ratingsaudiencia,color=Genero))
w + geom_point(size=2)
w + geom_point(size=2) + facet_grid(.~Año)
#agregamos dos ejes
w + geom_point(size=2) + facet_grid(Genero~Año)
#ahora con banda
w + geom_point(size=2) + facet_grid(Genero~Año)+geom_smooth()
#por presupuestp
w + geom_point(aes(size=Presupuestomillones)) + facet_grid(Genero~Año)+geom_smooth()
# coordenadas
m<-ggplot(data = PELIS, aes(x=Ratingcriticos,y=Ratingsaudiencia,color=Genero))
m+geom_point()+xlim(50,100)+ylim(50,100)
n<-ggplot(data=PELIS,aes(Presupuestomillones))
n+ geom_histogram(binwidth = 10,aes(fill=Genero),color="black")+coord_cartesian(ylim=c(0,50))
f<-ggplot(data = PELIS, aes(x=Ratingcriticos,y=Ratingsaudiencia,color=Genero))+geom_point(aes(size=Presupuestomillones)) + facet_grid(Genero~Año)+geom_smooth()
f+coord_cartesian(ylim = c(0,100))
#agregando temas
g<-ggplot(data=PELIS,aes(x=Presupuestomillones))
h<-g+geom_histogram(binwidth = 10,aes(fill=Genero),color="black")
h+xlab("Presupuesto")+ylab("Número de películas")+ggtitle("Distribución de presupuesto en películas")+theme(axis.title.x = element_text(color = "DarkGreen",size = 20),
                                                        axis.title.y = element_text(color = "DarkGreen",size = 20),
                                                        axis.text.x =element_text(size=15),
                                                        axis.text.y =element_text(size=15),
                                                        legend.title = element_text(size = 20),
                                                        legend.text = element_text(size = 15),
                                                        legend.position = c(0.98,0.98),#1,1:extremo superior derecho; 0,0:extremo inferior izquierdo
                                                        legend.justification = c(1,1),
                                                        plot.title = element_text(color = "DarkBlue",size=25,hjust = 0.5))
?theme


