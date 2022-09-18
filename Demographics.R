carlos <- 1:5
carlos
names(carlos)<-c("a","b","c","d","e")
carlos
#para borrar los nombres usamos NULL
names(carlos)<-NULL
carlos
head(datos)
#usando el simbolo de dolar $
datos[4,3]#obtenemos la tasa de natalidad de albania[fila,columna]
datos[4,"Tasa.Natalidad"]#podemos escribir el nombre de la columna como aparece en la base de datos
datos$Tasa.Natalidad
datos[,"Tasa.Natalidad"]#arroja el mismo resultado
datos$Tasa.Natalidad[4]#con esto obtengo tambien la tasa de natalidad de albania
levels(datos$Grupo.Ingresos)
#operaciones basicas
#subsetting
datos[3:9,]#quieri de la fila 3 a la 9 y todas las columnas
datos[c(4,100),]#quiero la 4 y la 100 y todas las columnas
datos[,1]#solo la primera columna
datos[,1,drop=F]#si quiero que siga siendo data.frame
datos$xyz<-1:5
head(datos,n=10)#he agregado una columna en la que aparece la secuencia 1,2,3,4,5
datos$xyz<-NULL #esto es para desaparecer la columna
head(datos, n=5)
#filtrando marcos de datos
#quiero ver aquellos paises con internet menor al 2%
datos$Penetracion.Internet<2
filtro<-datos$Penetracion.Internet<2
datos[filtro,]#me muestra los paises con internet menor que dos
#otra forma es
datos[datos$Tasa.Natalidad<40,]#no olvidar la coma(por c/1000 ha, hay 40 nacimiento por año)
datos[datos$Grupo.Ingresos=="Ingreso alto",]#solo con ingreso alto
#podemos combinar para cumplir dos condiciones
datos[datos$Tasa.Natalidad>40&datos$Penetracion.Internet<2,]
#para buscar un país, por ejemplo Mexico
datos[datos$Nombre.Pais=="Mexico",]
#qplot, ayuda ?plot
library(ggplot2)
qplot(data = datos, x=Penetracion.Internet)
qplot(data = datos, x=Grupo.Ingresos,y=Tasa.Natalidad,color=I("blue"), size=(3))
qplot(data = datos, x=Grupo.Ingresos,y=Tasa.Natalidad,geom = "boxplot")
qplot(data=datos, x=Penetracion.Internet, y=Tasa.Natalidad,size=I(5),
      color=Grupo.Ingresos)
#incluimos la region
Pais_dataset_p2 <- c("Aruba","Afghanistan","Angola","Albania","United Arab Emirates","Argentina","Armenia","Antigua and Barbuda","Australia","Austria","Azerbaijan","Burundi","Belgium","Benin","Burkina Faso","Bangladesh","Bulgaria","Bahrain","Bahamas, The","Bosnia and Herzegovina","Belarus","Belize","Bermuda","Bolivia","Brazil","Barbados","Brunei Darussalam","Bhutan","Botswana","Central African Republic","Canada","Switzerland","Chile","China","Cote d'Ivoire","Cameroon","Congo, Rep.","Colombia","Comoros","Cabo Verde","Costa Rica","Cuba","Cayman Islands","Cyprus","Czech Republic","Germany","Djibouti","Denmark","Dominican Republic","Algeria","Ecuador","Egypt, Arab Rep.","Eritrea","Spain","Estonia","Ethiopia","Finland","Fiji","France","Micronesia, Fed. Sts.","Gabon","United Kingdom","Georgia","Ghana","Guinea","Gambia, The","Guinea-Bissau","Equatorial Guinea","Greece","Grenada","Greenland","Guatemala","Guam","Guyana","Hong Kong SAR, China","Honduras","Croatia","Haiti","Hungary","Indonesia","India","Ireland","Iran, Islamic Rep.","Iraq","Iceland","Israel","Italy","Jamaica","Jordan","Japan","Kazakhstan","Kenya","Kyrgyz Republic","Cambodia","Kiribati","Korea, Rep.","Kuwait","Lao PDR","Lebanon","Liberia","Libya","St. Lucia","Liechtenstein","Sri Lanka","Lesotho","Lithuania","Luxembourg","Latvia","Macao SAR, China","Morocco","Moldova","Madagascar","Maldives","Mexico","Macedonia, FYR","Mali","Malta","Myanmar","Montenegro","Mongolia","Mozambique","Mauritania","Mauritius","Malawi","Malaysia","Namibia","New Caledonia","Niger","Nigeria","Nicaragua","Netherlands","Norway","Nepal","New Zealand","Oman","Pakistan","Panama","Peru","Philippines","Papua New Guinea","Poland","Puerto Rico","Portugal","Paraguay","French Polynesia","Qatar","Romania","Russian Federation","Rwanda","Saudi Arabia","Sudan","Senegal","Singapore","Solomon Islands","Sierra Leone","El Salvador","Somalia","Serbia","South Sudan","Sao Tome and Principe","Suriname","Slovak Republic","Slovenia","Sweden","Swaziland","Seychelles","Syrian Arab Republic","Chad","Togo","Thailand","Tajikistan","Turkmenistan","Timor-Leste","Tonga","Trinidad and Tobago","Tunisia","Turkey","Tanzania","Uganda","Ukraine","Uruguay","United States","Uzbekistan","St. Vincent and the Grenadines","Venezuela, RB","Virgin Islands (U.S.)","Vietnam","Vanuatu","West Bank and Gaza","Samoa","Yemen, Rep.","South Africa","Congo, Dem. Rep.","Zambia","Zimbabwe")
Codigo_Pais_dataset_p2 <- c("ABW","AFG","AGO","ALB","ARE","ARG","ARM","ATG","AUS","AUT","AZE","BDI","BEL","BEN","BFA","BGD","BGR","BHR","BHS","BIH","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN","BWA","CAF","CAN","CHE","CHL","CHN","CIV","CMR","COG","COL","COM","CPV","CRI","CUB","CYM","CYP","CZE","DEU","DJI","DNK","DOM","DZA","ECU","EGY","ERI","ESP","EST","ETH","FIN","FJI","FRA","FSM","GAB","GBR","GEO","GHA","GIN","GMB","GNB","GNQ","GRC","GRD","GRL","GTM","GUM","GUY","HKG","HND","HRV","HTI","HUN","IDN","IND","IRL","IRN","IRQ","ISL","ISR","ITA","JAM","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAC","MAR","MDA","MDG","MDV","MEX","MKD","MLI","MLT","MMR","MNE","MNG","MOZ","MRT","MUS","MWI","MYS","NAM","NCL","NER","NGA","NIC","NLD","NOR","NPL","NZL","OMN","PAK","PAN","PER","PHL","PNG","POL","PRI","PRT","PRY","PYF","QAT","ROU","RUS","RWA","SAU","SDN","SEN","SGP","SLB","SLE","SLV","SOM","SRB","SSD","STP","SUR","SVK","SVN","SWE","SWZ","SYC","SYR","TCD","TGO","THA","TJK","TKM","TLS","TON","TTO","TUN","TUR","TZA","UGA","UKR","URY","USA","UZB","VCT","VEN","VIR","VNM","VUT","PSE","WSM","YEM","ZAF","COD","ZMB","ZWE")
Region_dataset_p2 <- c("The Americas","Asia","Africa","Europe","Middle East","The Americas","Asia","The Americas","Oceania","Europe","Asia","Africa","Europe","Africa","Africa","Asia","Europe","Middle East","The Americas","Europe","Europe","The Americas","The Americas","The Americas","The Americas","The Americas","Asia","Asia","Africa","Africa","The Americas","Europe","The Americas","Asia","Africa","Africa","Africa","The Americas","Africa","Africa","The Americas","The Americas","The Americas","Europe","Europe","Europe","Africa","Europe","The Americas","Africa","The Americas","Africa","Africa","Europe","Europe","Africa","Europe","Oceania","Europe","Oceania","Africa","Europe","Asia","Africa","Africa","Africa","Africa","Africa","Europe","The Americas","The Americas","The Americas","Oceania","The Americas","Asia","The Americas","Europe","The Americas","Europe","Asia","Asia","Europe","Middle East","Middle East","Europe","Middle East","Europe","The Americas","Middle East","Asia","Asia","Africa","Asia","Asia","Oceania","Asia","Middle East","Asia","Middle East","Africa","Africa","The Americas","Europe","Asia","Africa","Europe","Europe","Europe","Asia","Africa","Europe","Africa","Asia","The Americas","Europe","Africa","Europe","Asia","Europe","Asia","Africa","Africa","Africa","Africa","Asia","Africa","Oceania","Africa","Africa","The Americas","Europe","Europe","Asia","Oceania","Middle East","Asia","The Americas","The Americas","Asia","Oceania","Europe","The Americas","Europe","The Americas","Oceania","Middle East","Europe","Europe","Africa","Middle East","Africa","Africa","Asia","Oceania","Africa","The Americas","Africa","Europe","Africa","Africa","The Americas","Europe","Europe","Europe","Africa","Africa","Middle East","Africa","Africa","Asia","Asia","Asia","Asia","Oceania","The Americas","Africa","Europe","Africa","Africa","Europe","The Americas","The Americas","Asia","The Americas","The Americas","The Americas","Asia","Oceania","Middle East","Oceania","Middle East","Africa","Africa","Africa","Africa")
mi_dataframe<-data.frame(Pais_dataset_p2,Codigo_Pais_dataset_p2,Region_dataset_p2)
head(mi_dataframe,n=10)
#cambiamos los nombres
mi_dataframe<-data.frame(Pais=Pais_dataset_p2,Codigo=Codigo_Pais_dataset_p2,Region=Region_dataset_p2)
mi_dataframe
tail(mi_dataframe)
#combinar dos dataframes para graficarlos (merge)
#tenemos dos dataframes: datos y mi_dataframe que tienen en comun los codigos
#abrimos un nuevo objeto
dataframe_combinado<-merge(datos,mi_dataframe,by.x = "Codigo.Pais",by.y = "Codigo")
head(dataframe_combinado,n=10)#Aparece dos veces el país asi que lo borramos
dataframe_combinado$Pais<-NULL
head(dataframe_combinado,n=10)
#grafiquemos
qplot(data=dataframe_combinado, x=Penetracion.Internet, y=Tasa.Natalidad, color=Region,size=I(5))
qplot(data=dataframe_combinado, x=Penetracion.Internet, y=Tasa.Natalidad, color=Region,size=I(5),shape=I(17))#con triangulos
#puedes buscar en google las figuras eb shapes Qplot.png
qplot(data=dataframe_combinado, x=Penetracion.Internet, y=Tasa.Natalidad, color=Region,size=I(5),shape=I(17),alpha=I(0.5))#para ver empalmes
#agregar un titulo
qplot(data=dataframe_combinado, x=Penetracion.Internet, y=Tasa.Natalidad, color=Region,size=I(5),shape=I(17),
      main="Tasa de Natalidad vs Penetración de Internet")
#practica
