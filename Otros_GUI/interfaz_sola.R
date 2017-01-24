
#####################################################################################
#####################################################################################
#####--------------------------CUERPO INTERFAZ GRÁFICA------------------------#######
#####################################################################################
#####################################################################################

#source("version_nueva.R")

win <- gwindow("RClimTool 1.0", visible=F ,width = 600) #Crea ventana inicial
nb = gnotebook(cont=win,expand=T,tab.pos = 2)


#---------------------------------------------------------------------
###Lectura y validacion de datos ###
#---------------------------------------------------------------------

lyt=glayout(homogeneous =T,cont=nb,spacing=1,label="1. Lectura de datos",expand=T) 

lyt[1,1:3]=(g=gframe("Lectura de datos",container=lyt,horizontal = T))

lytgb=glayout(homogeneous =T,cont=g,spacing=1,expand=T) 


lytgb[1,1]=(h=gbutton("Cambiar directorio",container=lytgb,handler = function(h,...)setwd(gfile(text="Seleccionar directorio",type="selectdir"))))

lytgb[2,1]=(glabel=(""))
lytgb[3,1]=(glabel=("Cargar datos:"))

lytgb[4,1]=gbutton("Temp. Máxima",container=lytgb, expand=F, 
                   handler =function(h,...) {cargar_tmax()})


lytgb[5,1]=gbutton("Temp. Mínima",container=lytgb, expand=F, 
                   handler =function(h,...) {cargar_tmin()})

# lytgb[5,1]=gbutton("Temp. Media",container=lytgb, expand=F, 
#                    handler =function(h,...) {cargar_tmean()})


lytgb[6,1]=gbutton("Precipitación",container=lytgb, expand=F, 
                   handler =function(h,...) {cargar_precip()})


#---------------------------------------------------------------------
####Analisis descriptivo#####
#---------------------------------------------------------------------
lyt1=glayout(homogeneous =F,cont=nb,spacing=1,label="2. Análisis gráfico y descriptivo",expand=T) 


lyt1[1,1:6]=(g1=gframe("Análisis descripivo",container=lyt,expand=T,horizontal=F))
lytg2=glayout(homogeneous =F,cont=g1,spacing=1,expand=T) 


lytg2[1,1]=glabel("-Período de análisis",container=lytg2)
lytg2[2,1]=glabel("Desde",container=lytg2)
lytg2[2,2]=(mesd=gedit("",cont=lytg2,expand=F,width =7,initial.msg="M"))
lytg2[2,3]=(añod=gedit("",cont=lytg2,expand=F,width =10,initial.msg="AAAA"))

lytg2[3,1]=glabel("Hasta",container=lytg2)
lytg2[3,2]=(mesd1=gedit("",cont=lytg2,expand=F,width =7,initial.msg="M"))
lytg2[3,3]=(añod1=gedit("",cont=lytg2,expand=F,width =10,initial.msg="AAAA"))

lytg2[4,1]=glabel("",container=lytg2)

lytg2[5,1]=glabel("-Variable a analizar  ",container=lytg2)
lytg2[5,2]=(nom_val1=gdroplist(c("tmax","tmin","precip"),selected = 0,cont=lytg2,expand=T,handler=function(h,...){attach(eval(parse(text=svalue(h$obj))))}))
lytg2[5,3]=gbutton("Descriptivas",container=lytg2,handler=function(h,...){print(descript2(eval(parse(text=svalue(nom_val1)))))})

lyt1[2,1:6]=glabel("")


#---------------------------------------------------------------------
###Analisis grafico###
#---------------------------------------------------------------------
lyt1[3,1]=(g.2=gframe("Gráficos autómaticos: ",container=lyt1 ,horizontal=F,expand=T))
lytg.1=glayout(homogeneous =F,cont=g.2,spacing=1,expand=T) 

lytg.1[1,1]=glabel("Tipo de análisis")
lytg.1[1,2]=(tipo=gdroplist(c("Diaria","Mensual"),selected=1,cont=lytg.1))

lytg.1[2,1]=glabel("",cont=lytg.1)

lytg.1[3,1]=gbutton("Gráficos Plot",container=lytg.1,
                   handler = function(h,...){graf_plot() } )

lytg.1[4,1]=gbutton("Gráficos Boxplot",container=lytg.1,
                    handler = function(h,...){graf_box() } )


lytg.1[5,1]=gbutton("Gráficos de dispersión",container=lytg.1,
                    handler = function(h,...){graf_disp() } )

lyt1[5,1:6]=glabel("")


lyt1[6,1]=(g2=gframe("Gráficos Personalizados: ",container=lyt1 ,horizontal=F,expand=T))
lytg1=glayout(homogeneous =F,cont=g2,spacing=1,expand=T) 

lytg1[1,1]=glabel("Clásicos",container=lytg1)
lytg1[2,1]=gbutton("Gráfico Plot",container=lytg1,
                   handler = function(h,...){plot2() } )

lytg1[3,1]=gbutton("Histograma",container=lytg1,
                   handler = function(h,...){hist2()} )

lytg1[4,1]=gbutton("Gráfico Boxplot",container=lytg1,
                   handler = function(h,...){boxplot2()} )

lytg1[1,2]=glabel("      ",container=lytg1)
lytg1[1,3]=glabel("P. ggplot2",container=lytg1)
lytg1[2,3]=gbutton("Gráfico Plot",container=lytg1,
                   handler = function(h,...){qplot2()} )

lytg1[3,3]=gbutton("Histograma",container=lytg1,
                   handler = function(h,...){hist_22()} )




#---------------------------------------------------------------------
##Seccion control de calidad##
#---------------------------------------------------------------------

lyt4=glayout(homogeneous =T,cont=nb,spacing=2,label="3. Control de calidad ",expand=T) 

lyt4[1,1]=(gg=gframe("Validación",cont=lyt4,horizontal=F))
lytg=glayout(homogeneous =F,cont=gg,spacing=1,expand=T)

lytg[1,1]=glabel("",cont=lytg)

lytg[2,1]=glabel("Variable a validar",container=lytg)
lytg[2,2]=(variable<-gdroplist(c("tmax","tmin","precip"),selected=0,cont=lytg,expand=T))

lytg[3,1]=glabel("No. de desviaciones estándar: ",container=lytg)
lytg[3,2]=(criterio=gedit("3",container=lytg,width = 5,initial.msg="Desv.Est."))

lytg[4,1]=glabel("Rango de la variable:",container=lytg)
lytg[4,2]=(minim=gedit("",container=lytg,width = 7,initial.msg="Mín."))
lytg[4,3]=(maxim=gedit("",container=lytg,width = 10,initial.msg="Máx."))

lytg[5,1]=glabel("",cont=lytg)
lytg[6,2]=gbutton("Validar",container=lytg,handler=function(h,...){print(validar2(eval(parse(text=svalue(variable)))))})
lytg[6,3]=gbutton("Gráficos",container=lytg,handler=function(h,...){grafcontrol(eval(parse(text=svalue(variable))))})

lyt4[2,1]=(gg.3=gframe("Pre-informe",cont=lyt4,horizontal=F))
lytgg.3=glayout(homogeneous =F,cont=gg.3,spacing=1,expand=T)

lytgg.3[1,1]=glabel("Nombre del archivo: ",container=lytgg.3)
lytgg.3[1,2]=(nom_arch1 <-gedit("preinforme.doc",container=lytgg.3,coerce.with=as.character)) #Genera un label editable para el nombre del archivo del informe

lytgg.3[2,2]=gbutton("Generar pre-informe",cont=lytgg.3,handler=function(h,...)inf2())

#---------------------------------------------------------------------
##Seccion datos faltantes##
#---------------------------------------------------------------------

lyt3=glayout(homogeneous =F,cont=nb,spacing=1,label="4. Datos faltantes",expand=T) 

lyt3[1,1]=glabel("Año inicial: ",container=lyt3)
lyt3[1,2]=(year_min <-gedit("",container=lyt3,width = 10,initial.msg="AAAA")) #Genera un label editable para el nombre del archivo del informe

lyt3[2,1]=glabel("Año final: ",container=lyt3)
lyt3[2,2]=(year_max <-gedit("",container=lyt3,width = 10,initial.msg="AAAA")) #Genera un label editable para el nombre del archivo del informe


lyt3[3,1]=glabel("Fecha inicial",container=lyt3,coerce.with=as.character)
lyt3[3,2]=(origin <-gedit("",initial.msg="AAAA",container=lyt3,width = 10)) 
lyt3[3,3]=(origin1 <-gedit("",initial.msg="M",container=lyt3,width = 10)) 
lyt3[3,4]=(origin2 <-gedit("",initial.msg="D",container=lyt3,width = 10)) 

lyt3[7,1]=gbutton("Completar datos",container=lyt3,handler = function(h,...){datos_falt()},expand=T)

lyt3[8,1:4]=gtext("Nota: Para llevar a cabo esta función es indispensable tener los datos de temperatura mínima, máxima y precipitación",container=lyt3,coerce.with=as.character)

#---------------------------------------------------------------------
##Seccion homogeneidad de series##
#---------------------------------------------------------------------
lyt5=glayout(homogeneous =F,cont=nb,spacing=2,label="5. Análisis de homogeneidad",expand=T) 

lyt5[1,1]=(gg.1=gframe("Definición de parámetros:",cont=lyt5,horizontal=F))
lyt55=glayout(homogeneous =F,cont=gg.1,spacing=2,expand=T) 

lyt55[1,1]=glabel("-Variable a analizar",container=lyt55)

lyt55[1,2]=(nom_val2=gdroplist(c("tmax","tmin", "precip"),selected=0,cont=lyt55,expand=T))

lyt55[2,1]=glabel("-Nivel de significancia",container=lyt55)

lyt55[2,2]=(obj <- gspinbutton(from=0, to = 0.3, by =0.01, value=0.05,
                              container=lyt55)) #Idea para seleccionar nivel de confianza


lyt55[3,1]=glabel("-Período de análisis",container=lyt55)
lyt55[4,1]=glabel("Desde",container=lyt55)
lyt55[4,2]=(mesh=gedit("",cont=lyt55,expand=F,width =7,initial.msg="M"))
lyt55[4,3]=(añoh=gedit("",cont=lyt55,expand=F,width =10,initial.msg="AAAA"))

lyt55[5,1]=glabel("Hasta",container=lyt55)
lyt55[5,2]=(mesh1=gedit("",cont=lyt55,expand=F,width =7,initial.msg="M"))
lyt55[5,3]=(añoh1=gedit("",cont=lyt55,expand=F,width =10,initial.msg="AAAA"))


lyt5[2,1]=glabel("")
lyt5[3,1]=(gg2=gframe("Test para Normalidad:",cont=lyt5,horizontal=F))
norm_test1=gcheckbox("Shapiro-Wilk",cont=gg2,handler=function(h,...){print(shap(eval(parse(text=svalue(nom_val2)))))})
norm_test2=gcheckbox("Kolmogorov-Smirnov",cont=gg2,handler=function(h,...){print(KS_test(eval(parse(text=svalue(nom_val2)))))})
norm_test3=gcheckbox("Jarque-Bera ",cont=gg2,handler=function(h,...){print(JB2(eval(parse(text=svalue(nom_val2)))))})

gbutton("Gráficos QQ-norm",cont=gg2,handler=function(h,...){graf_norm()},width=5)

lyt5[4,1]=glabel("")
lyt5[5,1]=(gg3=gframe("Test para Tendencia:",cont=lyt5,horizontal=F))
tend_test=gcheckbox("Rango correlación de Spearman",cont=gg3,handler=function(h,...){print(Rsp_Test(eval(parse(text=svalue(nom_val2)))))})
tend_test1=gcheckbox("Mann-Kendall",cont=gg3,handler=function(h,...){print(Ken_T(eval(parse(text=svalue(nom_val2)))))})

lyt5[6,1]=glabel("")
lyt5[7,1]=(gg4=gframe("Test para Estabilidad en Varianza:",cont=lyt5,horizontal=F))
estv_test=gcheckbox("Test-F",cont=gg4,handler=function(h,...){print(F_test.indic(eval(parse(text=svalue(nom_val2)))))})

lyt5[8,1]=glabel("")
lyt5[9,1]=(gg5=gframe("Test para Estabilidad en Media:",cont=lyt5,horizontal=F))
estm_test=gcheckbox("Test-T",cont=gg5,handler=function(h,...){print(T_Test(eval(parse(text=svalue(nom_val2)))))})

estm_test1=gcheckbox("Test U Mann - Whitney",cont=gg5,handler=function(h,...){print(Umann_Test(eval(parse(text=svalue(nom_val2)))))})

lyt5[1,2]=glabel("   ")
lyt5[1,3]=(gg.2=gframe("Informe",cont=lyt5,horizontal=F))
lyt.1=glayout(homogeneous =F,cont=gg.2,spacing=2,expand=T) 


lyt.1[1,1]=glabel("Nombre del archivo: ",container=lyt.1)
lyt.1[1,2]=(nom_arch <-gedit("informe.doc",container=lyt.1,coerce.with=as.character,width = 15)) #Genera un label editable para el nombre del archivo del informe
# lyt.1[2,1]=glabel("Seleccione la variable:",container=lyt.1)
# lyt.1[2,2]=(nom_val=gdroplist(c("tmax","tmin", "precip"),selected=0,cont=lyt.1,expand=T))

lyt.1[2,2]=gbutton("Generar",container=lyt.1,handler = function(h,...){inf()},expand=T)

#---------------------------------------------------------------------
##Seccion Indicadores##
#---------------------------------------------------------------------

lyt2=glayout(homogeneous =F,cont=nb,spacing=2,label="6. Cálculo de indicadores ",expand=T) 


 lyt2[1,1]=(gg.3=gframe("Indicadores:",cont=lyt2,horizontal=F))
 lyt.2=glayout(homogeneous =F,cont=gg.3,spacing=2,expand=T) 

lyt.2[1,1]=glabel("-Período de análisis",container=lyt55)
lyt.2[2,1]=glabel("Desde",container=lyt.2)
lyt.2[2,2]=(mesi=gedit("",cont=lyt.2,expand=F,width =7,initial.msg="M"))
lyt.2[2,3]=(añoi=gedit("",cont=lyt.2,expand=F,width =10,initial.msg="AAAA"))

lyt.2[3,1]=glabel("Hasta",container=lyt.2)
lyt.2[3,2]=(mesi1=gedit("",cont=lyt.2,expand=F,width =7,initial.msg="M"))
lyt.2[3,3]=(añoi1=gedit("",cont=lyt.2,expand=F,width =10,initial.msg="AAAA"))

lyt.2[4,1]=glabel("",container=lyt.2)

lyt.2[5,1]=glabel("-Variable a analizar",container=lyt55)

lyt.2[5,2]=(nom_val3=gdroplist(c("tmax","tmin", "precip"),selected=0,cont=lyt.2,expand=F))
lyt.2[5,3]=glabel("     ",container=lyt55)

 lyt.2[6,1]=glabel("-Indicadores anuales:",container=lyt.2)
 
 lyt.2[7,1]=gcheckbox("No. días mayores que",container=lyt.2,handler = function(h,...){print(diasmayor(as.numeric(svalue(valor1))))})
lyt.2[7,2]=(valor1=gedit("",container=lyt.2,width = 1))

 lyt.2[8,1]=gcheckbox("No. días menores que",container=lyt.2,handler = function(h,...){print(diasmenor(as.numeric(svalue(valor2))))})
lyt.2[8,2]=(valor2=gedit("",container=lyt.2,width = 1))


lyt.2[9,1]=glabel("",container=lyt.2)

lyt.2[10,1]=glabel("-Indicadores mensuales:",container=lyt.2)


  lyt.2[11,1]=gcheckbox("Máximos",container=lyt.2,handler = function(h,...){print(tempmax())})
  lyt.2[12,1]=gcheckbox("Mínimos",container=lyt.2,handler = function(h,...){print(tempmin())})
 
#---------------------------------------------------------------------
##Seccion Condición ENSO##
#---------------------------------------------------------------------


lyt22=glayout(homogeneous =F,cont=nb,spacing=2,label="7. Condición ENSO ",expand=T) 

lyt22[1,1]=(gg.4=gframe("Condición ENSO",cont=lyt22,horizontal=F))
lyt.3=glayout(homogeneous =T,cont=gg.4,spacing=3,expand=T) 

lyt.3[1,1]=glabel("-Mensual",container=lyt.3)
lyt.3[2,1]=glabel("Desde:",container=lyt.3)
lyt.3[2,2]=(mes=gdroplist(c("Mes",1:12),selected=1,cont=lyt.1,expand=T))
lyt.3[2,3:4]=(año=gdroplist(c("Año",1950:2013),selected=1,cont=lyt.1,expand=T))
 
 lyt.3[3,1]=glabel("Hasta:",container=lyt.3)
 lyt.3[3,2]=(mes1=gdroplist(c("Mes",1:12),selected=1,cont=lyt.1,expand=T))
 lyt.3[3,3:4]=(año1=gdroplist(c("Año",1950:2013),selected=1,cont=lyt.1,expand=T))
 lyt.3[4,2:3]=gbutton("Consulta Mensual",container=lyt.3,handler = function(h,...){print(enso())},expand=T)

#lyt.3[5,1]=glabel(" ",container=lyt.3)
lyt.3[6,1]=glabel("-Trimestral",container=lyt.3)
lyt.3[7,1]=glabel("Desde:",container=lyt.3)
lyt.3[7,2]=(trim=gdroplist(c("Trim",1:4),selected=1,cont=lyt.1,expand=T))
lyt.3[7,3:4]=(año2=gdroplist(c("Año",1950:2013),selected=1,cont=lyt.1,expand=T))

lyt.3[8,1]=glabel("Hasta:",container=lyt.3)
lyt.3[8,2]=(trim1=gdroplist(c("Trim",1:4),selected=1,cont=lyt.1,expand=T))
lyt.3[8,3:4]=(año3=gdroplist(c("Año",1950:2013),selected=1,cont=lyt.1,expand=T))
lyt.3[9,2:3]=gbutton("Consulta Trimestral",container=lyt.3,handler = function(h,...){print(enso1())},expand=T)


#---------------------------------------------------------------------
##Bienvenida##
#---------------------------------------------------------------------

g5=ggroup(container=nb,horizontal = F,label="Bienvenid@",cont=nb)
gimage("ciat4.png",dirname="C:/Users/lllanos/Dropbox/Trabajo_CIAT/Logos", size="menu", container=g5,label="Bienvenid@",width=700,heigth=700) #Inserta imagen "ciat.png"en la ventana
#gimage("ciat3.png",dirname="D:/Dropbox/Trabajo CIAT/Logos", size="dialog", container=g5,label="Bienvenid@") #Inserta imagen "ciat.png"en la ventana

mensj=gtext("Bienvenid@ ",cont=g5)
insert(mensj, "a RClimTool, la aplicación diseñada para el análisis de series climatológicas (Temperatura Mínima, Temperatura Máxima, Temperatura Media y Precipitación)")

visible(win) = T
focus(win)
