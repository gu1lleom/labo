#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot



rm(list = ls())
gc(verbose = FALSE)


#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Repos\\Maestria\\DM_EyF\\")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")



#############################################################


dataset[, fbajo_lim_financiacion := ifelse((ifelse(is.na(Master_mfinanciacion_limite),0,as.numeric(Master_mfinanciacion_limite)) + 
                                              ifelse(is.na(  Visa_mfinanciacion_limite),0,as.numeric(  Visa_mfinanciacion_limite)) ) < mpayroll,1,0)]
dataset[, fpocos_productos := ifelse(cproductos > 12,1,0)]

dataset[, falto_costo_mantenimiento := ifelse((ifelse(is.na(mcomisiones_mantenimiento),0,as.numeric(mcomisiones_mantenimiento)) + 
                                                 ifelse(is.na(        mcomisiones_otras),0,as.numeric(        mcomisiones_otras)) ) < mpayroll/150,1,0)]



dataset[, fcompromisos := ifelse((ifelse(is.na(  Visa_Finiciomora),0,as.numeric(Visa_Finiciomora  )) + 
                                    ifelse(is.na(Master_Finiciomora),0,as.numeric(Master_Finiciomora)) +
                                    cseguro_accidentes_personales +
                                    cseguro_auto +
                                    cseguro_vida +
                                    cplazo_fijo +
                                    cprestamos_hipotecarios +
                                    cprestamos_personales +
                                    cprestamos_prendarios +
                                    cinversion1 +
                                    cinversion2) > 15,1,0)]


dataset[, fpoco_uso := ifelse((ctarjeta_debito_transacciones +
                               ctarjeta_visa_transacciones +
                               ctarjeta_master_transacciones +
                               cpayroll_trx +
                               cpayroll2_trx +
                               ccuenta_debitos_automaticos +
                               ctarjeta_visa_debitos_automaticos +
                               ctarjeta_master_debitos_automaticos + 
                               cpagodeservicios +
                               cpagomiscuentas +
                               cforex +
                               cforex_buy +
                               cforex_sell +
                               ctransferencias_emitidas +
                               ctransferencias_recibidas +
                               cextraccion_autoservicio +
                               ccheques_depositados +
                               ccheques_emitidos +
                               ccallcenter_transacciones +
                               chomebanking_transacciones +
                               ccajas_transacciones +
                               ccajas_depositos +
                               ccajas_extracciones +
                               ccajas_otras + 
                               catm_trx +
                               catm_trx_other +
                               ctrx_quarter +
                               cmobile_app_trx +
                               ifelse(is.na(         Master_cconsumos),0,as.numeric(         Master_cconsumos)) + 
                               ifelse(is.na(Master_cadelantosefectivo),0,as.numeric(Master_cadelantosefectivo)) +  
                               ifelse(is.na(           Visa_cconsumos),0,as.numeric(           Visa_cconsumos)) + 
                               ifelse(is.na(  Visa_cadelantosefectivo),0,as.numeric(  Visa_cadelantosefectivo)) ) < 120 , 0,1)]
  









#############################################################
  dataset <- dataset[, -c("ctrx_quarter",
                          "active_quarter",
                          "cliente_edad" ,
                          "ctarjeta_visa_descuentos",
                          "ccajas_consultas",
                          "Master_msaldopesos",
                          "Master_msaldototal",
                          "mcaja_ahorro_dolares",
                          "Master_mfinanciacion_limite",                 
                          "Master_mlimitecompra",                 
                          "mttarjeta_master_debitos_automaticos",
                          "mcaja_ahorro",
                          "mtarjeta_visa_consumo",
                          "ctarjeta_visa_transacciones",
                          "mpasivos_margen",
                          "mprestamos_personales",
                          "mttarjeta_visa_debitos_automaticos",
                          "cdescubierto_preacordado",
                          "Visa_msaldopesos",
                          "cprestamos_personales"
                          )]


dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.5,   #esto significa no limitar la complejidad de los splits
                 minsplit=  600,     #minima cantidad de registros para que se haga el split
                 minbucket= 10,     #tamaño minimo de una hoja
                 maxdepth=  8 )    #profundidad maxima del arbol


#####################################################
print(modelo$variable.importance)
#####################################################



#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/KA2001" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2001/K101_002.csv",
        sep=  "," )





###############################################################################################################
###############################################################################################################

prueba1 <- data.frame(dataset[,"cprestamos_prendarios"], dataset[,"cinversion1"])
prueba1


print(modelo$variable.importance)









names(dataset1)
dataset[,"foto_mes"]



dtrain <- dataset[foto_mes==202101]  #defino donde voy a entrenar
dapply  <- dataset1[foto_mes==202103]  #defino donde voy a aplicar el modelo

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.5,   #esto significa no limitar la complejidad de los splits
                 minsplit=  600,     #minima cantidad de registros para que se haga el split
                 minbucket= 10,     #tamaño minimo de una hoja
                 maxdepth=  8 )    #profundidad maxima del arbol

