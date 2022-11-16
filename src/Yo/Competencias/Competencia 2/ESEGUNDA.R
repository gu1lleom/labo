# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


#setwd("C:\\Yo\\DM\\EyF\\")   #Establezco el Working Directory




#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "KA7240"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$finalmodel$max_bin           <- 31
PARAM$finalmodel$learning_rate     <- 0.101868939970205 #0.0153044989171801
PARAM$finalmodel$num_iterations    <- 143 #609
PARAM$finalmodel$num_leaves        <- 17 #655
PARAM$finalmodel$min_data_in_leaf  <- 1950 #96
PARAM$finalmodel$feature_fraction  <- 0.520233419129183 #0.924080951332845
PARAM$finalmodel$semilla           <- 363373

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
#setwd( "~/buckets/b1" )
setwd("C:\\Yo\\DM\\EyF\\")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)



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



dataset <- dataset[, -c("mcuentas_saldo",
                        "mcaja_ahorro",
                        "mcomisiones_mantenimiento",
                        "mcomisiones",
                        "mpayroll",
                        "Visa_Fvencimiento",
                        "mttarjeta_master_debitos_automaticos", 
                        "Visa_mpagado",
                        "Master_mpagado", 
                        "mtransferencias_recibidas",
                        "Master_mpagosdolares",
                        "Visa_msaldodolares",
                        "Visa_msaldodolares",
                        "mtransferencias_emitidas",
                        "mpagomiscuentas",
                        "mplazo_fijo_dolares",
                        "mcajeros_propios_descuentos",
                        "cinversion2",
                        "ccuenta_corriente",
                        "Master_msaldototal",
                        "mcuenta_corriente_adicional", 
                        "cprestamos_hipotecarios",
                        "cplazo_fijo", 
                        "mplazo_fijo_pesos",
                        "mplazo_fijo_pesos",
                        "mpayroll2",
                        "mtarjeta_master_descuentos", 
                        "mforex_buy", 
                        "Master_Finiciomora", 
                        "Master_madelantopesos", 
                        "Master_madelantodolares",
                        "Master_fultimo_cierre",
                        "Master_cadelantosefectivo", 
                        "Master_mpagominimo",
                        "Visa_Finiciomora",
                        "Visa_mconsumosdolares", 
                        "Visa_madelantopesos",
                        "Visa_madelantodolares",
                        "Visa_mpagosdolares",
                        "Visa_cadelantosefectivo"
)]

#####################################################################################################g


###########################################################################################################
#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla
                      )
)

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 7000, 10000, by=1000 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]
  
  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  PARAM$experimento, "_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------

quit( save= "no" )