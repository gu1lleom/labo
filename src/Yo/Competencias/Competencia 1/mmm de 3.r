#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")


# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Repos\\Maestria\\DM_EyF\\")
# Poner sus semillas
semillas <- c(102433 , 525299, 712561, 984427, 363373)

dataset <- fread("./datasets/cluster_de_bajas_3.txt")

#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
dataset[  , mean(mcuentas_saldo),  cluster2 ]
dataset[  , mean(chomebanking_transacciones),  cluster2 ]
cliente_edad 
cliente_antiguedad 
mcaja_ahorro


head(dataset)
sumary(dataset)

dataset[  , mean(cliente_edad),  cluster2 ]
dataset[  , mean(cliente_antiguedad),  cluster2 ]
dataset[  , mean(mcaja_ahorro),  cluster2 ]


##----------------------------------------------------------------------------------------------------------------------

dataset[  , mean(numero_de_cliente),  cluster2 ]
dataset[  , mean(foto_mes),  cluster2 ]
dataset[  , mean(active_quarter),  cluster2 ]
dataset[  , mean(cliente_vip),  cluster2 ]
dataset[  , mean(internet),  cluster2 ]
dataset[  , mean(cliente_edad),  cluster2 ]
dataset[  , mean(cliente_antiguedad),  cluster2 ]
dataset[  , mean(mrentabilidad),  cluster2 ]
dataset[  , mean(mrentabilidad_annual),  cluster2 ]
dataset[  , mean(mcomisiones),  cluster2 ]
dataset[  , mean(mactivos_margen),  cluster2 ]
dataset[  , mean(mpasivos_margen),  cluster2 ]
dataset[  , mean(cproductos),  cluster2 ]
dataset[  , mean(tcuentas),  cluster2 ]
dataset[  , mean(ccuenta_corriente),  cluster2 ]
dataset[  , mean(mcuenta_corriente_adicional),  cluster2 ]
dataset[  , mean(mcuenta_corriente),  cluster2 ]
dataset[  , mean(ccaja_ahorro),  cluster2 ]
dataset[  , mean(mcaja_ahorro),  cluster2 ]
dataset[  , mean(mcaja_ahorro_adicional),  cluster2 ]
dataset[  , mean(mcaja_ahorro_dolares),  cluster2 ]
dataset[  , mean(cdescubierto_preacordado),  cluster2 ]
dataset[  , mean(mcuentas_saldo),  cluster2 ]
dataset[  , mean(ctarjeta_debito),  cluster2 ]
dataset[  , mean(ctarjeta_debito_transacciones),  cluster2 ]
dataset[  , mean(mautoservicio),  cluster2 ]
dataset[  , mean(ctarjeta_visa),  cluster2 ]
dataset[  , mean(ctarjeta_visa_transacciones),  cluster2 ]
dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
dataset[  , mean(ctarjeta_master),  cluster2 ]
dataset[  , mean(ctarjeta_master_transacciones),  cluster2 ]
dataset[  , mean(mtarjeta_master_consumo),  cluster2 ]
dataset[  , mean(cprestamos_personales),  cluster2 ]
dataset[  , mean(mprestamos_personales),  cluster2 ]
dataset[  , mean(cprestamos_prendarios),  cluster2 ]
dataset[  , mean(mprestamos_prendarios),  cluster2 ]
dataset[  , mean(cprestamos_hipotecarios),  cluster2 ]
dataset[  , mean(mprestamos_hipotecarios),  cluster2 ]
dataset[  , mean(cplazo_fijo),  cluster2 ]
dataset[  , mean(mplazo_fijo_dolares),  cluster2 ]
dataset[  , mean(mplazo_fijo_pesos),  cluster2 ]
dataset[  , mean(cinversion1),  cluster2 ]
dataset[  , mean(minversion1_pesos),  cluster2 ]
dataset[  , mean(minversion1_dolares),  cluster2 ]
dataset[  , mean(cinversion2),  cluster2 ]
dataset[  , mean(minversion2),  cluster2 ]
dataset[  , mean(cseguro_vida),  cluster2 ]
dataset[  , mean(cseguro_auto),  cluster2 ]
dataset[  , mean(cseguro_vivienda),  cluster2 ]
dataset[  , mean(cseguro_accidentes_personales),  cluster2 ]
dataset[  , mean(ccaja_seguridad),  cluster2 ]
dataset[  , mean(cpayroll_trx),  cluster2 ]
dataset[  , mean(mpayroll),  cluster2 ]
dataset[  , mean(mpayroll2),  cluster2 ]
dataset[  , mean(cpayroll2_trx),  cluster2 ]
dataset[  , mean(ccuenta_debitos_automaticos),  cluster2 ]
dataset[  , mean(mcuenta_debitos_automaticos),  cluster2 ]
dataset[  , mean(ctarjeta_visa_debitos_automaticos),  cluster2 ]
dataset[  , mean(mttarjeta_visa_debitos_automaticos),  cluster2 ]
dataset[  , mean(ctarjeta_master_debitos_automaticos),  cluster2 ]
dataset[  , mean(mttarjeta_master_debitos_automaticos),  cluster2 ]
dataset[  , mean(cpagodeservicios),  cluster2 ]
dataset[  , mean(mpagodeservicios),  cluster2 ]
dataset[  , mean(cpagomiscuentas),  cluster2 ]
dataset[  , mean(mpagomiscuentas),  cluster2 ]
dataset[  , mean(ccajeros_propios_descuentos),  cluster2 ]
dataset[  , mean(mcajeros_propios_descuentos),  cluster2 ]
dataset[  , mean(ctarjeta_visa_descuentos),  cluster2 ]
dataset[  , mean(mtarjeta_visa_descuentos),  cluster2 ]
dataset[  , mean(ctarjeta_master_descuentos),  cluster2 ]
dataset[  , mean(mtarjeta_master_descuentos),  cluster2 ]
dataset[  , mean(ccomisiones_mantenimiento),  cluster2 ]
dataset[  , mean(mcomisiones_mantenimiento),  cluster2 ]
dataset[  , mean(ccomisiones_otras),  cluster2 ]
dataset[  , mean(mcomisiones_otras),  cluster2 ]
dataset[  , mean(cforex),  cluster2 ]
dataset[  , mean(cforex_buy),  cluster2 ]
dataset[  , mean(mforex_buy),  cluster2 ]
dataset[  , mean(cforex_sell),  cluster2 ]
dataset[  , mean(mforex_sell),  cluster2 ]
dataset[  , mean(ctransferencias_recibidas),  cluster2 ]
dataset[  , mean(mtransferencias_recibidas),  cluster2 ]
dataset[  , mean(ctransferencias_emitidas),  cluster2 ]
dataset[  , mean(mtransferencias_emitidas),  cluster2 ]
dataset[  , mean(cextraccion_autoservicio),  cluster2 ]
dataset[  , mean(mextraccion_autoservicio),  cluster2 ]
dataset[  , mean(ccheques_depositados),  cluster2 ]
dataset[  , mean(mcheques_depositados),  cluster2 ]
dataset[  , mean(ccheques_emitidos),  cluster2 ]
dataset[  , mean(mcheques_emitidos),  cluster2 ]
dataset[  , mean(ccheques_depositados_rechazados),  cluster2 ]
dataset[  , mean(mcheques_depositados_rechazados),  cluster2 ]
dataset[  , mean(ccheques_emitidos_rechazados),  cluster2 ]
dataset[  , mean(mcheques_emitidos_rechazados),  cluster2 ]
dataset[  , mean(tcallcenter),  cluster2 ]
dataset[  , mean(ccallcenter_transacciones),  cluster2 ]
dataset[  , mean(thomebanking),  cluster2 ]
dataset[  , mean(chomebanking_transacciones),  cluster2 ]
dataset[  , mean(ccajas_transacciones),  cluster2 ]
dataset[  , mean(ccajas_consultas),  cluster2 ]
dataset[  , mean(ccajas_depositos),  cluster2 ]
dataset[  , mean(ccajas_extracciones),  cluster2 ]
dataset[  , mean(ccajas_otras),  cluster2 ]
dataset[  , mean(catm_trx),  cluster2 ]
dataset[  , mean(matm),  cluster2 ]
dataset[  , mean(catm_trx_other),  cluster2 ]
dataset[  , mean(matm_other),  cluster2 ]
dataset[  , mean(ctrx_quarter),  cluster2 ]
dataset[  , mean(tmobile_app),  cluster2 ]
dataset[  , mean(cmobile_app_trx),  cluster2 ]
dataset[  , mean(Master_delinquency),  cluster2 ]
dataset[  , mean(Master_status),  cluster2 ]
dataset[  , mean(Master_mfinanciacion_limite),  cluster2 ]
dataset[  , mean(Master_Fvencimiento),  cluster2 ]
dataset[  , mean(Master_Finiciomora),  cluster2 ]
dataset[  , mean(Master_msaldototal),  cluster2 ]
dataset[  , mean(Master_msaldopesos),  cluster2 ]
dataset[  , mean(Master_msaldodolares),  cluster2 ]
dataset[  , mean(Master_mconsumospesos),  cluster2 ]
dataset[  , mean(Master_mconsumosdolares),  cluster2 ]
dataset[  , mean(Master_mlimitecompra),  cluster2 ]
dataset[  , mean(Master_madelantopesos),  cluster2 ]
dataset[  , mean(Master_madelantodolares),  cluster2 ]
dataset[  , mean(Master_fultimo_cierre),  cluster2 ]
dataset[  , mean(Master_mpagado),  cluster2 ]
dataset[  , mean(Master_mpagospesos),  cluster2 ]
dataset[  , mean(Master_mpagosdolares),  cluster2 ]
dataset[  , mean(Master_fechaalta),  cluster2 ]
dataset[  , mean(Master_mconsumototal),  cluster2 ]
dataset[  , mean(Master_cconsumos),  cluster2 ]
dataset[  , mean(Master_cadelantosefectivo),  cluster2 ]
dataset[  , mean(Master_mpagominimo),  cluster2 ]
dataset[  , mean(Visa_delinquency),  cluster2 ]
dataset[  , mean(Visa_status),  cluster2 ]
dataset[  , mean(Visa_mfinanciacion_limite),  cluster2 ]
dataset[  , mean(Visa_Fvencimiento),  cluster2 ]
dataset[  , mean(Visa_Finiciomora),  cluster2 ]
dataset[  , mean(Visa_msaldototal),  cluster2 ]
dataset[  , mean(Visa_msaldopesos),  cluster2 ]
dataset[  , mean(Visa_msaldodolares),  cluster2 ]
dataset[  , mean(Visa_mconsumospesos),  cluster2 ]
dataset[  , mean(Visa_mconsumosdolares),  cluster2 ]
dataset[  , mean(Visa_mlimitecompra),  cluster2 ]
dataset[  , mean(Visa_madelantopesos),  cluster2 ]
dataset[  , mean(Visa_madelantodolares),  cluster2 ]
dataset[  , mean(Visa_fultimo_cierre),  cluster2 ]
dataset[  , mean(Visa_mpagado),  cluster2 ]
dataset[  , mean(Visa_mpagospesos),  cluster2 ]
dataset[  , mean(Visa_mpagosdolares),  cluster2 ]
dataset[  , mean(Visa_fechaalta),  cluster2 ]
dataset[  , mean(Visa_mconsumototal),  cluster2 ]
dataset[  , mean(Visa_cconsumos),  cluster2 ]
dataset[  , mean(Visa_cadelantosefectivo),  cluster2 ]
dataset[  , mean(Visa_mpagominimo),  cluster2 ]


##----------------------------------------------------------------------------------------------------------------------

dataset[  , mean(ctarjeta_debito_transacciones+ctarjeta_visa_transacciones+ctarjeta_master_transacciones+ccallcenter_transacciones+chomebanking_transacciones+ccajas_transacciones),  cluster2 ]






dataset.corr()
chart.Correlation(dataset)

transacciones <- select(dataset,ctarjeta_debito_transacciones,
       ctarjeta_visa_transacciones,
       ctarjeta_master_transacciones,
       ccallcenter_transacciones,
       chomebanking_transacciones,
       ccajas_transacciones,cluster2)


fwrite( transacciones, 
        file= "transacciones.txt",
        sep= "\t" )


#Finalmente grabo el archivo para  Juan Pablo Cadaveira
#agrego a dataset12 el cluster2  y lo grabo

dataset12[ dataset,
           on= "numero_de_cliente",
           cluster2 := i.cluster2 ]

fwrite( dataset12, 
        file= "cluster_de_bajas_12meses.txt",
        sep= "\t" )







install.packages("corrr")

# Crear Matriz de correlaciones y veo las variables que mas correlacionan con el CLUSTER:
corr = dataset.corr()

# Elijo solamente las correlaciones contra los cluster y lar ordeno por las de mayor valor de correlacion:

#correl absoluta
abs = pd.Series(corr['cluster2']).abs().rename('abs_b12')

#selecciono y ordeno
corr_b12 = pd.concat([abs, corr['cluster2']], axis = 1)
corr_b12 = corr_b12.sort_values(by = 'abs_b12', ascending = False)
corr_b12[0:10]







data_corr <- dataset


dataset_cor <- dataset %>% 
  select(data_corr,ctarjeta_debito_transacciones,
         ctarjeta_visa_transacciones,
         ctarjeta_master_transacciones,
         ccallcenter_transacciones,
         chomebanking_transacciones,
         ccajas_transacciones,cluster2) %>% 
  correlate()

