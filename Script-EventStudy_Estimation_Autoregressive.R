

# REQUESITOS --------------------
options(scipen = 999) #Retirar notação científica

#Instalação dos pacotes necessários

if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, readxl, RVAideMemoire, car, tseries, FinTS, lmtest, sandwich, vars)




#EVENTO 1 -----------------------------------------------------------------------

#Nessa parte serão realizadas: 
#                importação da tabela;
#                Importação das variáveis de retorno dos ativos;
#                teste de estacionariedade sobre os retornos, antes do modelo
#                Escolha do AIC e Aplicação do Modelo Autorregressivo para cada ativo
#                Teste nos resíduos)
#                OBS: A amostra se refere a JANELA DE ESTIMAÇÃO do EVENTO 1


# Importando tabela e página

  tab_evento1 <- read_excel("Base_Event_Study.xlsx", sheet = "EV1")

# Conferindo página importada 
  View(tab_evento1)

#Formatando o separador das datas de (/) para (-)

  head(tab_evento1$DATA_EV1)
  tab_evento1$DATA_EV1 <- as.Date(tab_evento1$DATA_EV1, format = "%Y/%m/%d")
  head(tab_evento1$DATA_EV1)

# Criando o vetor da data da janela de estimação EV1
  DATA_EV1 <- c(tab_evento1$DATA_EV1)
  Jan_Estimação_EV1 <- tab_evento1[tab_evento1$JANELA_EV1 == "ESTIMAÇÃO", ]

#verificando a janela de estimação EV1
 min(Jan_Estimação_EV1$DATA_EV1)
 max(Jan_Estimação_EV1$DATA_EV1)
 View(Jan_Estimação_EV1)

#criando o vetor dos retornos da JANELA DE ESTIMAÇÃO

  R_SPX_EV1 <- tab_evento1$R_SPX[tab_evento1$JANELA_EV1 == "ESTIMAÇÃO"]
  R_SHY_EV1 <- tab_evento1$R_SHY[tab_evento1$JANELA_EV1 == "ESTIMAÇÃO"]
  R_BTC_EV1 <- tab_evento1$R_BTC[tab_evento1$JANELA_EV1 == "ESTIMAÇÃO"]
  R_ETH_EV1 <- tab_evento1$R_ETH[tab_evento1$JANELA_EV1 == "ESTIMAÇÃO"]
  R_OURO_EV1 <- tab_evento1$R_OURO[tab_evento1$JANELA_EV1 == "ESTIMAÇÃO"]
  R_PRATA_EV1 <- tab_evento1$R_PRATA[tab_evento1$JANELA_EV1 == "ESTIMAÇÃO"]
  R_DXY_EV1 <- tab_evento1$R_DXY[tab_evento1$JANELA_EV1 == "ESTIMAÇÃO"]

#Verificando os dados importados
 head(R_SPX_EV1)
 head(R_SHY_EV1)
 head(R_BTC_EV1)
 head(R_ETH_EV1)
 head(R_OURO_EV1)
 head(R_PRATA_EV1)
 head(R_DXY_EV1)
 
 # Verificação do pressuposto da estacionariedade nas séries dos retornos da JANELA DE ESTIMAÇÃO
 
 #Dickey-Fuller Aumentado (ADF) 
 #H0: a série possui raíz unitária, não é estacionária - p-value >5%
 #H1: a série não possui raíz unitária, é estacionária - p-value <5%
 
 #KPSS
 #H0: a série é estacionária - p-value >5%
 #H1: a série não é estacionária, possui raíz unitária - p-value <5%

 adf.test(R_SPX_EV1)
 kpss.test(R_SPX_EV1)

 adf.test(R_SHY_EV1)
 kpss.test(R_SHY_EV1)

 adf.test(R_BTC_EV1)
 kpss.test(R_BTC_EV1)

 adf.test(R_ETH_EV1)
 kpss.test(R_ETH_EV1)

 adf.test(R_OURO_EV1)
 kpss.test(R_OURO_EV1)

 adf.test(R_PRATA_EV1)
 kpss.test(R_PRATA_EV1)

 adf.test(R_DXY_EV1)
 kpss.test(R_DXY_EV1)


#MODELO AUTORREGRESSIVO PARA CADA ATIVO - EVENTO 1

# SPX

     SPX_AIC <- ar(R_SPX_EV1, method = "ols", aic = TRUE, order.max = 5)  #ATÉ 5 DEFASAGENS
     SPX_AIC$order #AIC = 4

     Dados_Mod_SPX <- data.frame(R_SPX_EV1, LAG1_SPX = dplyr::lag(R_SPX_EV1, 1), LAG2_SPX = dplyr::lag(R_SPX_EV1, 2), 
                                  LAG3_SPX = dplyr::lag(R_SPX_EV1, 3), LAG4_SPX = dplyr::lag(R_SPX_EV1, 4)) #CRIANDO VARIÁVEIS COM LAG
     View(Dados_Mod_SPX) 

     Dados_Mod_SPX_Novo <- na.omit(Dados_Mod_SPX) #AJUSTANDO A DATA COM OS LAGs
     View(Dados_Mod_SPX_Novo)

     Mod_SPX_Autoregressive <- lm(R_SPX_EV1 ~ LAG1_SPX + LAG2_SPX + LAG3_SPX + LAG4_SPX, data = Dados_Mod_SPX_Novo) #ESTIMAÇÃO DO MODELO
     summary(Mod_SPX_Autoregressive) 

     shapiro.test(Mod_SPX_Autoregressive$residuals) # Teste de Normalidade dos resíduos - R: Não há normalidade - p-value < 5%

     bptest(Mod_SPX_Autoregressive) # Teste de homocedasticidade - R: Não há homocedasticidade - p-value < 5%

     Box.test(Mod_SPX_Autoregressive$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%

     coeftest(Mod_SPX_Autoregressive, vcov. = NeweyWest(Mod_SPX_Autoregressive)) #Correção de Newey-West erros-padrão robusto
     
     
# SHY
     
     SHY_AIC <- ar(R_SHY_EV1, method = "ols", aic = TRUE, order.max = 5) #ATÉ 5 DEFASAGENS
     SHY_AIC$order #AIC = 5
     
     Dados_Mod_SHY <- data.frame(R_SHY_EV1, LAG1_SHY = dplyr::lag(R_SHY_EV1, 1), LAG2_SHY = dplyr::lag(R_SHY_EV1, 2), 
                                 LAG3_SHY = dplyr::lag(R_SHY_EV1, 3), LAG4_SHY = dplyr::lag(R_SHY_EV1, 4), LAG5_SHY = dplyr::lag(R_SHY_EV1, 5)) #CRIANDO VARIÁVEIS COM LAG
     View(Dados_Mod_SHY)
     
     Dados_Mod_SHY_Novo <- na.omit(Dados_Mod_SHY) #AJUSTANDO A DATA COM OS LAGs
     View(Dados_Mod_SHY_Novo)
     
     Mod_SHY_Autoregressive <- lm(R_SHY_EV1 ~ LAG1_SHY + LAG2_SHY + LAG3_SHY + LAG4_SHY + LAG5_SHY, data = Dados_Mod_SHY_Novo) #ESTIMAÇÃO DO MODELO
     summary(Mod_SHY_Autoregressive)
     
     shapiro.test(Mod_SHY_Autoregressive$residuals) # Teste de Normalidade dos resíduos - R: Não há normalidade - p-value < 5%
     
     bptest(Mod_SHY_Autoregressive) # Teste de homocedasticidade - R: há homocedasticidade - p-value > 5%
     
     Box.test(Mod_SHY_Autoregressive$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%
     
     
#BITCOIN
     
     BTC_AIC <- ar(R_BTC_EV1, method = "ols", aic = TRUE, order.max = 5) #ATÉ 5 DEFASAGENS
     
     BTC_AIC$order #AIC = 2
     
     Dados_Mod_BTC <- data.frame(R_BTC_EV1, LAG1_BTC = dplyr::lag(R_BTC_EV1, 1), LAG2_BTC = dplyr::lag(R_BTC_EV1, 2)) #CRIANDO VARIÁVEIS COM LAG
     View(Dados_Mod_BTC)
     
     Dados_Mod_BTC_Novo <- na.omit(Dados_Mod_BTC) #AJUSTANDO A DATA COM OS LAGs
     View(Dados_Mod_BTC_Novo)
     
     Mod_BTC_Autoregressive <- lm(R_BTC_EV1 ~ LAG1_BTC + LAG2_BTC, data = Dados_Mod_BTC_Novo) #ESTIMAÇÃO DO MODELO
     summary(Mod_BTC_Autoregressive)
     
     shapiro.test(Mod_BTC_Autoregressive$residuals) # Teste de Normalidade dos resíduos - R: Não há normalidade - p-value < 5%
     
     bptest(Mod_BTC_Autoregressive) # Teste de homocedasticidade - R: há homocedasticidade - p-value > 5%
     
     Box.test(Mod_BTC_Autoregressive$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%  
     
     
     
#ETHEREUM
     
     ETH_AIC <- ar(R_ETH_EV1, method = "ols", aic = TRUE, order.max = 5) #ATÉ 5 DEFASAGENS
     
     ETH_AIC$order #AIC = 0
     
     Mod_ETH_Autoregressive <- lm(R_ETH_EV1 ~ 1 ) #COMO O AIC FOI 0, FOI FEITO O MODELO APENAS COM A CONSTANTE
     summary(Mod_ETH_Autoregressive)
     
     shapiro.test(Mod_ETH_Autoregressive$residuals) # Teste de Normalidade dos resíduos - R: Não há normalidade - p-value < 5%
     
     Box.test(Mod_ETH_Autoregressive$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%
     
     
#OURO
     
     OURO_AIC <- ar(R_OURO_EV1, method = "ols", aic = TRUE, order.max = 5) #ATÉ 5 DEFASAGENS
     OURO_AIC$order #AIC = 4
     
     Dados_Mod_OURO <- data.frame(R_OURO_EV1, LAG1_OURO = dplyr::lag(R_OURO_EV1, 1), LAG2_OURO = dplyr::lag(R_OURO_EV1, 2), 
                                  LAG3_OURO = dplyr::lag(R_OURO_EV1, 3), LAG4_OURO = dplyr::lag(R_OURO_EV1, 4)) #CRIANDO VARIÁVEIS COM LAG
     View(Dados_Mod_OURO)
     
     Dados_Mod_OURO_Novo <- na.omit(Dados_Mod_OURO) #AJUSTANDO A DATA COM OS LAGs
     View(Dados_Mod_OURO_Novo)
     
     Mod_OURO_Autoregressive <- lm(R_OURO_EV1 ~ LAG1_OURO + LAG2_OURO + LAG3_OURO + LAG4_OURO, data = Dados_Mod_OURO_Novo) #ESTIMAÇÃO DO MODELO
     summary(Mod_OURO_Autoregressive)
     
     shapiro.test(Mod_OURO_Autoregressive$residuals) # Teste de Normalidade dos resíduos - R: Não há normalidade - p-value < 5%
     
     bptest(Mod_OURO_Autoregressive) # Teste de homocedasticidade - R: há homocedasticidade - p-value > 5%
     
     Box.test(Mod_OURO_Autoregressive$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%  
     
     
# PRATA
     
     PRATA_AIC <- ar(R_PRATA_EV1, method = "ols", aic = TRUE, order.max = 5) #ATÉ 5 DEFASAGENS
     PRATA_AIC$order #AIC = 2
     
     Dados_Mod_PRATA <- data.frame(R_PRATA_EV1, LAG1_PRATA = dplyr::lag(R_PRATA_EV1, 1), LAG2_PRATA = dplyr::lag(R_PRATA_EV1, 2)) #CRIANDO VARIÁVEIS COM LAG
     View(Dados_Mod_PRATA)
     
     Dados_Mod_PRATA_Novo <- na.omit(Dados_Mod_PRATA) #AJUSTANDO A DATA COM OS LAGs
     View(Dados_Mod_PRATA_Novo)
     
     Mod_PRATA_Autoregressive <- lm(R_PRATA_EV1 ~ LAG1_PRATA + LAG2_PRATA, data = Dados_Mod_PRATA_Novo) #ESTIMAÇÃO DO MODELO
     summary(Mod_PRATA_Autoregressive)
     
     shapiro.test(Mod_PRATA_Autoregressive$residuals) # Teste de Normalidade dos resíduos - R: Não há normalidade - p-value < 5%
     
     bptest(Mod_PRATA_Autoregressive) # Teste de homocedasticidade - R: há homocedasticidade - p-value > 5%
     
     Box.test(Mod_PRATA_Autoregressive$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%  

          
# DXY
     
     DXY_AIC <- ar(R_DXY_EV1, method = "ols", aic = TRUE, order.max = 5) #ATÉ 5 DEFASAGENS
     DXY_AIC$order #AIC = 1
     
     Dados_Mod_DXY <- data.frame(R_DXY_EV1, LAG1_DXY = dplyr::lag(R_DXY_EV1, 1)) #CRIANDO VARIÁVEIS COM LAG
     View(Dados_Mod_DXY)
     
     Dados_Mod_DXY_Novo <- na.omit(Dados_Mod_DXY) #AJUSTANDO A DATA COM OS LAGs
     View(Dados_Mod_DXY_Novo)
     
     Mod_DXY_Autoregressive <- lm(R_DXY_EV1 ~ LAG1_DXY, data = Dados_Mod_DXY_Novo) #ESTIMAÇÃO DO MODELO
     summary(Mod_DXY_Autoregressive)
     
     shapiro.test(Mod_DXY_Autoregressive$residuals) # Teste de Normalidade dos resíduos - R: há normalidade - p-value > 5%
     
     bptest(Mod_DXY_Autoregressive) # Teste de homocedasticidade - R: Não há homocedasticidade - p-value < 5%
     
     Box.test(Mod_DXY_Autoregressive$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%
     
     coeftest(Mod_DXY_Autoregressive, vcov. = NeweyWest(Mod_DXY_Autoregressive)) #Correção de Newey-West erros-padrão robusto
     

     
#EVENTO 2 -----------------------------------------------------------------------
     
#Nessa parte serão realizadas: 
#                importação da tabela;
#                Importação das variáveis de retorno dos ativos;
#                teste de estacionariedade sobre os retornos, antes do modelo
#                Escolha do AIC e Aplicação do Modelo Autorregressivo para cada ativo
#                Teste nos resíduos)
#                OBS: A amostra se refere a JANELA DE ESTIMAÇÃO do EVENTO 2;
     

  # Importando tabela e página
     
    tab_evento2 <- read_excel("Base_Event_Study.xlsx", sheet = "EV2")
     
     # Conferindo página importada 
     View(tab_evento2)
     
     #Formatando o separador das datas de (/) para (-)
     
     head(tab_evento2$DATA_EV2)
     tab_evento2$DATA_EV2 <- as.Date(tab_evento2$DATA_EV2, format = "%Y/%m/%d")
     head(tab_evento2$DATA_EV2)
     
     # Criando o vetor da data da janela de estimação EV2
     DATA_EV2 <- c(tab_evento2$DATA_EV2)
     Jan_Estimação_EV2 <- tab_evento2[tab_evento2$JANELA_EV2 == "ESTIMAÇÃO", ]
     
     #verificando a janela de estimação EV2
     min(Jan_Estimação_EV2$DATA_EV2)
     max(Jan_Estimação_EV2$DATA_EV2)
     View(Jan_Estimação_EV2)
     
     #criando o vetor dos retornos da JANELA DE ESTIMAÇÃO
     
     R_SPX_EV2 <- tab_evento2$R_SPX[tab_evento2$JANELA_EV2 == "ESTIMAÇÃO"]
     R_SHY_EV2 <- tab_evento2$R_SHY[tab_evento2$JANELA_EV2 == "ESTIMAÇÃO"]
     R_BTC_EV2 <- tab_evento2$R_BTC[tab_evento2$JANELA_EV2 == "ESTIMAÇÃO"]
     R_ETH_EV2 <- tab_evento2$R_ETH[tab_evento2$JANELA_EV2 == "ESTIMAÇÃO"]
     R_OURO_EV2 <- tab_evento2$R_OURO[tab_evento2$JANELA_EV2 == "ESTIMAÇÃO"]
     R_PRATA_EV2 <- tab_evento2$R_PRATA[tab_evento2$JANELA_EV2 == "ESTIMAÇÃO"]
     R_DXY_EV2 <- tab_evento2$R_DXY[tab_evento2$JANELA_EV2 == "ESTIMAÇÃO"]
     
     #Verificando os dados importados
     
     head(R_SPX_EV2)
     head(R_SHY_EV2)
     head(R_BTC_EV2)
     head(R_ETH_EV2)
     head(R_OURO_EV2)
     head(R_PRATA_EV2)
     head(R_DXY_EV2)
     
# Verificação do pressuposto da estacionariedade nas séries dos retornos da JANELA DE ESTIMAÇÃO
     
     #Dickey-Fuller Aumentado (ADF) 
     #H0: a série possui raíz unitária, não é estacionária - p-value >5%
     #H1: a série não possui raíz unitária, é estacionária - p-value <5%
     
     #KPSS
     #H0: a série é estacionária - p-value >5%
     #H1: a série não é estacionária, possui raíz unitária - p-value <5%
     
     adf.test(R_SPX_EV2)
     kpss.test(R_SPX_EV2)
     
     adf.test(R_SHY_EV2)
     kpss.test(R_SHY_EV2)
     
     adf.test(R_BTC_EV2)
     kpss.test(R_BTC_EV2)
     
     adf.test(R_ETH_EV2)
     kpss.test(R_ETH_EV2)
     
     adf.test(R_OURO_EV2)
     kpss.test(R_OURO_EV2)
     
     adf.test(R_PRATA_EV2)
     kpss.test(R_PRATA_EV2)
     
     adf.test(R_DXY_EV2)
     kpss.test(R_DXY_EV2)

#MODELO AUTORREGRESSIVO PARA CADA ATIVO - EVENTO 2
     
# SPX
     
     SPX_AIC_EV2 <- ar(R_SPX_EV2, method = "ols", aic = TRUE, order.max = 5)  #ATÉ 5 DEFASAGENS
     SPX_AIC_EV2$order #AIC = 0
     
     Mod_SPX_Autoregressive_EV2 <- lm(R_SPX_EV2 ~ 1 ) #COMO O AIC FOI 0, FOI FEITO O MODELO APENAS COM A CONSTANTE
     summary(Mod_SPX_Autoregressive_EV2)
     
     shapiro.test(Mod_SPX_Autoregressive_EV2$residuals) # Teste de Normalidade dos resíduos - R: há normalidade - p-value > 5%
     
     Box.test(Mod_SPX_Autoregressive_EV2$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%
     
# SHY
     
     SHY_AIC_EV2 <- ar(R_SHY_EV2, method = "ols", aic = TRUE, order.max = 5) #ATÉ 5 DEFASAGENS
     SHY_AIC_EV2$order #AIC = 4
     
     Dados_Mod_SHY_EV2 <- data.frame(R_SHY_EV2, LAG1_SHY_EV2 = dplyr::lag(R_SHY_EV2, 1), LAG2_SHY_EV2 = dplyr::lag(R_SHY_EV2, 2), 
                                 LAG3_SHY_EV2 = dplyr::lag(R_SHY_EV2, 3), LAG4_SHY_EV2 = dplyr::lag(R_SHY_EV2, 4)) #CRIANDO VARIÁVEIS COM LAG
     View(Dados_Mod_SHY_EV2) 
     
     Dados_Mod_SHY_EV2_Novo <- na.omit(Dados_Mod_SHY_EV2) #AJUSTANDO A DATA COM OS LAGs
     View(Dados_Mod_SHY_EV2_Novo)
     
     Mod_SHY_Autoregressive_EV2 <- lm(R_SHY_EV2 ~ LAG1_SHY_EV2 + LAG2_SHY_EV2 + LAG3_SHY_EV2 + LAG4_SHY_EV2, data = Dados_Mod_SHY_EV2_Novo) #ESTIMAÇÃO DO MODELO
     summary(Mod_SHY_Autoregressive_EV2)
     
     shapiro.test(Mod_SHY_Autoregressive_EV2$residuals) # Teste de Normalidade dos resíduos - R: há normalidade - p-value > 5%
     
     bptest(Mod_SHY_Autoregressive_EV2) # Teste de homocedasticidade - R: há homocedasticidade - p-value > 5%
     
     Box.test(Mod_SHY_Autoregressive_EV2$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%
     
     
#BITCOIN
     
     BTC_AIC_EV2 <- ar(R_BTC_EV2, method = "ols", aic = TRUE, order.max = 5) #ATÉ 5 DEFASAGENS
     
     BTC_AIC_EV2$order #AIC = 0
     
     
     Mod_BTC_Autoregressive_EV2 <- lm(R_BTC_EV2 ~ 1 ) #COMO O AIC FOI 0, FOI FEITO O MODELO APENAS COM A CONSTANTE
     summary(Mod_BTC_Autoregressive_EV2)
     
     shapiro.test(Mod_BTC_Autoregressive_EV2$residuals) # Teste de Normalidade dos resíduos - R: há normalidade - p-value > 5%
     
     Box.test(Mod_BTC_Autoregressive_EV2$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%

     
#ETHEREUM
     
     ETH_AIC_EV2 <- ar(R_ETH_EV2, method = "ols", aic = TRUE, order.max = 5) #ATÉ 5 DEFASAGENS
     
     ETH_AIC_EV2$order #AIC = 0
     
     
     Mod_ETH_Autoregressive_EV2 <- lm(R_ETH_EV2 ~ 1 ) #COMO O AIC FOI 0, FOI FEITO O MODELO APENAS COM A CONSTANTE
     summary(Mod_ETH_Autoregressive_EV2)
     
     shapiro.test(Mod_ETH_Autoregressive_EV2$residuals) # Teste de Normalidade dos resíduos - R: há normalidade - p-value > 5%
     
     Box.test(Mod_ETH_Autoregressive_EV2$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%
     
#OURO
     
     OURO_AIC_EV2 <- ar(R_OURO_EV2, method = "ols", aic = TRUE, order.max = 5) #ATÉ 5 DEFASAGENS
     OURO_AIC_EV2$order #AIC = 4

     Dados_Mod_OURO_EV2 <- data.frame(R_OURO_EV2, LAG1_OURO_EV2 = dplyr::lag(R_OURO_EV2, 1), LAG2_OURO_EV2 = dplyr::lag(R_OURO_EV2, 2), 
                                  LAG3_OURO_EV2 = dplyr::lag(R_OURO_EV2, 3), LAG4_OURO_EV2 = dplyr::lag(R_OURO_EV2, 4)) #CRIANDO VARIÁVEIS COM LAG
     View(Dados_Mod_OURO_EV2)
     
     Dados_Mod_OURO_EV2_Novo <- na.omit(Dados_Mod_OURO_EV2) #AJUSTANDO A DATA COM OS LAGs
     View(Dados_Mod_OURO_EV2_Novo)
     
     Mod_OURO_Autoregressive_EV2 <- lm(R_OURO_EV2 ~ LAG1_OURO_EV2 + LAG2_OURO_EV2 + LAG3_OURO_EV2 + LAG4_OURO_EV2, data = Dados_Mod_OURO_EV2_Novo) #ESTIMAÇÃO DO MODELO
     summary(Mod_OURO_Autoregressive_EV2)
     
     shapiro.test(Mod_OURO_Autoregressive_EV2$residuals) # Teste de Normalidade dos resíduos - R: há normalidade - p-value > 5%
     
     bptest(Mod_OURO_Autoregressive_EV2) # Teste de homocedasticidade - R: há homocedasticidade - p-value > 5%
     
     Box.test(Mod_OURO_Autoregressive_EV2$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5% 
     
# PRATA
     
     PRATA_AIC_EV2 <- ar(R_PRATA_EV2, method = "ols", aic = TRUE, order.max = 5) #ATÉ 5 DEFASAGENS
     PRATA_AIC_EV2$order #AIC = 4
     
     Dados_Mod_PRATA_EV2 <- data.frame(R_PRATA_EV2, LAG1_PRATA_EV2 = dplyr::lag(R_PRATA_EV2, 1), LAG2_PRATA_EV2 = dplyr::lag(R_PRATA_EV2, 2), 
                                      LAG3_PRATA_EV2 = dplyr::lag(R_PRATA_EV2, 3), LAG4_PRATA_EV2 = dplyr::lag(R_PRATA_EV2, 4)) #CRIANDO VARIÁVEIS COM LAG
     View(Dados_Mod_PRATA_EV2)
     
     Dados_Mod_PRATA_EV2_Novo <- na.omit(Dados_Mod_PRATA_EV2) #AJUSTANDO A DATA COM OS LAGs
     View(Dados_Mod_PRATA_EV2_Novo)
     
     Mod_PRATA_Autoregressive_EV2 <- lm(R_PRATA_EV2 ~ LAG1_PRATA_EV2 + LAG2_PRATA_EV2 + LAG3_PRATA_EV2 + LAG4_PRATA_EV2, data = Dados_Mod_PRATA_EV2_Novo) #ESTIMAÇÃO DO MODELO
     summary(Mod_PRATA_Autoregressive_EV2)
     
     shapiro.test(Mod_PRATA_Autoregressive_EV2$residuals) # Teste de Normalidade dos resíduos - R: há normalidade - p-value > 5%
     
     bptest(Mod_PRATA_Autoregressive_EV2) # Teste de homocedasticidade - R: há homocedasticidade - p-value > 5%
     
     Box.test(Mod_PRATA_Autoregressive_EV2$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5% 
     
# DXY
     
     DXY_AIC_EV2 <- ar(R_DXY_EV2, method = "ols", aic = TRUE, order.max = 5) #ATÉ 5 DEFASAGENS
     DXY_AIC_EV2$order #AIC = 0
     
     
     Mod_DXY_Autoregressive_EV2 <- lm(R_DXY_EV2 ~ 1 ) #COMO O AIC FOI 0, FOI FEITO O MODELO APENAS COM A CONSTANTE
     summary(Mod_DXY_Autoregressive_EV2)
     
     shapiro.test(Mod_DXY_Autoregressive_EV2$residuals) # Teste de Normalidade dos resíduos - R: há normalidade - p-value > 5%
     
     Box.test(Mod_DXY_Autoregressive_EV2$residuals, type = "Ljung-Box", lag = 5) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%
     


