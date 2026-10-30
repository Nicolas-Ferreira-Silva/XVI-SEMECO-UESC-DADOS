

# 1º PARTE ----------------------------------------------
#(Nessa parte serão realizadas: 
#                importação da tabela e instalação dos pacotes;
#                Importação das variáveis de retorno dos ativos
#                teste de estacionariedade sobre os retornos, antes do modelo)

   options(scipen = 999) #Retirar notação científica

#Instalação dos pacotes necessários

if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, readxl, RVAideMemoire, car, tseries, FinTS, lmtest, sandwich)

# Importando tabela e página

   basededados <- read_excel("Base_SafeHaven_And_Hedge.xlsx", sheet = "BASER")

# Conferindo página importada 
   View(basededados)

#Formatando o separador das datas de (/) para (-)

  head(basededados$DATA_FSR)
  basededados$DATA_FSR <- as.Date(basededados$DATA_FSR, format = "%Y/%m/%d")
  head(basededados$DATA_FSR)

#Criando o vetor dos retornos

    #cRIPTOMOEDAS
     R_BTC <- basededados$R_BTC 
     R_ETH <- basededados$R_ETH

    #METAIS
     R_OURO <- basededados$R_OURO 
     R_PRATA <- basededados$R_PRATA

    #MOEDA
     R_DXY <- basededados$R_DXY

    #TÍTULO DE CURTO PRAZO
     R_SHY <- basededados$R_SHY

    #MERCADO ACIONÁRIO
     R_SPX <- basededados$R_SPX

# Verificando os retornos importados

    #cRIPTOMOEDAS
     head(R_BTC) 
     head(R_ETH)

    #METAIS
     head(R_OURO) 
     head(R_PRATA)

    #MOEDA
     head(R_DXY)

    #TÍTULO DE CURTO PRAZO
     head(R_SHY)

    #MERCADO ACIONÁRIO
     head(R_SPX)


# Verificação do pressuposto da estacionariedade nas séries dos retornos

  #Dickey-Fuller Aumentado (ADF) 
  #H0: a série possui raíz unitária, não é estacionária - p-value >5%
  #H1: a série não possui raíz unitária, é estacionária - p-value <5%

  #KPSS
  #H0: a série é estacionária - p-value >5%
  #H1: a série não é estacionária, possui raíz unitária - p-value <5%

      #cRIPTOMOEDAS
       adf.test(R_BTC)
       kpss.test(R_BTC)

       adf.test(R_ETH)
       kpss.test(R_ETH)

     #METAIS
      adf.test(R_OURO)
      kpss.test(R_OURO)

      adf.test(R_PRATA)
      kpss.test(R_PRATA)

    #MOEDA
     adf.test(R_DXY)
     kpss.test(R_DXY)

   #TÍTULO DE CURTO PRAZO
     adf.test(R_SHY)
     kpss.test(R_SHY)

   #MERCADO ACIONÁRIO
     adf.test(R_SPX)
     kpss.test(R_SPX)

# 2º PARTE -------------------------------

#Criando o vetor das variáveis de crise

# Variável criada no excel
# Ativos considerados: SPX e SHY
# Considera crise quando: o retorno faz parte dos 5% piores resultados (quantil)
#                         sendo assim, atribuido o valor de 1
#                         resultando em: R_X * crise(1) = RC_x, ONDE X é o ativo.

# Não considera crise quando: o retorno não faz parte dos 5% piores resultados (quantil)
#                             sendo assim, atribuido o valor de 0.


RC_SPX <- basededados$RC_SPX
RC_SHY <- basededados$RC_SHY

##Construção do Modelo SAFEHAVEN AND HEDGE para os Ativos

        # BTC 

mod_btc_CP <- lm(R_BTC ~ R_SPX + RC_SPX + R_SHY + RC_SHY) #Regressão Múltipla

vif(mod_btc_CP) # Teste de Multicolinearidade - R: Não Há multicolinearidade - VIF < 10
##OBS: como as variáveis independentes não mudam nesta seção, não será mais aplicado esse teste.

shapiro.test(mod_btc_CP$residuals) # Teste de Normalidade dos resíduos - R: Não há normalidade - p-value < 5%

bptest(mod_btc_CP) # Teste de homocedasticidade - R: Não há homocedasticidade - p-value < 5%

Box.test(mod_btc_CP$residuals, type = "Ljung-Box", lag = 20) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%  

coeftest(mod_btc_CP, vcov. = NeweyWest(mod_btc_CP)) #Correção de Newey-West erros-padrão robusto

        # ETH

mod_eth_CP <- lm(R_ETH ~ R_SPX + RC_SPX + R_SHY + RC_SHY) #Regressão Múltipla

shapiro.test(mod_eth_CP$residuals) # Teste de Normalidade dos resíduos - R: Não há normalidade - p-value < 5%

bptest(mod_eth_CP) # Teste de homocedasticidade - R: Não há homocedasticidade - p-value < 5%

Box.test(mod_eth_CP$residuals, type = "Ljung-Box", lag = 20) #teste de autocorrelação R: há autocorrelação serial - p-value < 5%  

coeftest(mod_eth_CP, vcov. = NeweyWest(mod_eth_CP)) #Correção de Newey-West erros-padrão robusto

       # OURO

mod_ouro_CP <- lm(R_OURO ~ R_SPX + RC_SPX + R_SHY + RC_SHY) #Regressão Múltipla

shapiro.test(mod_ouro_CP$residuals) # Teste de Normalidade dos resíduos - R: Não há normalidade - p-value < 5%

bptest(mod_ouro_CP) # Teste de homocedasticidade - R: Não há homocedasticidade - p-value < 5%

Box.test(mod_ouro_CP$residuals, type = "Ljung-Box", lag = 20) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%

coeftest(mod_ouro_CP, vcov. = NeweyWest(mod_ouro_CP)) #Correção de Newey-West erros-padrão robusto


       # PRATA

mod_prata_CP <- lm(R_PRATA ~ R_SPX + RC_SPX + R_SHY + RC_SHY) #Regressão Múltipla

shapiro.test(mod_prata_CP$residuals) # Teste de Normalidade dos resíduos - R: Não há normalidade - p-value < 5%

bptest(mod_prata_CP) # Teste de homocedasticidade - R: Não há homocedasticidade - p-value < 5%

Box.test(mod_prata_CP$residuals, type = "Ljung-Box", lag = 20) #teste de autocorrelação R: não há autocorrelação serial - p-value > 5%

coeftest(mod_prata_CP, vcov. = NeweyWest(mod_prata_CP)) #Correção de Newey-West erros-padrão robusto

       # DXY
mod_dxy_CP <- lm(R_DXY ~ R_SPX + RC_SPX + R_SHY + RC_SHY) #Regressão Múltipla

shapiro.test(mod_dxy_CP$residuals) # Teste de Normalidade dos resíduos - R: Não há normalidade - p-value < 5%

bptest(mod_dxy_CP) # Teste de homocedasticidade - R: Não há homocedasticidade - p-value < 5%

Box.test(mod_dxy_CP$residuals, type = "Ljung-Box", lag = 20) #teste de autocorrelação R: há autocorrelação serial - p-value < 5%

coeftest(mod_dxy_CP, vcov. = NeweyWest(mod_dxy_CP)) #Correção de Newey-West erros-padrão robusto
