rm(list = ls())
library(fpc)
library(cluster)
library(kmed)
library(e1071)
library(dplyr)
library(ggplot2)
require(stringr)

require("curl")
require(bit64)

# setwd("C:/Users/pchir/Downloads/CSV_RJ")
# 
# basico <- read.csv("BASICO_RJ.csv", header = TRUE, sep = ";")
# entorno <- read.csv("Entorno03_RJ.csv", header = TRUE, sep = ";")
# dom.i <- read.csv("Domicilio01_RJ.csv", header = TRUE, sep = ";")
# dom.ii <- read.csv("Domicilio02_RJ.csv", header = TRUE, sep=";")
# pessoa <- read.csv("Pessoa03_RJ.csv", header = TRUE, sep=";")
# dom.renda <- read.csv("DomicilioRenda_RJ.csv", header = TRUE, sep=";")
# resp.alfa <- read.csv("DomicilioRenda_RJ.csv", header = TRUE, sep=";")


VulIndex = function(basico,entorno,dom.i,dom.ii,pessoa,dom.renda){
  # definiÃ§Ã£o das variÃ¡veis que farÃ£o parte do DataFrame final
  features <- c("V001","V001p","V001r","V003", "V004", "V005", "V006","V422", "V423", "V425", "V427", "V429", "V431", "V433", "V435", "V437", "V439", "V447", "V449", "V451", "V453", "V455", "V457",
                "V472", "V474", "V476", "V478", "V480", "V482", "V050", "V051", "V052", "V053", "V054", "V055", "V056", "V057", "V058", "V059",
                "V081", "V082", "V083", "V084", "V085", "V086", "V087", "V012", "V016", "V003", "V002","V093")

  # definiÃ§Ã£o de objetos vars para auxÃ­lio Ã s funÃ§Ãµes select
  vars.entorno <- vars(V422, V423, V425, V427, V429, V431, V433, V435, V437, V439, V447, V449, V451, V453, V455, V457, V472, V474, V476, V478, V480, V482)
  vars.dom <- vars(V001, V050, V051, V052, V053, V054, V055, V056, V057, V058, V059, V081, V082, V083, V084, V085, V086, V087)
  vars.resp.alfa <- vars(V093,V001)
  
  # seleciona apenas as variÃ¡veis de interesse de cada DataFrame, assim como define uma coluna extra (Mun) que contÃ©m o cÃ³digo do municÃ­pio
  # a variÃ¡vel Cod_setor Ã© mantida em todos os DataFrames, pois ela permite encontrar cÃ³digo e nome do bairro
  entorno <- select(entorno, c(Cod_setor, Situacao_setor, !!!vars.entorno)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
  dom.i <- select(dom.i, c(Cod_setor, !!!vars.dom)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
  dom.ii <- select(dom.ii, c(Cod_setor, V001, V012, V016)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
  pessoa <- select(pessoa, c(Cod_setor, V001, V003, V004, V005, V006)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
  dom.renda <- select(dom.renda, c(Cod_setor, V001, V002)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
  resp.alfa <- select(resp.alfa, c(Cod_setor, V093,V001)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
  
  # Renomear campo V001 da tabela pessoa para V001p
  pessoa$V001p <- pessoa$V001
  pessoa <- select(pessoa,-c(V001))
  
  # Renomear campo V001 da tabela respon.alfa para V001r
  resp.alfa$V001r <- resp.alfa$V001
  resp.alfa <- select(resp.alfa,-c(V001))
  
  # define uma variÃ¡vel que assume o identificador do municÃ­pio do Rio de Janeiro
  # id.mun <- 3304557
  
  # exclui a coluna Mun, pois ela jÃ¡ nÃ£o Ã© mais necessÃ¡ria, e remove as linhas com valores indesejados
  entorno <- select(filter(entorno, V423 != "X" ), -Mun)
  dom.i <- select(filter(dom.i, V001 > 0 & V052 != "X"), -Mun, -V001)
  dom.ii <- select(filter(dom.ii, V001 > 0 & V012 != "X" ), -Mun)
  pessoa <- select(filter(pessoa, V001p > 0 & V003 != "X"), -Mun)
  dom.renda <- select(filter(dom.renda, V001 != "X"), -Mun, -V001)
  resp.alfa <- select(filter(resp.alfa, V093 != "X"), -Mun)
  
  # altera o tipo das variÃ¡veis do DataFrame dom.i para numÃ©rico
  dom.i <- dom.i %>%
    mutate_at(vars(V050, V051, V052, V053, V054, V055, V056, V057, V058, V059, V081, V082, V083, V084, V085, V086, V087), function(x) as.numeric(as.character(x)))
  
  
  # cada variÃ¡vel do DataFrame dom.i se refere a nÃºmero de pessoas vivendo num determinado domicÃ­lio
  # como a idÃ©ia Ã© ter nÃºmero de pessoas por domicÃ­lio numa dada condiÃ§Ã£o, fazemos o multiplicaÃ§Ã£o do nÃºmero de domicÃ­lios pelo nÃºmero de pessoas
  # que vivem no domicÃ­lio
  dom.i$V051 <- dom.i$V051 * 2; dom.i$V052 <- dom.i$V052 * 3; dom.i$V053 <- dom.i$V053 * 4; dom.i$V054 <- dom.i$V054 * 5; dom.i$V055 <- dom.i$V055 * 6;
  dom.i$V056 <- dom.i$V056 * 7; dom.i$V057 <- dom.i$V057 * 8; dom.i$V058 <- dom.i$V058 * 9; dom.i$V059 <- dom.i$V059 * 10;
  
  # similar ao que foi feita nas duas linhas acima, sÃ³ que este cÃ¡lculo Ã© para definir quantas pessoas vivem em domicÃ­lios que tem mulheres
  # como mantenedoras
  dom.i$V081 <- dom.i$V081 * 2; dom.i$V082 <- dom.i$V082 * 3; dom.i$V083 <- dom.i$V083 * 4; dom.i$V084 <- dom.i$V084 * 5;
  dom.i$V085 <- dom.i$V085 * 6; dom.i$V086 <- dom.i$V086 * 7;
  
  # a linha comentada abaixo sÃ³ foi utilizada para verificar se os cÃ¡lculos nas 4 linhas acima faziam sentido
  # descomentar caso queira verificar (compare a variÃ¡vel V001 de dom.i com a variÃ¡vel V422 do entorno)
  # dom.i$V001 <- rowSums(dom.i[, c("V050", "V051", "V052", "V053", "V054", "V055", "V056", "V057", "V058", "V059")])
  
  # seleciona as colunas de interesse do DataFrame bairros
  bairros <- select(basico, c(Cod_UF,Cod_setor, Cod_bairro, Nome_do_bairro,Cod_municipio,Nome_do_municipio))
  
  # entorno <- entorno %>%
  #   mutate_all(function(x) as.numeric(str_replace(x, ",",".")))
  
  #entorno$Cod_setor <- as.numeric(str_replace(entorno$Cod_setor, ",","."))

  # junta todos os DataFrames pela coluna Cod_setor
  resumo <- inner_join(inner_join(inner_join(inner_join(inner_join(entorno, dom.i, by=c("Cod_setor")), dom.ii, by=c("Cod_setor")), pessoa, by=c("Cod_setor")), resp.alfa, by=c("Cod_setor")), dom.renda, by=c("Cod_setor"))
  
  # a variÃ¡vel V002 vem do arquivo DomicilioRenda que descreve a renda total das regiÃµes definidas pelo setor censitÃ¡rio
  # ao dividir este valor total de rendas pelo nÃºmero total de pessoas (representado pela variÃ¡vel V422) obtem-se a renda per capita da regiÃ£o
  #resumo$V002 <- as.numeric(resumo$V002) / as.numeric(resumo$V422)
  # LINHA COMENTADA POIS JÃ ESTÃ SENDO FEITO NA LINHA 105
  
  # regiÃµes onde o Censo identificou 0 pessoas, nÃ£o servem para a anÃ¡lise, portanto tais registros devem ser removidos
  # o DataFrame selected.features contem apenas as variÃ¡veis que serÃ£o utilizadas para realizar os cÃ¡lculos
  
  selected.features <- select(filter(resumo, V422!="0"),features)
  
  # Adiciona a informaÃ§Ã£o de bairro ao DataFrame que contem todas as demais informaÃ§Ãµes coletadas pelo Censo
  resumo <- inner_join(bairros, filter(resumo, V422!="0"), by=c("Cod_setor"))
  
  # converte todas as variÃ¡veis para o tipo numÃ©rico
  selected.features <- mutate_all(selected.features, function(x) as.numeric(as.character(x)))
  
  
  # calcula a proporÃ§Ã£o de pessoas vivendo nas condiÃ§Ãµes descritas pelas variÃ¡veis selecionadas
  features.abs <- selected.features
  
  # Cria cluster por faixa de renda
  # calcula a componente Renda e aplica o peso
  compDomRenda <- ifelse(features.abs$V002/features.abs$V001p>2090,1,
                         ifelse(features.abs$V002/features.abs$V001p>1045,0.7,
                                ifelse(features.abs$V002/features.abs$V001p>522.5,0.5,
                                       ifelse(features.abs$V002/features.abs$V001p>178,0.3,
                                              ifelse(features.abs$V002/features.abs$V001p>89,0.2,0.1)))))
  
  # calcula a componente Entorno do IVC e aplica os pesos
  compEntorno <-
    #Logradouro
    rowSums(features.abs[, c("V423", "V425", "V427")])/features.abs$V422 * (1/7) +
    #IluminaÃ§Ã£o PÃºblica
    rowSums(features.abs[, c("V429", "V431", "V433")])/features.abs$V422 * (1/7) +
    #PavimentaÃ§Ã£o
    rowSums(features.abs[, c("V435", "V437", "V439")])/features.abs$V422 * (1/7) +
    #Meio-fio/guia
    rowSums(features.abs[, c("V447", "V449", "V451")])/features.abs$V422 * (1/7) +
    #Bueiro/Boca de lobo
    rowSums(features.abs[, c("V453", "V455", "V457")])/features.abs$V422 * (1/7) +
    #Esgoto
    rowSums(features.abs[, c("V472", "V474", "V476")])/features.abs$V422 * (1/7) +
    #Lixo
    rowSums(features.abs[, c("V478", "V480", "V482")])/features.abs$V422 * (1/7)
  
  # Calcula componente de calculo mais de duas pessoas por domicÃ­lio
  # Calcula % de pessoas que moram sÃ³s ou com atÃ© mais uma outra pessoa
  # 1 - Soma a qtde de pessoas que vivem com mais do que 5 pessoas (total)
  # 2 - Subtrai de 1, a qtde de pessoas que vivem com mais do que 2 pessoas em residÃªncias sustentadas por mulheres
  # 3 - Divide o resultado de 2 pelo nÃºmero total de pessoas vivendo com mais do que 2 pessoas
  comp5maisdomicilio <-
    (1 - (rowSums(features.abs[, c("V055", "V056", "V057", "V058", "V059")])/
            (features.abs[, c("V422")]))) * (1/5)
  
  # Calcula % de pessoas com acesso a banheiro de uso exclusivo
  compbanheiro <-
    (features.abs[,c("V016")]/features.abs[,c("V001")]) * (1/5)
  
  # Calcula % de pessoas com acesso a rede de distribuiÃ§Ã£o de Ã¡gua
  compagua <-
    (features.abs[,c("V012")]/features.abs[,c("V001")]) * (1/5)
  
  compDomRenda <- compDomRenda * (2/5)
  compDomicilios <- compagua + compbanheiro + comp5maisdomicilio + compDomRenda
  
  # calcula a componente Docmicilios com mulheres como mantenedoras e aplica os pesos
  compDomiciliosMulher <-
    # Qtd pessoas domicios com mulheres como mantenedoras / Qtd pessoas dos domicilios
    (1-(rowSums(features.abs[,
                             c("V081","V082", "V083", "V084", "V085", "V086", "V087")]))/
       features.abs[, c("V422")]) * (1/3)
  
  
  # calcula % Pessoas brancas
  compPessoas <-
    (1 - rowSums(features.abs[,
                              c("V003", "V004", "V005", "V006")])/features.abs[,c("V001p")])*1/3
  
  # calcula % responsaveis analfa.
  compRespAlfa <-
    (1 - features.abs[,c("V093")]/features.abs[,c("V001r")])*1/3
  
  compPessoas <- compPessoas + compDomiciliosMulher + compRespAlfa
  
  # soma todas as componentes para formar o IVC
  # subtraindo as componentes de banheiros e agua para nÃ£o penalizar as regiÃµes 100% estruturadas nesse quesito
  # ipc <- (compDomRenda * .5) + (compEntorno * .2) + (compDomicilios * .2) + (compPessoas * .05)
  ipc <- (compEntorno * (1/3)) + (compPessoas * (1/3)) + (compDomicilios * (1/3))
  
  # adiciona a coluna IVC ao DataFrame que contem as informaÃ§Ãµes que permitem identificar o bairro de cada setor censitÃ¡rio
  resumoFinal <- cbind(resumo, ipc)
  
  # junta aos dados de UBS's, calcula a componentes da UBS, e adiciona ao IVC
  # resumoFinal <- left_join(resumoFinal, ubs, by=c("Cod_bairro"))
  
  # alguns bairros nÃ£o tÃªm UBS, o que resulta em NA
  # substituir NA por 0
  resumoFinal[is.na(resumoFinal)] <- 0
  
  #compUBS <- ifelse(resumoFinal$hospital >= 5,1,
  #                 ifelse(resumoFinal$hospital == 4,0.7,
  #                       ifelse(resumoFinal$hospital == 3,0.5,
  #                             ifelse(resumoFinal$hospital == 2,0.3,
  #                                   ifelse(resumoFinal$hospital == 1,0.1,0)))))
  
  # resumo.final <- cbind(resumo, compDomRenda, compEntorno, compDomiciliosMulher, comp5maisdomicilio, compbanheiro, compagua, compPessoas)
  
  # resumoFinal$ivc <- resumoFinal$ivc + (1 - resumoFinal$hospital) * (1/14)
  resumoFinal$ipc <- resumoFinal$ipc
  # resumoFinal$ivc <- resumoFinal$ipc + compUBS * 1
  
  resumo.final <- cbind(resumoFinal, compDomRenda, compEntorno, compDomiciliosMulher, comp5maisdomicilio, compbanheiro, compagua, compPessoas)
  
  resumo.final <- select(resumo.final, c(Cod_UF,Cod_setor, Cod_municipio,Nome_do_municipio,
                                         Cod_bairro, Nome_do_bairro, ipc,compEntorno, compDomRenda, compDomiciliosMulher,
                                         comp5maisdomicilio, compbanheiro, compagua, compPessoas))

  return(resumo.final)
  
}

# ipc.RJ <- VulIndex(basico,entorno,dom.i,dom.ii,pessoa,dom.renda)
# dados <- reduction_dimension(ipc.RJ)

# ipc.RJ%>%str

### todos os estados ###
set.seed(1234)

library(dplyr)
#library(devtools)
#install_github("pchiroque/vulindex")
#library(vulindex)

root <- "C:/Users/pchir/Downloads"

states <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG",
            "MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR",
            "RS","SC","SE","SP1","TO")
#6,7,11,23,27
#"CE" "DF" "MG" "RS" "TO"

#st=19
library (readr)
full <- NULL
for(st in c(1:27)[-c(6,7,11,23,27)]){
  
  estado <- states[st] 
  
  # allocated data set here! 
  root <- "C:/Users/pchir/Downloads"
  root <- paste0(root,"/CSV_",estado)
  setwd(root)

  # read data set here! 
  basico <- read.csv(paste0("BASICO_",estado,".csv"), header = TRUE, sep = ";",check.names = FALSE,encoding = "UTF-8")
  entorno <- read.csv(paste0("Entorno03_",estado,".csv"), header = TRUE, sep = ";",encoding = "UTF-8")
  dom.i <- read.csv(paste0("Domicilio01_",estado,".csv"), header = TRUE, sep = ";",encoding = "UTF-8")
  dom.ii <- read.csv(paste0("Domicilio02_",estado,".csv"), header = TRUE, sep=";",encoding = "UTF-8")
  pessoa <- read.csv(paste0("Pessoa03_",estado,".csv"), header = TRUE, sep=";",encoding = "UTF-8")
  dom.renda <- read.csv(paste0("DomicilioRenda_",estado,".csv"), header = TRUE, sep=";",encoding = "UTF-8")
  resp.alfa <- read.csv(paste0("Responsavel02_",estado,".csv"), header = TRUE, sep=";",encoding = "UTF-8")
  
  ipc_f <- VulIndex(basico,entorno,dom.i,dom.ii,pessoa,dom.renda)
  ipc_f$Nome_UF <- estado
  full <- rbind(full,ipc_f)
}

ipc.full <- full 

#save(ipc.full,file = "C:/Users/pchir/Google Drive/AcaoCovid/indice_vulnerabilidade-master/Pacote_IPC/IPC_FULL_UF_MUNI_BAIRRO.rda")

load("C:/Users/pchir/Google Drive/AcaoCovid/indice_vulnerabilidade-master/Pacote_IPC/IPC_FULL.rda")


library("readxl")

ipc.Cap_states <- ipc.full%>%group_by(UF)%>%summarise_at("ipc",mean)%>%
  `colnames<-`(c("state","ipc"))

Nome_capital <- read_excel("C:/Users/pchir/Google Drive/AcaoCovid/p_R0_Vacinados/dados_R0-original/IBGE_states.xlsx",sheet = "InfoFull_IBGE")
Nome_capital%>%str
#ipc.Cap_states%>%left_join(Nome_capital)

ipc_Capitais_UF <- Nome_capital%>%left_join(ipc.Cap_states)%>%
  dplyr::select(c("state","UF","Capital","Cod_IBGE","ipc"))%>%
  as.data.frame()

#save(ipc_Capitais_UF,file = "ipc_Capitais_UF.rda")
