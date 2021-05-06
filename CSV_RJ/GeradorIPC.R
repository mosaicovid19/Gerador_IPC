#set seed para evitar valores diferentes
set.seed(1234)

library(fpc)
library(cluster)
library(kmed)
library(e1071)

library(dplyr)

setwd("C:/Users/Bruna/Documents/MOSAICOVID-19/RJ/Base informaçoes setores2010 universo RJ/CSV")

#leitura dos arquivos csv

bairros <- read.csv("BASICO_RJ.csv", header = TRUE, sep = ";")

entorno <- read.csv("Entorno03_RJ.csv", header = TRUE, sep = ";")
dom.i <- read.csv("Domicilio01_RJ.csv", header = TRUE, sep = ";")
dom.ii <- read.csv("Domicilio02_RJ.csv", header = TRUE, sep=";")
pessoa <- read.csv("Pessoa03_RJ.csv", header = TRUE, sep=";")
dom.renda <- read.csv("DomicilioRenda_RJ.csv", header = TRUE, sep=";")
ubs <- read.csv("ubs_rj.csv", header = TRUE, sep = ";")

# define uma coluna com 1's apenas para agrupar UBS's por bairro, assim como calcular nº total de UBS's
ubs$hospital <- 1

ubs <- ubs %>% 
  group_by(Cod_bairro) %>% 
  summarise(hospital=sum(hospital))

# Atribui valores conforme a qtd de hospitais por bairro
ubs$faixa <- ifelse(ubs$hospital>4,1,
                    ifelse(ubs$hospital>3,0.7,
                           ifelse(ubs$hospital>2,0.5,
                                  ifelse(ubs$hospital>1,0.3,0.1))))* (1/14)




# definição das variáveis que farão parte do DataFrame final
features <- c("V001","V001p","V003", "V004", "V005", "V006","V422", "V423", "V425", "V427", "V429", "V431", "V433", "V435", "V437", "V439", "V447", "V449", "V451", "V453", "V455", "V457", 
              "V472", "V474", "V476", "V478", "V480", "V482", "V050", "V051", "V052", "V053", "V054", "V055", "V056", "V057", "V058", "V059",
              "V081", "V082", "V083", "V084", "V085", "V086", "V087", "V012", "V016", "V003", "V002")

# definição de objetos vars para auxílio às funções select
vars.entorno <- vars(V422, V423, V425, V427, V429, V431, V433, V435, V437, V439, V447, V449, V451, V453, V455, V457, V472, V474, V476, V478, V480, V482)
vars.dom <- vars(V001, V050, V051, V052, V053, V054, V055, V056, V057, V058, V059, V081, V082, V083, V084, V085, V086, V087)

# seleciona apenas as variáveis de interesse de cada DataFrame, assim como define uma coluna extra (Mun) que contém o código do município
# a variável Cod_setor é mantida em todos os DataFrames, pois ela permite encontrar código e nome do bairro
entorno <- select(entorno, c(Cod_setor, Situacao_setor, !!!vars.entorno)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
dom.i <- select(dom.i, c(Cod_setor, !!!vars.dom)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
dom.ii <- select(dom.ii, c(Cod_setor, V001, V012, V016)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
pessoa <- select(pessoa, c(Cod_setor, V001, V003, V004, V005, V006)) %>% mutate(Mun = substr(Cod_setor, 1, 7))
dom.renda <- select(dom.renda, c(Cod_setor, V001, V002)) %>% mutate(Mun = substr(Cod_setor, 1, 7))

# Renomear campo V001 da tabela pessoa para V001p
pessoa$V001p <- pessoa$V001 
pessoa <- select(pessoa,-c(V001))


# define uma variável que assume o identificador do município do Rio de Janeiro
id.mun <- 3304557

# exclui a coluna Mun, pois ela já não é mais necessária, e remove as linhas com valores indesejados
entorno <- select(filter(entorno, V423 != "X" & Mun == id.mun), -Mun)
dom.i <- select(filter(dom.i, V001 > 0 & V052 != "X" & Mun == id.mun), -Mun, -V001)
dom.ii <- select(filter(dom.ii, V001 > 0 & V012 != "X" & Mun == id.mun), -Mun)
pessoa <- select(filter(pessoa, V001p > 0 & V003 != "X" & Mun == id.mun), -Mun)
dom.renda <- select(filter(dom.renda, V001 != "X" & Mun == id.mun), -Mun, -V001)

# altera o tipo das variáveis do DataFrame dom.i para numérico
dom.i <- dom.i %>%
  mutate_at(vars(V050, V051, V052, V053, V054, V055, V056, V057, V058, V059, V081, V082, V083, V084, V085, V086, V087), function(x) as.numeric(as.character(x)))


# cada variável do DataFrame dom.i se refere a número de pessoas vivendo num determinado domicílio
# como a idéia é ter número de pessoas por domicílio numa dada condição, fazemos o multiplicação do número de domicílios pelo número de pessoas
# que vivem no domicílio
dom.i$V051 <- dom.i$V051 * 2; dom.i$V052 <- dom.i$V052 * 3; dom.i$V053 <- dom.i$V053 * 4; dom.i$V054 <- dom.i$V054 * 5; dom.i$V055 <- dom.i$V055 * 6;
dom.i$V056 <- dom.i$V056 * 7; dom.i$V057 <- dom.i$V057 * 8; dom.i$V058 <- dom.i$V058 * 9; dom.i$V059 <- dom.i$V059 * 10;

# similar ao que foi feita nas duas linhas acima, só que este cálculo é para definir quantas pessoas vivem em domicílios que tem mulheres
# como mantenedoras
dom.i$V081 <- dom.i$V081 * 2; dom.i$V082 <- dom.i$V082 * 3; dom.i$V083 <- dom.i$V083 * 4; dom.i$V084 <- dom.i$V084 * 5;
dom.i$V085 <- dom.i$V085 * 6; dom.i$V086 <- dom.i$V086 * 7;

# a linha comentada abaixo só foi utilizada para verificar se os cálculos nas 4 linhas acima faziam sentido
# descomentar caso queira verificar (compare a variável V001 de dom.i com a variável V422 do entorno)
# dom.i$V001 <- rowSums(dom.i[, c("V050", "V051", "V052", "V053", "V054", "V055", "V056", "V057", "V058", "V059")])

# seleciona as colunas de interesse do DataFrame bairros
bairroscod <- select(bairros, c(Cod_setor, Cod_bairro))

# junta todos os DataFrames pela coluna Cod_setor
resumo <- inner_join(inner_join(inner_join(inner_join(entorno, dom.i, by=c("Cod_setor")), dom.ii, by=c("Cod_setor")), pessoa, by=c("Cod_setor")), dom.renda, by=c("Cod_setor"))

# a variável V002 vem do arquivo DomicilioRenda que descreve a renda total das regiões definidas pelo setor censitário
# ao dividir este valor total de rendas pelo número total de pessoas (representado pela variável V422) obtem-se a renda per capita da região
#resumo$V002 <- as.numeric(resumo$V002) / as.numeric(resumo$V422)
# LINHA COMENTADA POIS JÀ ESTÁ SENDO FEITO NA LINHA 105 


# Adiciona a informação de bairro ao DataFrame que contem todas as demais informações coletadas pelo Censo
resumo <- inner_join(bairroscod, filter(resumo, V422!="0"), by=c("Cod_setor"))

# converte todas as variáveis para o tipo numérico
features.norm <- mutate_all(resumo, function(x) as.numeric(as.character(x)))

# seleciona as colunas de interesse do DataFrame bairros
bairros <- select(bairros, c(Cod_bairro,Nome_do_bairro))
features.norm <- inner_join(features.norm, bairros, by=c("Cod_bairro"))

# Cria cluster por faixa de renda 
# calcula a componente Renda e aplica o peso
features.norm$compDomRenda <- ifelse(features.norm$V002>2090,1,
                       ifelse(features.norm$V002>1045,0.7,
                              ifelse(features.norm$V002>522.5,0.5,
                                     ifelse(features.norm$V002>178,0.3,
                                            ifelse(features.norm$V002>89,0.2,0.1)))))* (1/14)

# calcula a componente Entorno do IVC e aplica os pesos
features.norm$compEntorno <- 
  #Logradouro
  rowSums(features.norm[, c("V423", "V425", "V427")])/features.norm$V422 * (1/14) +
  #Iluminação Pública
  rowSums(features.norm[, c("V429", "V431", "V433")])/features.norm$V422 * (1/14) +
  #Pavimentação
  rowSums(features.norm[, c("V435", "V437", "V439")])/features.norm$V422 * (1/14) +
  #Meio-fio/guia
  rowSums(features.norm[, c("V447", "V449", "V451")])/features.norm$V422 * (1/14) +
  #Bueiro/Boca de lobo 
  rowSums(features.norm[, c("V453", "V455", "V457")])/features.norm$V422 * (1/14) +
  #Esgoto 
  rowSums(features.norm[, c("V472", "V474", "V476")])/features.norm$V422 * (1/14) +
  #Lixo 
  rowSums(features.norm[, c("V478", "V480", "V482")])/features.norm$V422 * (1/14)

# calcula a componente Docmicilios com mulheres como mantenedoras e aplica os pesos
features.norm$compDomiciliosMulher <-
  # Qtd pessoas domicios com mulheres como mantenedoras / Qtd pessoas dos domicilios
  (1-(rowSums(features.norm[, 
                            c("V082", "V083", "V084", "V085", "V086", "V087")]))/
     rowSums(features.norm[, 
                           c("V052", "V053", "V054", "V055", "V056", "V057", "V058", "V059")])) * (1/14)


# Calcula componente de calculo mais de duas pessoas por domicílio 
features.norm$comp2maisdomicilio <- 
  (1 - (rowSums(features.norm[, c("V052", "V053", "V054", "V055", "V056", "V057", "V058", "V059")]) - 
          (rowSums(features.norm[,c("V082", "V083", "V084", "V085", "V086", "V087")]))))/
  (rowSums(features.norm[, c("V052", "V053", "V054", "V055", "V056", "V057", "V058", "V059")])) * (1/14)

# Calcula componente de calculo Banheiro 
features.norm$compbanheiro <- 
  (1 - features.norm[,c("V016")]/features.norm[,c("V001")]) * (1/14)

# Calcula componente de calculo Agua 
features.norm$compagua <- 
  (1 - features.norm[,c("V012")]/features.norm[,c("V001")]) * (1/14)


# calcula a componente Pessoas não brancas e aplica o peso
features.norm$compPessoas <-
  (1 - rowSums(features.norm[, 
                             c("V003", "V004", "V005", "V006")])/features.norm[,c("V001p")]) * (1/14)


# soma todas as componentes para formar o IVC
features.norm$ivc <- rowSums(features.norm[, 
                                           c("compDomRenda","compEntorno","compDomiciliosMulher","comp2maisdomicilio","compbanheiro","compagua","compPessoas")])


ivc_df <- inner_join(features.norm, ubs, by=c("Cod_bairro"))

ivc_df$ivc <- ivc_df$ivc + ivc_df$faixa



ivc_df <- ivc_df %>%
  group_by(Cod_bairro, Nome_do_bairro) %>% 
  summarise_each(mean)

write.csv(ivc_df,"BaseFinalRJ_IPC.csv", row.names = FALSE)

ivc_df <- ivc_df %>%
  group_by(Cod_bairro, Nome_do_bairro) %>% 
  summarise(IPC = mean(ivc, na.rm = TRUE))