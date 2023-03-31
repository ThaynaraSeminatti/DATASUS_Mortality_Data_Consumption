
### instalando o pacote dos microdados
### install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)
library(dplyr)


ano_inicio = 2011
ano_fim = 2011
anos_interesse = 2011:2021
vars = c("UF_ZI", "SEXO", "IDADE", "CID_MORTE")
ufs_brasil <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
                "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")


dados = fetch_datasus(year_start = ano_inicio, month_start = 1,
                      year_end = ano_fim, month_end = 11, vars = vars,
                      information_system = "SIH-RD")


#head(dados)

#vars = data.frame(colnames(dados))

dados = fetch_datasus(year_start = ano_inicio, month_start = 1,
                      year_end = ano_fim, month_end = 11, vars = vars,
                      information_system = "SIH-RD")
dados$categorias_idade <- cut(dados$IDADE, breaks = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80, Inf))
dados$UF<- ufs_brasil[1]

#1- Mortalidade por idade e sexo ----

mort_idade_sexo = dados %>% 
  select(categorias_idade, SEXO) %>% 
  mutate(SEXO_TRAT = if_else(condition = SEXO == 1,true =  'MASCULINO',false = 'FEMININO')) %>% 
  group_by(categorias_idade, SEXO_TRAT) %>% 
  count()

#2. Mortalidade por idade, sexo e região -----

anos_interesse = c(2011)
#vars = c( "SEXO", "IDADE")
ufs_brasil <- c("TO")
resultado <- list()



#lista<-list()
ufs_brasil = c("SC", "SP", "SE", "TO")


for ( i in 1:length(ufs_brasil)) {
  resultado_interno <- list()
  for (j in 1:length(anos_interesse)) {
    
    dados = fetch_datasus(year_start = anos_interesse[j], month_start = 1,
                          year_end = anos_interesse[j]+1, month_end = 1, vars = vars,uf = ufs_brasil[i],
                          information_system = "SIH-RD")
    
    dados$categorias_idade = cut(dados$IDADE, breaks = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80, Inf))
    
    mort_idade_sexo_reg = dados %>% 
      select(categorias_idade, SEXO) %>% 
      mutate(SEXO_TRAT = if_else(condition = SEXO == 1,true =  'MASCULINO',false = 'FEMININO')) %>% 
      group_by(categorias_idade, SEXO_TRAT) %>% 
      count()
    
    mort_idade_sexo_reg$UF = ufs_brasil[i]
    
    mort_idade_sexo_reg$ANO = anos_interesse[j]
    
    resultado_interno[[j]] <- data.frame(mort_idade_sexo_reg)
    
  }
  resultado[[i]] <- resultado_interno
}

### df_concatenado <- do.call(rbind, resultado)

ufs_brasil <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
                "RJ", "RN", "RS", "RO", "RR")
anos_interesse<- 2011:2021

###### salvando os arquivos
for (j in 1:length(ufs_brasil)) {
  filename<-""
  for (i in 1:length(anos_interesse)) {
  filename <- paste0("datas/mortalidade_sexo_ano_uf/",ufs_brasil[j],"_mortalidade_idade_sex_reg_",anos_interesse[i], ".csv")
  write.csv(resultado[[j]][[i]], filename, row.names = FALSE)
  }
}



#3. mortalidade por idade, sexo, região e causas-----
