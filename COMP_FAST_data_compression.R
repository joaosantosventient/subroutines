# START -------------------------------------------------------------------
rm(list=ls())
options(digits.secs=3)

# FOLDERS -------------------------------------------------------------------
directoria_subroutines <- "//de-noe-comp1/shm/routines/subroutines"


# SUBROUTINES -----------------------------------------------
source(file.path(directoria_subroutines,"subset modal.R"))
source(file.path(directoria_subroutines,"transduction.R"))
source(file.path(directoria_subroutines,"data_compression.R"))

# LIBRARIES -----------------------------------------------
library(dplyr); library(plyr); library(pracma); library(signal); library(cluster); library(fpc); library(e1071)


# INPUT -------------------------------------------------------------------

# ***25 abril -------------------------------------------------------------
nome_estrutura <- "25 abril" #nome da pasta
rate_compressed_COMP <- 50 #Hz -> frequencia final dos dados comprimidos
low_pass_freq <- 25 #frequencia passa-baixo de filtagrem dos dados COMP
rate_compressed_FAST <- 5 #Hz -> frequencia final dos dados comprimidos
grandezas_analise_estatica <- c("S","T","e","c","w")
grandezas_analise_dinamica <- c("a","p")
periodo_nao_comprimido <- 0#3                               # anos | So comprime ficheiro com mais de X anos

data_compression(RUN = T,
                 nome_estrutura = nome_estrutura,
                 rate_compressed_COMP = rate_compressed_COMP,
                 low_pass_freq = low_pass_freq,
                 rate_compressed_FAST = rate_compressed_FAST,
                 grandezas_analise_estatica = grandezas_analise_estatica,
                 grandezas_analise_dinamica = grandezas_analise_dinamica,
                 periodo_nao_comprimido = periodo_nao_comprimido)


# ***Sado -------------------------------------------------------------
nome_estrutura <- "sado - fase 2" #nome da pasta
rate_compressed_COMP <- 50 #Hz -> frequencia final dos dados comprimidos
low_pass_freq <- 25 #frequencia passa-baixo de filtagrem dos dados COMP
rate_compressed_FAST <- 5 #Hz -> frequencia final dos dados comprimidos
grandezas_analise_estatica <- c("S","T","e","c","w")
grandezas_analise_dinamica <- c("a","p")
periodo_nao_comprimido <- 1#3 # anos |So comprime ficheiro com mais de 3 anos

data_compression(RUN = F,
                 nome_estrutura = nome_estrutura,
                 rate_compressed_COMP = rate_compressed_COMP,
                 low_pass_freq = low_pass_freq,
                 rate_compressed_FAST = rate_compressed_FAST,
                 grandezas_analise_estatica = grandezas_analise_estatica,
                 grandezas_analise_dinamica = grandezas_analise_dinamica,
                 periodo_nao_comprimido = periodo_nao_comprimido)








