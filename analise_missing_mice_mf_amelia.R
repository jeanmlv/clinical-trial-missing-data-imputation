install.packages(c("mice", "missForest", "Amelia", "dplyr", "ggplot2", "VIM", "tidyr", "psych", "caret"))

# Criar diretórios
dir.create("R", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)
dir.create("output/imputacoes", recursive = TRUE)
dir.create("output/plots", recursive = TRUE)

# 01_carregar_dados.R
writeLines(
  'library(mice)
data(nhanes)
write.csv(nhanes, "data/dataset_com_missing.csv", row.names = FALSE)
df <- read.csv("data/dataset_com_missing.csv")',
  "R/01_carregar_dados.R"
)

# 02_explorar_missing.R
writeLines(
  'library(VIM)
library(dplyr)

aggr_plot <- aggr(df, col = c("navyblue","red"), numbers = TRUE, sortVars = TRUE,
                  labels = names(df), cex.axis = .7, gap = 3,
                  ylab = c("Missing data","Pattern"))

summary(df)',
  "R/02_explorar_missing.R"
)

# 03_imputacao_mice.R
writeLines(
  'library(mice)

imp_mice <- mice(df, m = 5, method = "pmm", seed = 123)
df_mice <- complete(imp_mice, 1)
write.csv(df_mice, "output/imputacoes/dados_imputados_mice.csv", row.names = FALSE)',
  "R/03_imputacao_mice.R"
)

# 04_imputacao_missForest.R
writeLines(
  'library(missForest)

set.seed(123)
imp_mf <- missForest(df)
df_mf <- imp_mf$ximp
write.csv(df_mf, "output/imputacoes/dados_imputados_missforest.csv", row.names = FALSE)',
  "R/04_imputacao_missForest.R"
)

# 05_imputacao_amelia.R
writeLines(
  'library(Amelia)

set.seed(123)
amelia_obj <- amelia(df, m = 1, idvars = NULL)
df_amelia <- amelia_obj$imputations[[1]]
write.csv(df_amelia, "output/imputacoes/dados_imputados_amelia.csv", row.names = FALSE)',
  "R/05_imputacao_amelia.R"
)

# 06_analise_comparativa.R
writeLines(
  'library(dplyr)

# Função de análise simples
analisar_cor <- function(data, metodo) {
  cor_val <- cor(data$bmi, data$age, use = "complete.obs")
  data.frame(metodo = metodo, cor = cor_val)
}

# Dados originais
orig <- analisar_cor(df, "Original")

# Imputações
df_mice <- read.csv("output/imputacoes/dados_imputados_mice.csv")
df_mf <- read.csv("output/imputacoes/dados_imputados_missforest.csv")
df_amelia <- read.csv("output/imputacoes/dados_imputados_amelia.csv")

mice_res <- analisar_cor(df_mice, "mice")
mf_res <- analisar_cor(df_mf, "missForest")
amelia_res <- analisar_cor(df_amelia, "amelia")

# Comparação
comparacao <- bind_rows(orig, mice_res, mf_res, amelia_res)
print(comparacao)',
"R/06_analise_comparativa.R"
)

# Executar os scripts na ordem correta
source("R/01_carregar_dados.R")
source("R/02_explorar_missing.R")
source("R/03_imputacao_mice.R")
source("R/04_imputacao_missForest.R")
source("R/05_imputacao_amelia.R")
source("R/06_analise_comparativa.R")

######################################### analise dos dados ausentes

# verificando o percentual de dados ausentes por variavel e resumo estatistico das variaveis
library(mice)
data(nhanes)
write.csv(nhanes, "data/dataset_com_missing.csv", row.names = FALSE)
df <- read.csv("data/dataset_com_missing.csv")

library(VIM)
library(dplyr)

aggr_plot <- aggr(df, col = c('navyblue','red'), numbers = TRUE, sortVars = TRUE,
                  labels = names(df), cex.axis = .7, gap = 3,
                  ylab = c("Missing data","Pattern"))

summary(df)

# imputação múltipla com o pacote mice
library(mice)

imp_mice <- mice(df, m = 5, method = 'pmm', seed = 123)
df_mice <- complete(imp_mice, 1)
write.csv(df_mice, "output/imputacoes/dados_imputados_mice.csv", row.names = FALSE)

# imputação com o pacote missForest

library(missForest)

set.seed(123)
imp_mf <- missForest(df)
df_mf <- imp_mf$ximp
write.csv(df_mf, "output/imputacoes/dados_imputados_missforest.csv", row.names = FALSE)

# imputação com o pacote Amelia

library(Amelia)

set.seed(123)
amelia_obj <- amelia(df, m = 1, idvars = NULL)
df_amelia <- amelia_obj$imputations[[1]]
write.csv(df_amelia, "output/imputacoes/dados_imputados_amelia.csv", row.names = FALSE)

library(dplyr)

# Função para rodar uma análise simples (correlação entre bmi e age)
analisar_cor <- function(data, metodo) {
  cor_val <- cor(data$bmi, data$age, use = "complete.obs")
  data.frame(metodo = metodo, cor = cor_val)
}

# Dados originais com missing
orig <- analisar_cor(df, "Original")

# Imputações
mice_res <- analisar_cor(df_mice, "mice")
mf_res <- analisar_cor(df_mf, "missForest")
amelia_res <- analisar_cor(df_amelia, "amelia")

# Comparação
comparacao <- bind_rows(orig, mice_res, mf_res, amelia_res)
print(comparacao)