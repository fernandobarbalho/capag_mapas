library(patchwork)



##Análise dos missing values

# Load necessary libraries
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Load the data
# Create a binary target column for missing values in Indicador_3
dados_capag_2022$Indicador_3_Missing <- as.integer(is.na(dados_capag_2022$indicador_3))

# Prepare the dataset
# Drop the Indicador_3 and Município columns
data_tree_view <- dados_capag_2022[, (names(dados_capag_2022) %in% c("uf", "populacao", "nota_1", "nota_2", "Indicador_3_Missing"))]



# Generate the decision tree
fit <- rpart(Indicador_3_Missing ~ ., data=data_tree_view, method="class")

# Plot the decision tree
rpart.plot(fit, extra=101)



library(stats)


# Perform logistic regression
model <- glm(Indicador_3_Missing ~ ., data=data_tree_view, family=binomial())

# View the summary of the model
summary(model)

# Example coefficients (replace these values with your own)
coefficients <- c(
  ufAL=1.084, ufAM=0.7016, ufAP=0.2323, ufBA=0.1697, ufCE=0.1394,
  ufES=-2.424, ufGO=1.355, ufMA=0.8154, ufMG=0.9493, ufMS=0.2023,
  ufMT=0.1854, ufPA=2.318, ufPB=0.269, ufPE=0.4115, ufPI=0.278,
  ufPR=0.1065, ufRJ=1.206, ufRN=-0.3822, ufRO=-0.4282, ufRR=-0.8339,
  ufRS=-0.7683, ufSC=-0.7709, ufSE=-0.003936, ufSP=1.144, ufTO=1.488,
  populacao=-0.000008859, nota_1B=0.1037, nota_1C=0.3842, nota_2B=-0.05007,
  nota_2C=0.3119, nota_2n.d.=-0.3219
)

# Create a data frame of the coefficients for easier plotting
coeff_df <- data.frame(Variable = names(coefficients), Coefficient = as.numeric(coefficients))
coeff_df <- coeff_df[order(coeff_df$Coefficient),]

# Plotting
library(ggplot2)

ggplot(coeff_df, aes(x=Variable, y=Coefficient)) +
  geom_col(aes(fill=(Coefficient > 0)), show.legend=F) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  coord_flip() +
  theme_minimal() +
  labs(
    title="Coefficient Plot for Logistic Regression Analysis",
    x="Variable",
    y="Coefficient Value"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




dados_capag_2022 %>%
  mutate(indicador_3_ausente =as.factor(is.na(indicador_3)) ) %>%
  mutate(municipio_menor_63k= ifelse(populacao>=68000,"maior que 68 mil habitantes", "menor que 68 mil habitantes")) %>%
  summarise(.by = c(Indicador_3_Missing, municipio_menor_63k),
            numero_municipios = n())

#@@#Análise dos indicadores



filter_outliers <- function(.data, column_name, type="E") {
  # Ensure column_name exists in the .data
  if (!(column_name %in% colnames(.data))) {
    stop(paste("Column", column_name, "not found in the dataframe."))
  }

  # Extract the column as a vector
  column_data <- .data[[column_name]]

  # Calculate IQR and outlier boundaries
  Q1 <- quantile(column_data, 0.25, na.rm = TRUE)
  Q3 <- quantile(column_data, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value


  if (type=="E"){
    # Exclude data based on outlier boundaries
    filtered_data <- .data %>% filter(!!sym(column_name) > lower_bound & !!sym(column_name) < upper_bound)

  } else  if (type =="U"){
    # Maintain data based on upper boundaries
    filtered_data <- .data %>% filter(!!sym(column_name) >= upper_bound)
  }else  if (type =="L"){
    # Maintain data based on lower boundaries
    filtered_data <- .data %>% filter(!!sym(column_name) <= lower_bound)
  }else  if (type =="B"){
    # Maintain data based on both boundaries
    filtered_data <- .data %>% filter(!(!!sym(column_name) > lower_bound & !!sym(column_name) < upper_bound))
  }



  return(filtered_data)
}



gera_box_plot_indicador<- function(.data, a_indicador, escala_log=FALSE){

  indicador<- str_c("indicador_",a_indicador)
  nota<- str_c("nota_",a_indicador)


  nota_final<-
    .data %>%
    ggplot() +
    geom_boxplot(aes(x=capag_oficial, y = !!sym(indicador)))


  nota_indicador<-
    .data %>%
    ggplot() +
    geom_boxplot(aes(x=!!sym(nota), y = !!sym(indicador)))

  if (escala_log){
    nota_final <-
      nota_final +
      scale_y_log10()

    nota_indicador <-
      nota_indicador +
      scale_y_log10()
  }



  nota_final + nota_indicador


}

#Identificação dos limites das notas por categoria

seed_data<-

dados_capag_2022 %>%
  pivot_longer(cols = c(nota_1, nota_2, nota_3), names_to = "tipo_nota", values_to = "nota")


seed_data %>%
  filter(tipo_nota == "nota_1") %>%
  filter(nota !="n.d.") %>%
  select(-c(indicador_2,indicador_3)) %>%
  rename(indicador  = indicador_1) %>%
  bind_rows(
    seed_data %>%
      filter(tipo_nota == "nota_2") %>%
      filter(nota !="n.d.") %>%
      select(-c(indicador_1,indicador_3))%>%
      rename(indicador  = indicador_2)) %>%
  bind_rows(
    seed_data %>%
      filter(tipo_nota == "nota_3") %>%
      filter(nota !="n.d.") %>%
      select(-c(indicador_1,indicador_2))%>%
      rename(indicador  = indicador_3)) %>%
  summarise(.by = c(tipo_nota,nota),
            valor_min = min(indicador, na.rm = TRUE),
            valor_max = max(indicador, na.rm = TRUE))






#Box plot dos dados considerando todos os dados

dados_capag_2022 %>%
  #filter_outliers("indicador_1") %>%
  filter(indicador_1 <10) %>%
  gera_box_plot_indicador("1")


dados_capag_2022 %>%
  filter_outliers("indicador_2") %>%
  gera_box_plot_indicador("2")


dados_capag_2022 %>%
  #filter_outliers("indicador_3") %>%
  filter(indicador_3 <10) %>%
  gera_box_plot_indicador("3")





#Box plot dos dados com tratamento de outliers

#Box-plot dos extremos superiores
dados_capag_2022 %>%
  filter_outliers("indicador_1", type="U") %>%
  gera_box_plot_indicador("1")


#Box plot sem outliers
dados_capag_2022 %>%
  filter_outliers("indicador_1", type="E") %>%
  gera_box_plot_indicador("1")


dados_capag_2022 %>%
  filter_outliers("indicador_2") %>%
  gera_box_plot_indicador("2")


dados_capag_2022 %>%
  filter_outliers("indicador_3") %>%
  gera_box_plot_indicador("3")


#Lsta dos dados considerando os outliers superiores

out_sup_1<-
dados_capag_2022 %>%
  filter_outliers("indicador_1", type="U")


out_sup_2<-
  dados_capag_2022 %>%
  filter_outliers("indicador_2", type="U")


out_sup_3<-
  dados_capag_2022 %>%
  filter_outliers("indicador_3", type="U")



gera_box_plot_indicador(dados_capag_2022, "1", limite_max = 10)
gera_box_plot_indicador("2", limite_max = 10^20)
gera_box_plot_indicador("3", limite_max = 1000)


nota_final_2<-
  dados_capag_2022 %>%
  ggplot() +
  geom_boxplot(aes(x=capag_oficial, y = indicador_2))

nota_indicador_2<-
  dados_capag_2022 %>%
  ggplot() +
  geom_boxplot(aes(x=nota_1, y = indicador_2))

nota_final_2 + nota_indicador_2



### Testes de chi quadrado

teste_chi<-
chisq.test(dados_capag_2022$uf, dados_capag_2022$capag_oficial, simulate.p.value = TRUE)


residuo_teste<-
as.tibble(teste_chi[["stdres"]])

names(residuo_teste) <- c("uf","capag_oficial","n")


residuo_teste %>%
  filter(capag_oficial == "A") %>%
  mutate(uf = reorder(uf, n)) %>%
  ggplot(aes(x=n,y=uf))+
  geom_col()


residuo_teste %>%
  filter(capag_oficial == "B") %>%
  mutate(uf = reorder(uf, n)) %>%
  ggplot(aes(x=n,y=uf))+
  geom_col()

residuo_teste %>%
  filter(capag_oficial == "C") %>%
  mutate(uf = reorder(uf, n)) %>%
  ggplot(aes(x=n,y=uf))+
  geom_col()


residuo_teste %>%
  filter(capag_oficial == "D") %>%
  mutate(uf = reorder(uf, n)) %>%
  ggplot(aes(x=n,y=uf))+
  geom_col()


residuo_teste %>%
  filter(capag_oficial == "n.d.") %>%
  mutate(uf = reorder(uf, n)) %>%
  ggplot(aes(x=n,y=uf))+
  geom_col()



residuo_teste %>%
  mutate(rank = rank(-abs(n))) %>%
  filter(rank<=10) %>%
  mutate(ordem = as.factor(rank)) %>%
  mutate(ordem = fct_reorder(ordem, rank, .desc=TRUE) ) %>%
  ggplot(aes(x=n,y=ordem, fill= capag_oficial))+
  geom_col() +
  geom_text(aes(label= uf)) +
  geom_vline(xintercept = c(-2,2))


####Análises com REGIC


capag_regic<-
  dados_capag_2022 %>%
  rename(cod_cidade= cod_ibge) %>%
  left_join(regic_trabalho) %>%
  mutate(nivel_hierarquia = ifelse(is.na(nivel_hierarquia),0,nivel_hierarquia),
         nome_nivel_hierarquia = ifelse(is.na(nome_nivel_hierarquia),"Arranjo populacional", nome_nivel_hierarquia))

capag_regic_trabalho<-
capag_regic %>%
  mutate(nivel_hierarquia = ifelse(str_sub(nivel_hierarquia,1,1)=="1","1",nivel_hierarquia),
         nome_nivel_hierarquia = ifelse(nivel_hierarquia == "1", "Metrópole",nome_nivel_hierarquia ),
         nh = paste(nivel_hierarquia, nome_nivel_hierarquia, sep = " - "))


#teste chi_quadrado

teste_chi_capag_regic<-
chisq.test(capag_regic_trabalho$nh, capag_regic_trabalho$capag_oficial,  simulate.p.value = TRUE)


residuo_teste_capag_regic<-
  as_tibble(teste_chi_capag_regic[["stdres"]], .name_repair= make.names)

names(residuo_teste_capag_regic) <- c("nivel_hierarquico","capag_oficial","n")


residuo_teste_capag_regic %>%
  filter(capag_oficial == "A") %>%
  mutate(nivel_hierarquico = reorder(nivel_hierarquico, n)) %>%
  ggplot(aes(x=n,y=nivel_hierarquico))+
  geom_col() +
  geom_vline(xintercept = c(-2,2), color = 'red')


residuo_teste_capag_regic %>%
  filter(capag_oficial == "B") %>%
  mutate(nivel_hierarquico = reorder(nivel_hierarquico, n)) %>%
  ggplot(aes(x=n,y=nivel_hierarquico))+
  geom_col()+
  geom_vline(xintercept = c(-2,2), color = 'red')


residuo_teste_capag_regic %>%
  filter(capag_oficial == "C") %>%
  mutate(nivel_hierarquico = reorder(nivel_hierarquico, n)) %>%
  ggplot(aes(x=n,y=nivel_hierarquico))+
  geom_col()+
  geom_vline(xintercept = c(-2,2), color = 'red')


residuo_teste_capag_regic %>%
  filter(capag_oficial == "n.d.") %>%
  mutate(nivel_hierarquico = reorder(nivel_hierarquico, n)) %>%
  ggplot(aes(x=n,y=nivel_hierarquico))+
  geom_col()+
  geom_vline(xintercept = c(-2,2), color = 'red')



residuo_teste_capag_regic %>%
  mutate(rank = rank(-abs(n))) %>%
  filter(rank<=10) %>%
  mutate(ordem = as.factor(rank)) %>%
  mutate(ordem = fct_reorder(ordem, rank, .desc=TRUE) ) %>%
  ggplot(aes(x=n,y=ordem, fill= capag_oficial))+
  geom_col() +
  geom_text(aes(label= nivel_hierarquico))+
  geom_vline(xintercept = c(-2,2), color = 'red')



capag_regic_trabalho %>%
  ggplot() +
  geom_bar(aes(y=nh, fill= capag_oficial), position = "fill")


capag_regic_trabalho %>%
  ggplot() +
  geom_bar(aes(y=nh, fill= capag_oficial))


capag_regic_trabalho %>%
  ggplot() +
  geom_bar(aes(y=capag_oficial))



###########Análise de effect size

library(vcd)

# Example data
data_test <- matrix(c(10, 20, 30, 40), nrow = 2)

# Chi-square test
chi2 <- chisq.test(data_test)

# Cramér's V
assocstats(data_test)$cramer


teste_chi_capag_regic<-
  chisq.test(capag_regic_trabalho$nh, capag_regic_trabalho$capag_oficial,  simulate.p.value = TRUE)

assocstats(as.matrix(teste_chi_capag_regic[["observed"]]) )

assocstats(as.matrix(teste_chi[["observed"]]))$cramer

k<- NROW(as.matrix(teste_chi[["observed"]]))
r<- NCOL(as.matrix(teste_chi[["observed"]]))
n<- NROW(dados_capag_2022)

sqrt(teste_chi$statistic/(n*min(c(k-1,r-1))))


#####
# Estatísticas descritivas

names(dados_capag_2022)
