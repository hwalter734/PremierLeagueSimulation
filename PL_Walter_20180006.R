library(dplyr)
library(readr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(devtools)


cronograma <- read_excel("plschedule.xlsx")
premier <- read_excel("dataset_premier.xlsx")

equipos <- c("Arsenal","Aston Villa", "Brighton", "Burnley",
             "Chelsea", "Crystal Palace", "Everton","Fulham",
             "Leeds United", "Leicester City", "Liverpool",
             "Man City", "Man United", "Newscastle", "Sheffield United",
             "Southampton", "Spurs", "West Brom", "West Ham", "Wolves")

temp_combinado <- subset(premier, select = c(HomeTeam:FTR))

resultados_home <- temp_combinado %>% filter(., FTR == "H") %>% group_by(HomeTeam) %>%
  summarise(victorias_casa = n())
resultados_away <- temp_combinado %>% filter(., FTR == "A") %>% group_by(AwayTeam) %>%
  summarise(victorias_visitante = n())
resultados_empate1 <- temp_combinado %>% filter(., FTR == "D") %>% group_by(AwayTeam) %>%
  summarise(empates1 = n())
resultados_empate2 <- temp_combinado %>% filter(., FTR == "D") %>% group_by(HomeTeam) %>%
  summarise(empates2 = n())
goles_home <- temp_combinado %>% group_by(HomeTeam) %>%
  summarise(hg = sum(FTHG))
goles_away <- temp_combinado %>%  group_by(HomeTeam) %>%
  summarise(ag = sum(FTAG))
past_results <- cbind(resultados_home,resultados_away,resultados_empate1, resultados_empate2,goles_home,goles_away)
past_results$puntos = (past_results$victorias_casa*3)+(past_results$victorias_visitante*3)
past_results$puntos = past_results$puntos + past_results$empates1 + past_results$empates2

df_temporadas <- past_results[,c(1,10,12,13)]
df_temporadas <- arrange(df_temporadas, -puntos)
colnames(df_temporadas) <- c("equipo", "goles_home_total","goles_away_total","puntos")

gamma <- sum(temp_combinado$FTHG)/sum(temp_combinado$FTAG)

modelo_basico1 <- temp_combinado %>%
  group_by(HomeTeam) %>% 
  summarise(goles_a_favor = mean(FTHG), goles_concedidos = mean(FTAG), gaf = sd(FTHG), gc = sd(FTAG))

modelo_basico2 <- temp_combinado %>%
  group_by(AwayTeam) %>% 
  summarise(goles_a_favor2 = mean(FTAG), goles_concedidos2 = mean(FTHG), gaf1 = sd(FTAG), gc1 = sd(FTHG)) 

modelo_basico <- cbind(modelo_basico1, modelo_basico2)
modelo_basico$alpha_prueba <- (modelo_basico$goles_a_favor + modelo_basico$goles_a_favor2)/2
modelo_basico$beta_prueba <- (modelo_basico$goles_concedidos + modelo_basico$goles_concedidos2)/2
modelo_basico$alpha_prueba_sd <- (modelo_basico$gaf+modelo_basico$gaf1)/2
modelo_basico$beta_prueba_sd <- (modelo_basico$gc+modelo_basico$gc1)/2
modelo_basico <- subset(modelo_basico, select = c(HomeTeam, alpha_prueba, beta_prueba, alpha_prueba_sd, beta_prueba_sd))

nombres_equipos <- sort(as.vector(unique(cronograma$`Home Team`)))
nombres <- sort(as.vector(unique(cronograma$`Home Team`)))
nombres <- data.frame(HomeTeam=nombres)
modelo_basico$HomeTeam <- mgsub::mgsub(modelo_basico$HomeTeam, c("Man United", "Sheffield United", "Tottenham"), 
                                       c("Man Utd", "Sheffield Utd", "Spurs"))

modelo_basico <- merge(x = modelo_basico, y = nombres, by.y = "HomeTeam")

#West Brom
wb <- read_excel("westbrom.xlsx")
wb <- wb %>% filter(Temporada %in% c(1617, 1819))
wb_p1 <- wb %>% group_by(Local) %>%
  summarise(goles_favor = mean(`GOL L`), goles_concedidos = mean(`GOL V`), gaf = sd(`GOL L`), gc = sd(`GOL V`))
wb_p2 <- wb %>% group_by(Visitante) %>%
  summarise(goles_favor1 = mean(`GOL V`), goles_concedidos2 = mean(`GOL L`), gaf1 = sd(`GOL V`), gc1 = sd(`GOL L`))
wb_data <- cbind(wb_p1, wb_p2)
resultado_wb <- wb_data[26, ]
alpha_wb <- (resultado_wb$goles_favor+resultado_wb$goles_favor1)/2
beta_wb <- (resultado_wb$goles_concedidos+resultado_wb$goles_concedidos2)/2
alpha_wb_sd <- (resultado_wb$gaf + resultado_wb$gaf1)/2
beta_wb_sd <- (resultado_wb$gc + resultado_wb$gc1)/2

modelo_basico <- modelo_basico %>% add_row(HomeTeam = "West Brom", alpha_prueba = alpha_wb, beta_prueba = beta_wb,
                                           alpha_prueba_sd = alpha_wb_sd, beta_prueba_sd = beta_wb_sd)

#Leeds
a_leeds <- mean(modelo_basico$alpha_prueba)*0.6
b_leeds <- mean(modelo_basico$beta_prueba)*0.6
a_leeds_sd <- mean(modelo_basico$alpha_prueba_sd)
b_leeds_sd <- mean(modelo_basico$beta_prueba_sd)
modelo_basico <- modelo_basico %>% add_row(HomeTeam = "Leeds", alpha_prueba = a_leeds, beta_prueba = b_leeds,
                                           alpha_prueba_sd = a_leeds_sd, beta_prueba_sd = b_leeds_sd)
modelo_basico <- arrange(modelo_basico, HomeTeam)
colnames(modelo_basico)[1] <- "Equipos"
modelo_limpio <- modelo_basico
modelo_limpio <- subset(modelo_limpio, select = c(1:3))
colnames(modelo_limpio) <- c("Equipos", "alpha","beta")

prediccion <- function(home, away, parametros){
  expected_goles_home <- as.numeric(parametros$alpha[home] * parametros$beta[away] * gamma)
  expected_goles_away <- as.numeric(parametros$alpha[away] * parametros$beta[home])
  
  df <- data.frame(home = home, away = away, goles_home = expected_goles_home, goles_away = expected_goles_away)
  return(df)
}

parametros_basicos <- modelo_limpio %>%
  select(-Equipos) %>%
  as.list() %>%
  lapply(., function(x){names(x) <- nombres_equipos;return(x)})

colnames(cronograma)[4] <- "home"
colnames(cronograma)[5] <- "away"
cronograma <- subset(cronograma, select = -c(Date, Location, Result))


goles_prediccion <- map2_df(cronograma$home, cronograma$away,
                            prediccion, parametros_basicos) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  print()

goles_prediccion$goles_home <- floor(goles_prediccion$goles_home)
goles_prediccion$goles_away <- floor(goles_prediccion$goles_away)

datos_compilados <- function(x){
  x %>%
    gather(localidad, equipo, -goles_home, -goles_away) %>%
    mutate(g_for = case_when(
      localidad == "home" ~ goles_home,
      localidad == "away" ~ goles_away
    )) %>%
    mutate(g_ag = case_when(
      localidad == "home" ~ goles_away,
      localidad == "away" ~ goles_home
    ))
}

scores <- function(x){
  x %>%
    datos_compilados(.) %>%
    mutate(puntos = case_when(
      g_for > g_ag ~ 3,
      g_ag > g_for~ 0,
      g_for == g_ag ~ 1
    )) %>%
    group_by(equipo) %>%
    summarise(juegos_jugados = n(),
              goles_total = sum(g_for),
              goles_concedidos_total = sum(g_ag),
              puntos = sum(puntos)) %>%
    arrange(-puntos)
}

final_scores <- goles_prediccion %>%
  scores(.)%>%
  print()

top10 <- head(final_scores, 10)
top10

#Segundo modelo de Predicción
pl_simulator <- function(modelo_basico, cronograma, n){

i = 0
ganadores = c()
for(i in 0:20) {
improved_model <- modelo_basico
improved_model$alpha <- rnorm(20, mean = improved_model$alpha_prueba, sd = improved_model$alpha_prueba_sd)
improved_model$beta <- rnorm(20, mean = improved_model$beta_prueba, sd = improved_model$beta_prueba_sd)
improved_model <- subset(improved_model, select = c(Equipos, alpha,beta))

prediccion <- function(home, away, parametros){
  expected_goles_home <- as.numeric(parametros$alpha[home] * parametros$beta[away] * gamma)
  expected_goles_away <- as.numeric(parametros$alpha[away] * parametros$beta[home])
  
  df <- data.frame(home = home, away = away, goles_home = expected_goles_home, goles_away = expected_goles_away)
  return(df)
}

parametros_basicos <- improved_model %>%
  select(-Equipos) %>%
  as.list() %>%
  lapply(., function(x){names(x) <- nombres_equipos;return(x)})

goles_prediccion <- map2_df(cronograma$home, cronograma$away,
                            prediccion, parametros_basicos) %>%
  mutate_if(is.numeric, round, digits = 2)

goles_prediccion$goles_home <- floor(goles_prediccion$goles_home)
goles_prediccion$goles_away <- floor(goles_prediccion$goles_away)

datos_compilados <- function(x){
  x %>%
    gather(localidad, equipo, -goles_home, -goles_away) %>%
    mutate(g_for = case_when(
      localidad == "home" ~ goles_home,
      localidad == "away" ~ goles_away
    )) %>%
    mutate(g_ag = case_when(
      localidad == "home" ~ goles_away,
      localidad == "away" ~ goles_home
    ))
}

scores <- function(x){
  x %>%
    datos_compilados(.) %>%
    mutate(puntos = case_when(
      g_for > g_ag ~ 3,
      g_ag > g_for~ 0,
      g_for == g_ag ~ 1
    )) %>%
    group_by(equipo) %>%
    summarise(juegos_jugados = n(),
              goles__total = sum(g_for),
              goles_concedidos_total = sum(g_ag),
              puntos = sum(puntos)) %>%
    arrange(-puntos)
}

final_scores <- goles_prediccion %>%
  scores(.)

winner <- as.character(final_scores[1,1])
ganadores = append(ganadores, winner)
i = i + 1
}

outcomes <- as.data.frame(table(ganadores))
outcomes <- arrange(outcomes, -Freq)
outcomes$Probabilidad_Ganar <- (outcomes$Freq/n)
outcomes <- outcomes[c(1:3), ]

return(outcomes)
}




