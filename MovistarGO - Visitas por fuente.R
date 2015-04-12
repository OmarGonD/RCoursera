library(RGA)
library(dplyr)
library(ggvis)


client.id1 <- "471075043173-gjnqf9ute7qevbnr3f6vj3k9pmn9kq16.apps.googleusercontent.com"
client.secret1 <- "lF1x5oPOlzB1emBsNz0PZYJ9"


authorize(client.id1,client.secret1, cache = F)

get_profiles()

idMovistarGo <- 71411233





#### ==== Visitas Septiembre ====



Visitas_Por_Fuente_Oct <- tbl_df(get_ga(idMovistarGo, start.date = "2014-01-01", end.date = "2014-10-31",
                                        dimensions = "ga:year,ga:month,ga:userType,ga:source,ga:Medium,ga:campaign",
                                        metrics = "ga:sessions", start = 1, max = NULL))





Visitas_Por_Fuente_Nov <- tbl_df(get_ga(idMovistarGo, start.date = "2014-11-01", end.date = "2014-11-30",
                                     dimensions = "ga:year,ga:month,ga:userType,ga:source,ga:Medium,ga:campaign",
                                     metrics = "ga:sessions", start = 1, max = NULL))




Visitas_Por_Fuente_Dic <- tbl_df(get_ga(idMovistarGo, start.date = "2014-12-01", end.date = "2014-12-31",
                                        dimensions = "ga:year,ga:month,ga:userType,ga:source,ga:Medium,ga:campaign",
                                        metrics = "ga:sessions", start = 1, max = NULL))



Visitas_Por_Fuente_Ene <- tbl_df(get_ga(idMovistarGo, start.date = "2015-01-01", end.date = "2015-01-31",
                                        dimensions = "ga:year,ga:month,ga:userType,ga:source,ga:Medium,ga:campaign",
                                        metrics = "ga:sessions", start = 1, max = NULL))



Visitas_Por_Fuente_Feb <- tbl_df(get_ga(idMovistarGo, start.date = "2015-02-01", end.date = "2015-02-28",
                                        dimensions = "ga:year,ga:month,ga:userType,ga:source,ga:Medium,ga:campaign",
                                        metrics = "ga:sessions", start = 1, max = NULL))




Visitas_Por_Fuente_Mar <- tbl_df(get_ga(idMovistarGo, start.date = "2015-03-01", end.date = "2015-03-14",
                                        dimensions = "ga:year,ga:month,ga:userType,ga:source,ga:Medium,ga:campaign",
                                        metrics = "ga:sessions", start = 1, max = NULL))







Visitas_Por_Fuente <- rbind(Visitas_Por_Fuente_Oct,Visitas_Por_Fuente_Nov,
                            Visitas_Por_Fuente_Dic,
                            Visitas_Por_Fuente_Ene,
                            Visitas_Por_Fuente_Feb,
                            Visitas_Por_Fuente_Mar)

View(Visitas_Por_Fuente)






Visitas_Por_Fuente <- Visitas_Por_Fuente %>%
                           mutate(Campanas1 = NA,
                           Campanas1 = ifelse(grepl("\\(direct\\)", source), "Directo", Campanas1),
                           Campanas1 = ifelse(grepl("referral", Medium), "Referencias", Campanas1),
                           Campanas1 = ifelse(grepl(".*mail.*", Medium), "Email", Campanas1),
                           Campanas1 = ifelse(grepl("social-media", Medium), "Social Media", Campanas1),
                           Campanas1 = ifelse(grepl(".*face.*", source), "Social Media", Campanas1),
                           Campanas1 = ifelse(grepl(".*paginas.*", source), "Campañas", Campanas1),
                           Campanas1 = ifelse(grepl("organic", Medium), "SEO", Campanas1),
                           Campanas1 = ifelse(grepl(".*mail.*", source), "Email", Campanas1),
                           Campanas1 = ifelse(grepl(".*tutv.*", source), "Campañas", Campanas1),
                           Campanas1 = ifelse(grepl("google", source)
                                              & grepl("cpc", Medium), "Adwords", Campanas1),
                           Campanas1 = ifelse(grepl("youtube", source)
                                              & grepl("cpv", Medium), "Adwords", Campanas1)) %>%
                           
                           ## Inicia columna Campañas2 ##
                           mutate(Campanas2 = NA,
                           Campanas2 = ifelse(!grepl(".*mail.*", source)
                                              & grepl("referral", Medium), "Otras referencias", Campanas2),
                           Campanas2 = ifelse(grepl(".*movistar.*", source), "Movistar Content", Campanas2),
                           Campanas2 = ifelse(grepl(".*telefonica.*", source), "Telefónica", Campanas2),
                           Campanas2 = ifelse(grepl(".*diario.*", source) | grepl(".*comercio.*", source) | grepl(".*prensa.*", source), "Diarios Online", Campanas2),
                           Campanas2 = ifelse(grepl(".*espn.*", source), "ESPN", Campanas2),
                           Campanas2 = ifelse(grepl(".*terra.*", source), "Terra", Campanas2),
                           Campanas2 = ifelse(grepl("social-media", Medium), "Social Media", Campanas2),
                           Campanas2 = ifelse(grepl(".*paginas.*", source), "Campañas", Campanas2),
                           Campanas2 = ifelse(grepl(".*face.*", source), "Social Media", Campanas2),
                           Campanas2 = ifelse(grepl("organic", Medium), "SEO", Campanas2),
                           Campanas2 = ifelse(grepl(".*tutv.*", source), "Campañas", Campanas2),
                           Campanas2 = ifelse(grepl(".*mail.*", source) | grepl(".*mail.*", Medium), "Email", Campanas2),
                           Campanas2 = ifelse(grepl("youtube", source)
                                              & grepl("cpv", Medium), "Adwords", Campanas2),
                           Campanas2 = ifelse(grepl("\\(direct\\)", source), "Directo", Campanas2),
                           Campanas2 = ifelse(grepl("google", source)
                                              & grepl("cpc", Medium), "Adwords", Campanas2),
                           Campanas2 = ifelse(grepl("youtube", source)
                                              & grepl("cpv", Medium), "Adwords", Campanas2)) %>%
                           rename(Fuentes = Campanas1)
                           


Visitas_Por_Fuente$month <- gsub("^1$", "Enero", Visitas_Por_Fuente$month)
Visitas_Por_Fuente$month <- gsub("^2$", "Febrero", Visitas_Por_Fuente$month)
Visitas_Por_Fuente$month <- gsub("^3$", "Marzo", Visitas_Por_Fuente$month)
Visitas_Por_Fuente$month <- gsub("^4$", "Abril", Visitas_Por_Fuente$month)
Visitas_Por_Fuente$month <- gsub("^5$", "Mayo", Visitas_Por_Fuente$month)
Visitas_Por_Fuente$month <- gsub("^6$", "Junio", Visitas_Por_Fuente$month)
Visitas_Por_Fuente$month <- gsub("^7$", "Julio", Visitas_Por_Fuente$month)
Visitas_Por_Fuente$month <- gsub("^8$", "Agosto", Visitas_Por_Fuente$month)
Visitas_Por_Fuente$month <- gsub("^9$", "Setiembre", Visitas_Por_Fuente$month)
Visitas_Por_Fuente$month <- gsub("^10$", "Octubre", Visitas_Por_Fuente$month)
Visitas_Por_Fuente$month <- gsub("^11$", "Noviembre", Visitas_Por_Fuente$month)
Visitas_Por_Fuente$month <- gsub("^12$", "Diciembre", Visitas_Por_Fuente$month)




View(tail(Visitas_Por_Fuente))
str(Visitas_Por_Fuente)


Visitas_Por_Fuente$month <- factor(Visitas_Por_Fuente$month,
                                 levels = c("Enero", "Febrero",
                                          "Marzo", "Abril", "Mayo",
                                      "Junio", "Julio",
                                  "Agosto", "Setiembre","Octubre",
                                  "Noviembre",
                                            "Diciembre"), ordered =T)





Visitas_Por_Fuente <- Visitas_Por_Fuente %>%
                      group_by(year,month, Fuentes) %>%
                      rename(Mes = month, Ano = year) %>%  
                      summarise(Visitas = sum(sessions))

View(Visitas_Por_Fuente)



write.csv(Visitas_Por_Fuente, "OmarSesiones_1415.csv", row.names = F)

getwd()
setwd("D:\\Omar\\OmarApp")

################################################################################
################################################################################








Visitas_Por_Fuente$Mes <- factor(Visitas_Por_Fuente$Mes)


Visitas_Por_Fuente$Dia_de_la_semana <- factor(Visitas_Por_Fuente$Dia_de_la_semana,
                                              levels = c("Monday","Tuesday", "Wednesday",
                                                         "Thrusday", "Friday","Saturday",
                                                         "Sunday"), ordered = T)
Visitas_Por_Fuente$Mes <- factor(Visitas_Por_Fuente$Mes,
                                 levels = c("09", "10","11"), ordered =T)







str(Visitas_Por_Fuente)








write.csv(Visitas_Por_Fuente, "Visitas_Por_Fuente_NOV.csv", row.names = F)
