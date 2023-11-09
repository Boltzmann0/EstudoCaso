# Carregar pacotes
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)

# Importar os dados limpos
clean_trip_final <- read_csv("clean_trip_final.csv")
str(clean_trip_final)
names(clean_trip_final)

# Ordenar os dados
clean_trip_final$month <- ordered(clean_trip_final$month,
                                  levels = c("Nov_22", "Dez_22", "Jan_23", "Fev_23",
                                             "Mar_23", "Abr_23", "Mai_23", "Jun_23",
                                             "Jul_23", "Ago_23", "Set_23", "Out_23"))

clean_trip_final$week_day <- ordered(clean_trip_final$week_day,
                                     levels = c("Domingo", "Segunda", "Terça",
                                                "Quarta", "Quinta",
                                                "Sexta", "Sábado"))

# Análise: mínimo, máximo, mediana e média
View(summary(clean_trip_final$ride_length, fast = TRUE))

# Número total de clientes
View(table(clean_trip_final$costumer_type))

# Total de viagens para cada tipo de cliente em minutos
View(setNames(aggregate(ride_length ~ costumer_type, clean_trip_final, sum),
              c("Tipo de cliente", "Duração total da viagem (minutos)")))

# Diferenças entre membros e passageiros casuais em termos de duração da viagem
View(clean_trip_final %>% 
       group_by(costumer_type) %>% 
       summarise(min_length_mins = min(ride_length), max_length_min = max(ride_length),
       median_length_mins = median(ride_length), mean_length_min = mean(ride_length)))


# Duração média da viagem para usuários por dia da semana e número total de viagens por dia da semana
View(clean_trip_final %>% 
       group_by(week_day) %>% 
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()))

# Duração média da viagem por mês
View(clean_trip_final %>% 
       group_by(month) %>% 
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()))

# Comparação da duração média da viagem por cada dia da semana de acordo com cada tipo de cliente
View(aggregate(clean_trip_final$ride_length ~ clean_trip_final$costumer_type +
                 clean_trip_final$week_day, FUN = mean))

# Comparação da duração média da viagem por cada mês de acordo com cada tipo de cliente
View(aggregate(clean_trip_final$ride_length ~ clean_trip_final$costumer_type +
                 clean_trip_final$month, FUN = mean))

# Analisar dados de duração do passeio por tipo de cliente e dia da semana
View(clean_trip_final %>% 
       group_by(costumer_type, week_day) %>% 
       summarise(number_of_ride = n(),
                 avgerage_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))

# Analisar dados de duração do passeio por tipo de cliente e mês
View(clean_trip_final %>% 
       group_by(costumer_type, month) %>% 
       summarise(nummber_of_ride = n(),
                 average_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))

# Salvar os dados para visualização de dados
write.csv(clean_trip_final, file = "clean_trip_final_tableau.csv", row.names = FALSE)
