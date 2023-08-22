# Analisis  y Comparacion entre los mejores tenistas argentinos : Juan Martin Del Potro - Guillermo Vilas - David Nalbandian


# Porcentage de semanas como Top 3 y Top 10 


library(ggplot2)


tennisData <- data.frame(
  jugador = c("Juan Martin Del Potro", "Guillermo Vilas", "David Nalbandian"),
  Top10 = c(105, 537, 58),
  Top3 = c(2, 52, 14)
)


getPercentage <- function(x) {
  (x / sum(x)) * 100
}


tennisData$Top10Pct <- getPercentage(tennisData$Top10)
tennisData$Top3Pct <- getPercentage(tennisData$Top3)


tennisData <- tennisData[order(tennisData$Top10), ]


colores <- c("#FF7F0E", "#1F77B4", "#33A02C")


p1 <- ggplot(data = tennisData, aes(x = "", y = Top10Pct, fill = jugador)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste(round(Top10Pct, 1), "%")), position = position_stack(vjust = 0.5), color = "white") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Porcentage de semanas como Top 10") +
  scale_fill_manual(values = colores) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(family = "Avenir", face = "bold"),
        panel.border = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

# Crear gráfico de torta para Top 3
p2 <- ggplot(data = tennisData, aes(x = "", y = Top3Pct, fill = jugador)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste(round(Top3Pct, 1), "%")), position = position_stack(vjust = 0.5), color = "white") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Porcentage de semanas como Top 3") +
  scale_fill_manual(values = colores) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(family = "Avenir", face = "bold"),
        panel.border = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))


gridExtra::grid.arrange(p1, p2, nrow = 1)









################################################################################################




# Victorias de los tenistas ante los top 10 de la epoca


#Del Potro

library(ggplot2)
library(forcats)

atpPerform <- data.frame(
  winner_name = c("Cilic", "Isner", "Federer", "Murray", "Nadal", "Djokovic", "Roddick", "Tsonga", "Berdych", "Ferrer"),
  loser_name = rep("Juan Martin Del Potro", 10),
  wins = c(11, 8, 7, 3, 6, 4, 4, 5, 5, 7),
  total_matches = c(12, 9, 25, 10, 17, 20, 5, 7, 9, 13)
)

atpPerform$win_prop <- atpPerform$wins / atpPerform$total_matches

groupedFedResults <- atpPerform %>%
  arrange(desc(win_prop), desc(total_matches)) %>%
  filter(total_matches >= 5) %>%
  top_n(10)

ggplot(data = groupedFedResults, aes(x = fct_reorder(winner_name, win_prop), y = win_prop)) +
  geom_point(aes(size = total_matches)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_size_continuous(limits = c(5, 25), breaks = c(5,7, 9, 10, 12, 13, 17, 20, 25)) +
  xlab("Player Name") +
  ylab("Win Proportion") +
  labs(size = "Matches Played") +
  ggtitle(" 10 jugadores con al menos 5 vs. Juan Martin Del Potro") +
  coord_flip() +
  theme_bw()

###########################################################


#NALBANDIAN

library(ggplot2)
library(forcats)

nalbandian <- data.frame(
  player_name = c("Gasquet", "Robredo", "Soderling", "Gonzalez", "Henman", "Berdych", "Ferrero", "Ljubicic", "Nadal", "Djokovic"),
  wins = c(7, 6, 6, 5, 5, 4, 4, 4, 2, 1),
  total_matches = c(7, 9, 7, 8, 6, 5, 7, 9, 7, 5)
)

nalbandian$win_prop <- nalbandian$wins / nalbandian$total_matches

groupedNalbandian <- nalbandian %>%
  arrange(desc(win_prop), desc(total_matches)) %>%
  filter(total_matches >= 5) %>%
  top_n(10)

ggplot(data = groupedNalbandian, aes(x = fct_reorder(player_name, win_prop), y = win_prop)) +
  geom_point(aes(size = total_matches)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_size_continuous(limits = c(5, 9), breaks = c(5, 6, 7, 8, 9)) +
  xlab("Player Name") +
  ylab("Win Proportion") +
  labs(size = "Matches Played") +
  ggtitle("10 jugadores con al menos 5 vs. David Nalbandian") +
  coord_flip() +
  theme_bw()

###########################################################


#Vilas

library(ggplot2)
library(forcats)


vilas <- data.frame(
  player_name = c("Clerc", "Noah", "Orantes", "Nastase", "Connors", "McEnroe", "Borg", "Ashe", "Tanner", "Wilander"),
  wins = c(10, 9, 7, 7, 4, 6, 5, 5, 3, 1),
  total_matches = c(14, 11, 15, 12, 9, 14, 22, 10, 7, 8)
)

vilas$win_prop <- vilas$wins / vilas$total_matches


groupedVilas <- vilas %>%
  arrange(desc(win_prop), desc(total_matches)) %>%
  filter(total_matches >= 5) %>%
  top_n(10)


ggplot(data = groupedVilas, aes(x = fct_reorder(player_name, win_prop), y = win_prop)) +
  geom_point(aes(size = total_matches)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_size_continuous(limits = c(5, 22), breaks = c(7,8,9,10,11,12,14,15,22)) +
  xlab("Player Name") +
  ylab("Win Proportion") +
  labs(size = "Matches Played") +
  ggtitle("10 jugadores con al menos 5 vs. Guillermo Vilas") +
  coord_flip() +
  theme_bw()






################################################################################################


# Medallas Olimpicas entre los tenistas


library(ggplot2)
library(grid)
library(jpeg)

medallas <- c(2, 0, 0)
nombres <- c("Del Potro", "Nalbandian", "Vilas")
imagen_del_potro <- "C:\\Users\\Usuario\\Pictures\\delpo-medallas.jpg"

data_medallas <- data.frame(Medallas = medallas, Nombres = nombres)

plot <- ggplot(data_medallas, aes(x = reorder(Nombres, -Medallas), y = Medallas)) +
  geom_bar(aes(fill = Medallas), stat = "identity", color = "black", show.legend = FALSE) +
  geom_text(aes(label = Medallas), size = 4, vjust = -0.5) +
  scale_fill_gradient(low = "paleturquoise", high = "paleturquoise4") +
  labs(title = "Medallas en Juegos Olímpicos",
       subtitle = "Comparación de medallas en Juegos Olímpicos de tenistas destacados",
       x = "Tenista", y = "Número de medallas") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank()) +
  ylim(0, 2)


img_del_potro <- readJPEG(imagen_del_potro)
plot <- plot +
  annotation_custom(rasterGrob(img_del_potro, interpolate = TRUE), 
                    xmin = 1 - 0.4, xmax = 1 + 0.4, ymin = 0, ymax = medallas[1])

plot






################################################################################################



# Partidos ganados a nivel ATP de los tenistas


library(ggplot2)
library(grid)
library(jpeg)


victorias_data <- data.frame(
  Tenistas = c("Guillermo Vilas", "Juan Martín Del Potro", "David Nalbandian"),
  Victorias = c(951, 439, 383),
  Imagenes = c("C:\\Users\\Usuario\\Pictures\\vilas.jpg",
               "C:\\Users\\Usuario\\Pictures\\delpo.jpg",
               "C:\\Users\\Usuario\\Pictures\\david.jpg")
)


plot <- ggplot(victorias_data, aes(x = reorder(Tenistas, -Victorias), y = Victorias)) +
  geom_bar(aes(fill = Victorias), stat = "identity", color = "black", show.legend = FALSE) +
  scale_fill_gradient(low = "paleturquoise", high = "paleturquoise4") +
  labs(title = "Victorias a nivel ATP",
       subtitle = "Número de victorias en la carrera ATP",
       x = "Tenista", y = "Número de Victorias") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ylim(0, 1000)


for (i in 1:nrow(victorias_data)) {
  img <- readJPEG(victorias_data$Imagenes[i])
  plot <- plot +
    annotation_custom(rasterGrob(img, interpolate = TRUE),
                      xmin = i - 0.4, xmax = i + 0.4, ymin = 0, ymax = victorias_data$Victorias[i]) +
    geom_label(aes(label = format(Victorias, big.mark = ",")), size = 5, vjust = -0.2,
               color = "black", fill = "white",
               label.padding = unit(0.5, "lines"),
               label.r = unit(0.2, "lines"),
               label.round = TRUE,
               label.color = "black",
               fontface = "bold")
}

plot







################################################################################################


library(tidyr)
library(ggplot2)


data <- data.frame(
  jugador = c("Guillermo Vilas", "David Nalbandian", "Juan Martin Del Potro"),
  VICTORIA = c(23, 12, 5),
  PERDIDOS = c(12, 11, 5)
)


meltedFifth <- reshape2::melt(data, id.vars = "jugador")


ggplot(data = meltedFifth, aes(x = jugador, y = value, fill = factor(variable, levels = c("PERDIDOS", "VICTORIA")))) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = value), position = position_fill(vjust = 0.5), fontface = "bold") +
  xlab("Jugador") +
  ylab("Proporción de Victorias/Derrotas") +
  ggtitle("Rendimiento en Quinto Set de Grand Slam por Jugador") +
  labs(fill = "Resultado") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", face = "bold"))



################################################################################################

#Partidos ganados de cada uno de los tenistas en Grand Slam

library(ggplot2)
library(dplyr)


data <- data.frame(
  player = c(rep("Juan Martin Del Potro", 4), rep("David Nalbandian", 4), rep("Guillermo Vilas", 4)),
  grand_slam = rep(c("Australia Open", "Roland Garros", "Wimbledon", "US Open"), 3),
  wins = c(19, 22, 35, 21, 26, 20, 21, 19, 23, 56, 43, 15)
)


data <- data %>% filter(wins > 0)


ggplot() +
  geom_bar(data = data, aes(x = player, y = wins, fill = factor(grand_slam, levels = c("Wimbledon", "US Open", "Roland Garros", "Australia Open"))), stat = "identity") +
  geom_label(data = data %>% group_by(player) %>% summarise(total_wins = sum(wins)),
             aes(x = player, y = total_wins, label = total_wins), vjust = -0.5) +
  geom_text(data = data, aes(x = player, y = wins, label = wins), position = position_stack(vjust = 0.2), hjust = 0.2) +
  xlab("Jugador") +
  ylab("Número de Partidos Ganados en Grand Slam") +
  labs(fill = "Nombre del Grand Slam") +
  ggtitle("Partidos Ganados en Grand Slam por Jugador") +
  theme_bw()




################################################################################################


#Titulos de cada uno de los tenistas

library(ggplot2)
library(dplyr)
library(tidyr)


titles <- data.frame(
  player = c("Juan Martin Del Potro", "David Nalbandian", "Guillermo Vilas"),
  grand_slam = c(1, 0, 4),
  atp_finals = c(0, 1, 1),
  atp_1000 = c(1, 1, 8),
  atp_500 = c(8, 2, 4),
  atp_250 = c(12, 6, 24),
  atp_challenger = c(0, 0, 21)
)


titles_stacked <- titles %>%
  pivot_longer(cols = -player, names_to = "titulo", values_to = "cantidad")


ggplot(data = titles_stacked, aes(x = player, y = cantidad, fill = titulo)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("grand_slam" = "blue", "atp_finals" = "green", 
                               "atp_1000" = "orange", "atp_500" = "red", 
                               "atp_250" = "purple", "atp_challenger" = "pink")) +
  xlab("Jugador") +
  ylab("Número de Títulos") +
  labs(fill = "Tipo de Título") +
  ggtitle("Títulos Ganados por Jugador") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

################################################################################################





























