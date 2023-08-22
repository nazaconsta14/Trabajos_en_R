#Comparacion de Los Mejores Pesos Pesados de la Historia en : Alcance - Estatura - Peso

library(ggplot2)

boxers <- data.frame(
  Boxeador = c("Muhammad Ali", "Mike Tyson", "Rocky Marciano", "Joe Louis"),
  Peso = c(107, 100, 85, 97),
  Estatura = c(191, 178, 179, 187),
  Alcance = c(198, 180, 173, 193)
)

boxers <- boxers[order(boxers$Estatura, decreasing = TRUE), ] 

ggplot(boxers, aes(x = Boxeador, y = Estatura, color = Boxeador)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Boxeador, xend = Boxeador, y = 0, yend = Estatura)) +
  labs(title = "Comparación de Estatura de los Boxeadores", x = "", y = "Estatura (cm)") +
  geom_text(aes(label = Estatura), hjust = -1, size = 3) +
  theme(legend.position = "none", plot.title = element_text(size = 15, hjust = -0.1),
        axis.text.x = element_text(angle = 45, vjust = 0.4)) +
  ylim(0, max(boxers$Estatura))



library(ggplot2)

boxers <- data.frame(
  Boxeador = c("Muhammad Ali", "Mike Tyson", "Rocky Marciano", "Joe Louis"),
  Peso = c(107, 100, 85, 97),
  Estatura = c(191, 178, 179, 187),
  Alcance = c(198, 180, 173, 193)
)

boxers <- boxers[order(boxers$Peso, decreasing = TRUE), ] 

ggplot(boxers, aes(x = Boxeador, y = Peso, color = Boxeador)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Boxeador, xend = Boxeador, y = 0, yend = Peso)) +
  labs(title = "Comparación de Peso de los Mejores Peso Pesado", x = "", y = "Peso (kg)") +
  geom_text(aes(label = Peso), hjust = -1, size = 3) +
  theme(legend.position = "none", plot.title = element_text(size = 15, hjust = -0.1),
        axis.text.x = element_text(angle = 45, vjust = 0.4)) +
  ylim(0, max(boxers$Peso))





library(ggplot2)

boxers <- data.frame(
  Boxeador = c("Muhammad Ali", "Mike Tyson", "Rocky Marciano", "Joe Louis"),
  Peso = c(107, 100, 85, 97),
  Estatura = c(191, 178, 179, 187),
  Alcance = c(198, 180, 173, 193)
)

boxers <- boxers[order(boxers$Alcance, decreasing = TRUE), ] 

ggplot(boxers, aes(x = Boxeador, y = Alcance, color = Boxeador)) +
  geom_point(size = 3) +
  geom_segment(aes(x = Boxeador, xend = Boxeador, y = 0, yend = Alcance)) +
  labs(title = "Comparación del Alcance de los Mejores Peso Pesado", x = "", y = "Alcance (cm)") +
  geom_text(aes(label = Alcance), hjust = -1, size = 3) +
  theme(legend.position = "none", plot.title = element_text(size = 15, hjust = -0.1),
        axis.text.x = element_text(angle = 45, vjust = 0.4)) +
  ylim(0, max(boxers$Alcance))

####################################################################################

#Peleas Anuladas de los Mejores Pesos Pesados de la Historia



rachas_anuladas <- data.frame(boxeador = c("Muhammad Ali", "Mike Tyson", "Rocky Marciano", "Joe Louis"),
                              rachas = c(length(rle(ali$Resultado)$lengths[ali$Resultado == "Anulada"]),
                                         length(rle(tyson$Resultado)$lengths[tyson$Resultado == "Anulada"]),
                                         length(rle(marciano$Resultado)$lengths[marciano$Resultado == "Anulada"]),
                                         length(rle(louis$Resultado)$lengths[louis$Resultado == "Anulada"])))


rachas_anuladas <- rachas_anuladas[order(-rachas_anuladas$rachas), ]


library(ggplot2)


my_colors <- c("#FF7F00", "#4DAF4A", "#984EA3", "#E41A1C")

ggplot(rachas_anuladas, aes(x = boxeador, y = rachas, fill = boxeador)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = rachas), vjust = -0.5, size = 5, fontface = "bold", color = "white", show.legend = FALSE) +
  geom_label(aes(label = rachas), fill = "white", color = "black", size = 5, fontface = "bold", show.legend = FALSE) +
  scale_fill_manual(values = my_colors) +
  xlab("Boxeador") +
  ylab("Cantidad de peleas anuladas") +
  ggtitle("Peleas Anuladas de los Mejores Pesos Pesados de la Historia") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1),
        plot.margin = margin(1, 1, 1, 1, "cm"))






####################################################################################




#Formas de victorias de Derrotas y Victorias de los Boxeadores


# Joe Louis

library(ggplot2)
library(ggpubr)

win_counts <- table(louis$Tipo[louis$Resultado == "Victoria"])
win_percentages <- round(prop.table(win_counts) * 100)

plot1 <- ggplot(data.frame(win_counts), aes(x = "", y = Freq, fill = names(win_counts))) +
  geom_bar(stat = "identity", width = 8) +
  coord_polar(theta = "y") +
  labs(title = "Formas de victoria de Joe Louis") +
  theme_void() +
  theme(legend.title = element_blank(),
        plot.title = element_text(vjust = 1.7))

plot1 <- plot1 + geom_text(aes(label = paste0(win_percentages, "%")),
                           position = position_stack(vjust = 0.2),
                           color = "white", size = 3, fontface = "bold")

loss_counts <- table(louis$Tipo[louis$Resultado == "Derrota"])
loss_percentages <- round(prop.table(loss_counts) * 100, 2)

plot2 <- ggplot(data.frame(loss_counts), aes(x = "", y = Freq, fill = names(loss_counts))) +
  geom_bar(stat = "identity", width = 7) +
  coord_polar(theta = "y") +
  labs(title = "Formas de derrota de Joe Louis") +
  theme_void() +
  theme(legend.title = element_blank(),
        plot.title = element_text(vjust = 0.5)) 
plot2 <- plot2 + geom_text(aes(label = paste0(loss_percentages, "%")),
                           position = position_stack(vjust = 0.5),
                           color = "white", size = 4, fontface = "bold")

ggarrange(plot1, plot2, ncol = 2, common.legend = TRUE, legend = "bottom")



#Muhammad Ali

library(ggplot2)
library(ggpubr)


win_counts <- table(ali$`Tipo de Resultado`[ali$Resultado == "Victoria"])
win_percentages <- round(prop.table(win_counts) * 100, 2)


plot1 <- ggplot(data.frame(win_counts), aes(x = "", y = Freq, fill = names(win_counts))) +
  geom_bar(stat = "identity", width = 4) +
  coord_polar(theta = "y") +
  labs(title = "Formas de victoria de Muhammad Ali") +
  theme_void() +
  theme(legend.title = element_blank())


plot1 <- plot1 + geom_text(aes(label = paste0(win_percentages, "%")), 
                           position = position_stack(vjust = 0.5),
                           color = "white", size = 4, fontface = "bold")


loss_counts <- table(ali$`Tipo de Resultado`[ali$Resultado == "Derrota"])
loss_percentages <- round(prop.table(loss_counts) * 100, 2)


plot2 <- ggplot(data.frame(loss_counts), aes(x = "", y = Freq, fill = names(loss_counts))) +
  geom_bar(stat = "identity", width = 6) +
  coord_polar(theta = "y") +
  labs(title = "Formas de derrota de Muhammad Ali") +
  theme_void() +
  theme(legend.title = element_blank())


plot2 <- plot2 + geom_text(aes(label = paste0(loss_percentages, "%")), 
                           position = position_stack(vjust = 0.5),
                           color = "white", size = 4, fontface = "bold")


ggarrange(plot1, plot2, ncol = 2, common.legend = TRUE, legend = "bottom")


#Mike Tyson
library(ggplot2)
library(ggpubr)


win_counts <- table(tyson$Método[tyson$Resultado == "Victoria"])
win_percentages <- round(prop.table(win_counts) * 100, 2)


plot1 <- ggplot(data.frame(win_counts), aes(x = "", y = Freq, fill = names(win_counts))) +
  geom_bar(stat = "identity", width = 4) +
  coord_polar(theta = "y") +
  labs(title = "Formas de victoria de Mike Tyson") +
  theme_void() +
  theme(legend.title = element_blank())


plot1 <- plot1 + geom_text(aes(label = paste0(win_percentages, "%")), 
                           position = position_stack(vjust = 0.5),
                           color = "white", size = 4, fontface = "bold")


loss_counts <- table(tyson$Método[tyson$Resultado == "Derrota"])
loss_percentages <- round(prop.table(loss_counts) * 100, 2)


plot2 <- ggplot(data.frame(loss_counts), aes(x = "", y = Freq, fill = names(loss_counts))) +
  geom_bar(stat = "identity", width = 6) +
  coord_polar(theta = "y") +
  labs(title = "Formas de derrota de Mike Tyson") +
  theme_void() +
  theme(legend.title = element_blank())


plot2 <- plot2 + geom_text(aes(label = paste0(loss_percentages, "%")), 
                           position = position_stack(vjust = 0.5),
                           color = "white", size = 4, fontface = "bold")


ggarrange(plot1, plot2, ncol = 2, common.legend = TRUE, legend = "bottom")



#Rocky Marciano


library(ggplot2)
library(ggpubr)


win_counts <- table(marciano$Método[marciano$Resultado == "Victoria"])
win_percentages <- round(prop.table(win_counts) * 100, 2)


plot1 <- ggplot(data.frame(win_counts), aes(x = "", y = Freq, fill = names(win_counts))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Formas de victoria de Rocky Marciano") +
  theme_void() +
  theme(legend.title = element_blank())

plot1 <- plot1 + geom_text(aes(label = paste0(win_percentages, "%")), 
                           position = position_stack(vjust = 0.5),
                           color = "white", size = 5.5, fontface = "bold")


ggarrange(plot1, ncol = 1, common.legend = TRUE, legend = "bottom")

####################################################################################

#Asaltos donde mas ganaron y perdieron los boxeadores


library(ggplot2)


joe_derrotas <- subset(joe_louis, Resultado == "Derrota")


resumen_derrotas <- table(joe_derrotas$`Rd., Tiempo`)


data_derrotas <- data.frame(Asalto = as.numeric(names(resumen_derrotas)),
                            Derrotas = as.numeric(resumen_derrotas))            #Joe Louis


resumen_victorias <- table(joe_louis$`Rd., Tiempo`)

data_victorias <- data.frame(Asalto = as.numeric(names(resumen_victorias)),
                             Victorias = as.numeric(resumen_victorias))

data_derrotas <- data_derrotas[order(data_derrotas$Asalto), ]
data_victorias <- data_victorias[order(data_victorias$Asalto), ]

data_combinado <- merge(data_derrotas, data_victorias, by = "Asalto", all = TRUE)

data_combinado[is.na(data_combinado)] <- 0

ggplot(data = data_combinado) +
  geom_line(aes(x = Asalto, y = Derrotas, color = "Derrotas"), size = 2, linetype = "dashed") +
  geom_line(aes(x = Asalto, y = Victorias, color = "Victorias"), size = 2) +
  scale_y_continuous(breaks = 1:max(data_combinado$Asalto), labels = 1:max(data_combinado$Asalto)) +
  labs(title = "Victorias y derrotas de Joe Louis por asalto",
       x = "Asalto", y = "Número de peleas") +
  scale_color_manual(values = c("Victorias" = "green", "Derrotas" = "red")) +
  theme_bw() +
  theme(legend.position = "right")





library(ggplot2)

tyson_victorias <- subset(mike2, Resultado == "Victoria")

resumen_victorias <- table(tyson_victorias$`Asalto, Tiempo`)  #Mike Tyson

data_victorias <- data.frame(Asalto = as.numeric(names(resumen_victorias)),
                             Victorias = as.numeric(resumen_victorias))

tyson_derrotas <- subset(mike2, Resultado == "Derrota")

resumen_derrotas <- table(tyson_derrotas$`Asalto, Tiempo`)

data_derrotas <- data.frame(Asalto = as.numeric(names(resumen_derrotas)),
                            Derrotas = as.numeric(resumen_derrotas))

data_victorias <- data_victorias[order(data_victorias$Asalto), ]     
data_derrotas <- data_derrotas[order(data_derrotas$Asalto), ]

data_combinado <- merge(data_victorias, data_derrotas, by = "Asalto", all = TRUE)

data_combinado[is.na(data_combinado)] <- 0

ggplot(data = data_combinado) +
  geom_line(aes(x = Asalto, y = Victorias, color = "Victorias"), size = 2) +
  geom_line(aes(x = Asalto, y = Derrotas, color = "Derrotas"), size = 2, linetype = "dashed") +
  scale_y_continuous(breaks = 1:max(data_combinado$Asalto), labels = 1:max(data_combinado$Asalto)) +
  labs(title = "Victorias y derrotas de Mike Tyson por asalto",
       x = "Asalto", y = "Número de peleas") +
  scale_color_manual(values = c("Victorias" = "green", "Derrotas" = "red")) +
  theme_bw() +
  theme(legend.position = "right")





library(ggplot2)


ali_victorias <- subset(ali, Resultado == "Victoria") #Muhammad Ali


resumen_victorias <- table(ali_victorias$Asalto)


data_victorias <- data.frame(Asalto = as.numeric(names(resumen_victorias)),
                             Victorias = as.numeric(resumen_victorias))


ali_derrotas <- subset(ali, Resultado == "Derrota")


resumen_derrotas <- table(ali_derrotas$Asalto)


data_derrotas <- data.frame(Asalto = as.numeric(names(resumen_derrotas)),
                            Derrotas = as.numeric(resumen_derrotas))

data_victorias <- data_victorias[order(data_victorias$Asalto), ]
data_derrotas <- data_derrotas[order(data_derrotas$Asalto), ]


data_combinado <- merge(data_victorias, data_derrotas, by = "Asalto", all = TRUE)


data_combinado[is.na(data_combinado)] <- 0


ggplot(data = data_combinado) +
  geom_line(aes(x = Asalto, y = Victorias, color = "Victorias"), size = 2) +
  geom_line(aes(x = Asalto, y = Derrotas, color = "Derrotas"), size = 2, linetype = "dashed") +
  scale_y_continuous(breaks = 1:max(data_combinado$Asalto), labels = 1:max(data_combinado$Asalto)) +
  labs(title = "Victorias y derrotas de Muhammad Ali por asalto",
       x = "Asalto", y = "Número de peleas") +
  scale_color_manual(values = c("Victorias" = "green", "Derrotas" = "red")) +
  theme_bw() +
  theme(legend.position = "right")



library(ggplot2)


rocky_victorias <- subset(rocky_marciano, Resultado == "Victoria") #Rocky Marciano
 

rocky_victorias <- rocky_victorias[!is.na(rocky_victorias$`Asalto - Tiempo`),]


resumen_victorias <- table(rocky_victorias$`Asalto - Tiempo`)

data_victorias <- data.frame(Asalto = as.numeric(names(resumen_victorias)),
                             Victorias = as.numeric(resumen_victorias))


data_victorias <- data_victorias[order(data_victorias$Asalto), ]


ggplot(data = data_victorias) +
  geom_line(aes(x = Asalto, y = Victorias, color = "Victorias"), size = 2) +
  scale_y_continuous(breaks = seq(0, max(data_victorias$Victorias), by = 1)) +
  labs(title = "Victorias de Rocky Marciano por asalto",
       x = "Asalto", y = "Número de peleas") +
  scale_color_manual(values = c("Victorias" = "green")) +
  theme_bw() +
  theme(legend.position = "none")






####################################################################################

#Comparacion entre los boxeadores para ver quien tiene mas defensas del titulo


library(ggplot2)
library(grid)
library(jpeg)

defensas <- c(26, 19, 12, 9)
nombres <- c("Joe Louis", "Muhammad Ali", "Rocky Marciano", "Mike Tyson")
imagenes <- c("C://Users//Usuario//Pictures//louis.jpg",
              "C://Users//Usuario//Pictures//ali.jpg",
              "C://Users//Usuario//Pictures//rocky.jfif",
              "C://Users///Usuario//Pictures//mike.jpg")


data_defensas <- data.frame(Defensas = defensas, Nombres = nombres, Imagenes = imagenes)

plot <- ggplot(data_defensas, aes(x = reorder(Nombres, -Defensas), y = Defensas)) +
  geom_bar(aes(fill = Defensas), stat = "identity", color = "black", show.legend = FALSE) +
  geom_text(aes(label = Defensas), size = 4, vjust = -0.5) +
  scale_fill_gradient(low = "paleturquoise", high = "paleturquoise4") +
  labs(title = "Defensas al título en boxeo",
       subtitle = "Comparación de defensas al título entre boxeadores legendarios",
       x = "Boxeador", y = "Número de defensas") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank()) +
  ylim(0, 30)


img_mike <- readJPEG(imagenes[4])
plot <- plot +
  annotation_custom(rasterGrob(img_mike, interpolate = TRUE), 
                    xmin = 4 - 0.2, xmax = 4 + 0.2, ymin = 0, ymax = defensas[4])


img_joe <- readJPEG(imagenes[1])
plot <- plot +
  annotation_custom(rasterGrob(img_joe, interpolate = TRUE), 
                    xmin = 1 - 0.4, xmax = 1 + 0.4, ymin = 0, ymax = defensas[1])


img_rocky <- readJPEG(imagenes[3])
plot <- plot +
  annotation_custom(rasterGrob(img_rocky, interpolate = TRUE), 
                    xmin = 3 - 0.3, xmax = 3 + 0.3, ymin = 0, ymax = defensas[3])


img_ali <- readJPEG(imagenes[2])
plot <- plot +
  annotation_custom(rasterGrob(img_ali, interpolate = TRUE), 
                    xmin = 2 - 0.4, xmax = 2 + 0.4, ymin = 0, ymax = defensas[2])

plot

