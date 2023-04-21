install.packages('babynames')

# Libraries
library(ggplot2)
library(babynames) # provide the dataset: a dataframe called babynames
library(dplyr)


test <- babynames

# Keep only 3 names
don <- babynames %>% 
  filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
  filter(sex=="F")

# Plot
don %>%
  ggplot( aes(x=year, y=n, group=name, color=name)) +
  geom_line()





empresas_clae %>% 
  ggplot(aes(x=fecha, y=empresas)) +
  geom_line(color="#00B0F0", size=1)+
  # geom_text(aes(y=paste0(round(value*100,1),"%")))+
  scale_x_date(date_breaks = "1 month", date_labels = "%Y%m",
               #limits = c("2007-01-01", "2022-04-01")
               expand = c(0, 250)
  )+
  ylab("")+
  xlab("") +
  theme_bw()+
  # annotate(geom="text", x=as.Date("2019-12-01"), y=0.336, size=4,
  #          label="Diciembre 2019") +
  # annotate(geom="point", x=as.Date("2019-12-01"), y=0.334, size=3, shape=21,fill="transparent")+
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        # text=element_text(family="Arial"),
        axis.text.x=element_text(colour="black", size = 10, angle = 90),
        axis.text.y=element_text(colour="black", size = 10),
        legend.position = "bottom")+
  ggtitle("Evolución de empresas", subtitle = "Sector Software y Servicios Informáticos")+
  labs(caption = "Elaboración CEP XXI en base a registros SIPA.")