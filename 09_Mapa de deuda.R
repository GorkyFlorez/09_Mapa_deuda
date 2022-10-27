library(sf)
library(raster)
library(ggplot2)
SurAmerica = st_read("01_SHP/Sur_america.shp")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Sudamérica = st_read("01_SHP/Sudamérica.shp")  %>% st_as_sf()
Sudaméric   <- st_transform(Sudamérica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Valores= c (74.4,
            86.1,
            91.9,
            38.3,
            60.6,
            62.2,
            0,
            0,
            0,
            0,
            39.4,
            34.4,
            0,
            65.7,
            307
)
Sudaméric_vlor= cbind(Sudaméric, Valores)

colores<- c("#03045e", "#023e8a", "#0077b6", "#0096c7", "#00b4d8",
            "#48cae4", "#90e0ef", "#ade8f4", "#caf0f8")



library(hrbrthemes)
library(gcookbook)
library(tidyverse)

# current verison
packageVersion("hrbrthemes")
## [1] '0.8.6'
update_geom_font_defaults(font_rc_light)


Mapa =ggplot()+
  geom_sf(data = SurAmeric, fill="#D8DDE1", color="white")+
  geom_sf(data = Sudaméric_vlor, aes(fill=Valores), color="white")+
  scale_fill_gradientn(colours = colores,
                       breaks = c(34.4,39.4, 38.3,60.6,62.2,65.7,74.4,86.1, 91.9, 307),
                       na.value = 'white'
                       )+
  theme_void()+

  labs(x = 'Longitud', y = 'Latitud',
       title="Deuda pública de los países de \nAmérica Latina",
       subtitle="en relacion a su PIB",
       caption="Gorky Florez  'Datos: DW'")+


  guides(fill = guide_legend(
    title = " deuda",
    direction = "horizontal",
    nrow = 1,
    keywidth = 1.75,
    keyheight = 0.5,
    label.position = "bottom",
    title.position = "right",
    override.aes = list(alpha = 1)
  ))+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = NA, color="white"),
        plot.title = element_text( family="Anton",color ="#012D5A",face='bold',hjust=0.5,
                                   size = 14),
        plot.subtitle  = element_text(face = "bold", family="Anton",color ="#012D5A",
                                      hjust=0.5),
        plot.caption = element_text(size = 9, hjust = 0.95, family="Anton", face = "bold",
                                    color ="#012D5A"),
        panel.border = element_rect (color = "white",
                                     fill = NA),
        )

summ = filter(Sudaméric_vlor, Valores > 0)

summ <- summ %>%
  mutate(PAÍS= fct_reorder(PAÍS, Valores, .desc = F))

library(hrbrthemes)
library(gcookbook)
library(tidyverse)

# current verison
packageVersion("hrbrthemes")
## [1] '0.8.6'
update_geom_font_defaults(font_rc_light)

Estadis = ggplot(data = summ, aes(x= PAÍS, y=Valores, fill=Valores)) +
  geom_col(show.legend = F) +
  scale_fill_gradientn(colours = colores)+
  scale_y_comma(limits=c(0,350)) +
  coord_flip() +
  labs(x="",
       y="",
       title="",
       subtitle=" ",
       caption="Fuente: Fondo Monetario Internacional, datos de 2021 \n*datos de 2020'") +
  theme_ipsum_rc(grid="X")+
  geom_text(aes(label=paste0(round(Valores,1), "%"), hjust=0, nudge_y=20) ,
            family="Anton",color ="#012D5A",face='bold',size = 2)+
  theme(plot.background = element_rect(fill = "white", color="white"),
        plot.subtitle = element_text(face = "italic", family="serif",size = 8),
        plot.caption = element_text( hjust = 0.5, family="serif", face = "italic",size = 8),

        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"))

Estadis



summ$Valores =round(summ$Valores,2)
# lets count proportions
summ$fraction = summ$Valores/sum(summ$Valores)
summ

# Compute the cumulative proportions (top of each rectangle)
summ$ymax = cumsum(summ$fraction)
summ
# Compute the bottom of each rectangle
summ$ymin = c(0, head(summ$ymax, n=-1))
summ
#compute label position
summ$labelPosition= (summ$ymax+summ$ymin)/2
summ
#get data label
summ$label= paste0(summ$Categoria ,"\n Valor=", summ$Valores)
summ
library(ggrepel)
Ha_total = sum(summ$Valores)

summ$Porcentaje =  summ$Valores*100/Ha_total
summ$Porcentaje =round(summ$Porcentaje,2)

Pastel=ggplot(summ,aes(ymax=ymax,ymin=ymin,xmax=4, xmin=2, fill=Valores))+
  geom_rect(alpha=0.8)+
  coord_polar(theta="y")+
  xlim(c(0,4))+
  theme_void()+
  geom_text(aes(y=labelPosition,label=paste0(round(Porcentaje,2), "%")),x=3,
            color="black", family="serif",angle=90,
            size=2.5)+
  theme(legend.position = "none")+
  scale_fill_gradientn(colours = colores)

Pastel

library(cowplot)
Final =ggdraw() +
  coord_equal(xlim = c(0, 21), ylim = c(0, 25), expand = FALSE) +


  draw_plot(Pastel , width = 6, height = 6,x = 13, y = 4)+
  draw_plot(Estadis, width = 8, height = 15,x = 0, y = 4)+

  draw_plot(Mapa, width = 21, height = 21,x = 0, y = 00001)+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect (color = "white",
                                     fill = NA))

Final


ggsave(plot=Final,"02_Mapas/08_Mapa de deuda.png",units = "cm",
       width = 21, #alto
       height = 29, #ancho
       dpi=1500)
















