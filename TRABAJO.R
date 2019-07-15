library(WDI)
library(wbstats)
library(ggrepel)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(gganimate)


#########   GENERALES


############################################################################

WDIsearch(string = "Fertility rate",
          field = "name",
          short = F)

WDIsearch("^Mortality*rate.", cache = WDIcache())

WDIsearch("^Divorce*rate.", cache = WDIcache())
  ########----------------

vida_T <- data.frame(
  WDI(country = c(#"US",
                  #"CAN",
                  #"MEX",
                  
#                  "GBR",
 #                 "ESP",
  #                "PRT",
   #               "DEU",
    #              "FRA",
     #             "BEL",
      #            "NLD",
                  
#                  "CHN",
 #                 "IND",
  #                "BGD",
   #               "JPN",
    #              "KOR",
     #             "PRK",
                  
                  "NGA",
                  "KN",
                  "TZA",
                  "GHA"
                  
#                  "BRA",
 #                 "ECU",
  #                "COL",
   #               "PER",
    #              "BOL"
                        ),
      indicator = c("SP.DYN.LE00.IN", #esperanza de vida
                    "SP.POP.TOTL",  #población total
                    "NY.GDP.PCAP.KD", #pib
                    "SP.DYN.AMRT.MA", #tasa mortalidad adulto H 
                    "SP.DYN.AMRT.FE", #tasa mortalidad adulto M
                    "SP.DYN.CBRT.IN", #Tasa cruda de natalidad 
                    "SP.DYN.TFRT.IN"),  #tasa fertilidad
      start = 1985,
      end = 2016,
      extra = F))

####--------
vida_T <- mutate(vida_T, 
                 continent = as.factor(case_when(iso2c  == "US" ~ "North America",
                                             iso2c  == "MX" ~ "North America",
                                             iso2c  == "CA" ~ "North America",
                                             
                                             iso2c  == "GB" ~ "Europe",
                                             iso2c  == "ES" ~ "Europe",
                                             iso2c  == "PT" ~ "Europe",
                                             iso2c  == "DE" ~ "Europe",
                                             iso2c  == "FR" ~ "Europe",
                                             iso2c  == "BE" ~ "Europe",
                                             iso2c  == "NL" ~ "Europe",
                                             
                                             iso2c  == "CN" ~ "Asia",
                                             iso2c  == "IN" ~ "Asia",
                                             iso2c  == "BD" ~ "Asia",
                                             iso2c  == "JP" ~ "Asia",
                                             iso2c  == "KR" ~ "Asia",
                                             iso2c  == "KP" ~ "Asia",
                                             
                                             iso2c  == "NG" ~ "Africa",
                                             iso2c  == "KN" ~ "Africa",
                                             iso2c  == "TZ" ~ "Africa",
                                             iso2c  == "GH" ~ "Africa",
                                             
                                             iso2c  == "BR" ~ "South America",
                                             iso2c  == "EC" ~ "South America",
                                             iso2c  == "CO" ~ "South America",
                                             iso2c  == "PE" ~ "South America",
                                             iso2c  == "BO" ~ "South America"
                                             )))
               


############-----------

#animate de esperanza de vida
p <- ggplot(vida_T, aes(NY.GDP.PCAP.KD, 
                    SP.DYN.LE00.IN, 
                    size = SP.POP.TOTL, 
                    colour = country,
                    label = country)) +
  geom_point(alpha = 0.7, show.legend = F) +
 # scale_colour_manual(values = country_colors) + 
  geom_text(hjust=-.2,vjust=-.2)+
  scale_color_viridis_d(name="")+
  
theme(legend.position = "none",
        plot.caption = element_text(size=9,face="italic"))+
  guides(size=F,color=F)+

  scale_size(range = c(2, 9)) +
  scale_x_log10() +
  #facet_wrap(~continent) +
  #gganimate
  labs(title = 'Año: {frame_time}', 
       x = 'GDP per capita', 
       y = 'Esperanza de Vida',
       caption="Fuente: World Bank \nPlot by @cris_cdcc") +
  transition_time(year) +
  ease_aes('linear')

anim_save("Life_SA.gif", p)

#####-----
#animate de mortalidad H

p <- ggplot(vida_T, aes(NY.GDP.PCAP.KD, 
                   SP.DYN.AMRT.MA, 
                   size = SP.POP.TOTL, 
                   colour = country,
                   label = country)) +
  geom_point(alpha = 0.7, show.legend = F) +
  #scale_colour_manual(values = country_colors) + 
  geom_text(hjust=-.2,vjust=-.2)+
  scale_color_viridis_d(name="")+
  
  theme(legend.position = "none",
        plot.caption = element_text(size=9,face="italic"))+
  guides(size=F,color=F)+
  
  scale_size(range = c(2, 9)) +
  scale_x_log10() +
  #facet_wrap(~continent) +
  #gganimate
  labs(title = 'Año: {frame_time}', 
       x = 'GDP per capita', 
       y = 'Mortalidad por cada 1000 personas (Hombre)',
       caption="Fuente: United Nations Population Division\nPlot generated by Cristian Carrión") +
  transition_time(year) +
  ease_aes('linear')

anim_save("mortal_SA_H.gif", p)

#animate de mortalidad M

p <- ggplot(vida_T, aes(NY.GDP.PCAP.KD, 
                   SP.DYN.AMRT.FE, 
                   size = SP.POP.TOTL, 
                   colour = country,
                   label = country)) +
  geom_point(alpha = 0.7, show.legend = F) +
  #scale_colour_manual(values = country_colors) + 
  geom_text(hjust=-.2,vjust=-.2)+
  scale_color_viridis_d(name="")+
  
  theme(legend.position = "none",
        plot.caption = element_text(size=9,face="italic"))+
  guides(size=F,color=F)+
  
  scale_size(range = c(2, 9)) +
  scale_x_log10() +
  #facet_wrap(~continent) +
  #gganimate
  labs(title = 'Año: {frame_time}', 
       x = 'GDP per capita', 
       y = 'Mortalidad por cada 1000 personas (Mujer)',
       caption="Fuente: United Nations Population Division\nPlot generated by Cristian Carrión") +
  transition_time(year) +
  ease_aes('linear')

anim_save("mortal_SA_M.gif", p)

####-----

#animate de Natalidad

p <- ggplot(vida_T, aes(NY.GDP.PCAP.KD, 
                        SP.DYN.CBRT.IN, 
                        size = SP.POP.TOTL, 
                        colour = country,
                        label = country)) +
  geom_point(alpha = 0.7, show.legend = F) +
  #scale_colour_manual(values = country_colors) + 
  geom_text(hjust=-.2,vjust=-.2)+
  scale_color_viridis_d(name="")+
  
  theme(legend.position = "none",
        plot.caption = element_text(size=9,face="italic"))+
  guides(size=F,color=F)+
  
  scale_size(range = c(2, 9)) +
  scale_x_log10() +
  #facet_wrap(~continent) +
  #gganimate
  labs(title = 'Año: {frame_time}', 
       x = 'GDP per capita', 
       y = 'Tasa de natalidad cruda por cada 1000 personas (Personas)',
       caption="Fuente: United Nations Population Division\nPlot generated by Cristian Carrión") +
  transition_time(year) +
  ease_aes('linear')

anim_save("natal_AF.gif", p)


####-----
 #animate de Fertilidad
p <- ggplot(vida_T, aes(NY.GDP.PCAP.KD, 
                        SP.DYN.TFRT.IN, 
                        size = SP.POP.TOTL, 
                        colour = country,
                        label = country)) +
  geom_point(alpha = 0.7, show.legend = F) +
  #scale_colour_manual(values = country_colors) + 
  geom_text(hjust=-.2,vjust=-.2)+
  scale_color_viridis_d(name="")+
  
  theme(legend.position = "none",
        plot.caption = element_text(size=9,face="italic"))+
  guides(size=F,color=F)+
  
  scale_size(range = c(2, 9)) +
  scale_x_log10() +
  #facet_wrap(~continent) +
  #gganimate
  labs(title = 'Año: {frame_time}', 
       x = 'GDP per capita', 
       y = 'Tasa de fertilidad, total (nacimientos por cada mujer)',
       caption="Fuente: United Nations Population Division\nPlot generated by Cristian Carrión") +
  transition_time(year) +
  ease_aes('linear')

anim_save("fertil_AF.gif", p)


###########
# AMERICA
data_deth <- read.csv("C:\\Users\\Usuario\\Desktop\\Teorias del desarrollo\\muertes\\IHME-GBD_2017_DATA-08495555-1.csv",
                                  stringsAsFactors = T)
data_dt <- filter(data_deth, location == "Global")
### causas de muertes
caus_deth <- data_dt %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = min_rank(-val) * 1,
         Value_rel = val/val[rank==1],
         Value_lbl = paste0(" ",val)) %>%
  filter(rank <=10) %>%
  ungroup()

p <- ggplot(caus_deth, aes(rank, group = cause, 
                     fill = cause, color = cause)) +
  geom_tile(aes(y = val/2,
                height = val,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(cause, " ")), vjust = 0.1, hjust = 1) +
  #geom_text(aes(y=val,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  
  labs(title='Año: {closest_state}', x = "", y = "Causa de muertes",
       caption = "Source: Global Burden of Disease Collaborative Network \nPlot generated by Cristian Carrión") +
  theme(plot.title = element_text(hjust = 0, size = 20),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,0.9,1,8, "cm")) +  #1) arriba hacia abajo   2) tamaño del plot 3) abajo hacia arriba 4) izq a derecha
  
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

anim_save("Global_muertes.gif", p)



animate(p, 200, fps = 10, duration = 40, width = 800, height = 600, renderer = gifski_renderer("gganim.gif"))






#####

library(readxl)
marri <- read_excel("C:/Users/Usuario/Desktop/Teorias del desarrollo/casados_corregido.xlsx", 
                         col_types = c("text", "numeric", "numeric", 
                                       "numeric"))

marr_casad <- filter(marri, year >= 1990 )

marri <- marr_casad %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = min_rank(-divorce) * 1) %>%
  filter(rank <=20 ) %>%
  ungroup()

p <- ggplot(marri, aes(rank, group = country, 
                           fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = divorce/2,
                height = divorce,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.1, hjust = 1) +
  #geom_text(aes(y=val,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  
  labs(title='Año: {closest_state}', x = "", y = "Tasa cruda de divorcio por cada 1000 personas (Divorcios)",
       caption = "Source: The Organisation for Economic Co-operation and Development (OECD) \nPlot generated by Cristian Carrión") +
  theme(plot.title = element_text(hjust = 0, size = 20),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +  #1) arriba hacia abajo   2) tamaño del plot 3) abajo hacia arriba 4) izq a derecha
  
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

anim_save("divorcios.gif", p)

animate(p, 200, fps = 10, duration = 40, width = 800, height = 600, renderer = gifski_renderer("gganim.gif"))

######
# Cobertura de alcantarillado

library(readxl)

ag_al <- read_excel("C:/Users/Usuario/Desktop/Teorias del desarrollo/agua_alcantarilla.xlsx", 
                                col_types = c("numeric", "text", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric"))

#agua_alcantarilla <- filter(ag_al, year >= 2000 )

p <- ggplot(ag_al, aes(GDPpc, 
                        Sanitation, 
                        size = Population, 
                        colour = Country,
                        label = Country)) +
  geom_point(alpha = 0.7, show.legend = F) +
  #scale_colour_manual(values = country_colors) + 
  geom_text(hjust=-.2,vjust=-.2)+
  scale_color_viridis_d(name="")+
  
  theme(legend.position = "none",
        plot.caption = element_text(size=9,face="italic"))+
  guides(size=F,color=F)+
  
  scale_size(range = c(2, 9)) +
  scale_x_log10() +
  #facet_wrap(~continent) +
  #gganimate
  labs(title = 'Año: {frame_time}', 
       x = 'GDP per capita expresado en US$ 2005', 
       y = 'Cobertura de Alcantarillado',
       caption="Fuente: World Health Organization\nPlot generated by Cristian Carrión") +
  transition_time(Year) +
  ease_aes('linear')

animate(p, height = 800, width =800)
anim_save("alcant.gif")

anim_save("natal_AF.gif", p)

animate(p, 200, fps = 10, duration = 40, width = 800, height = 600, renderer = gifski_renderer("gganim.gif"))



p <- ggplot(ag_al, aes(GDPpc, 
                       Water, 
                       size = Population, 
                       colour = Country,
                       label = Country)) +
  geom_point(alpha = 0.7, show.legend = F) +
  #scale_colour_manual(values = country_colors) + 
  geom_text(hjust=-.2,vjust=-.2)+
  scale_color_viridis_d(name="")+
  
  theme(legend.position = "none",
        plot.caption = element_text(size=9,face="italic"))+
  guides(size=F,color=F)+
  
  scale_size(range = c(2, 9)) +
  scale_x_log10() +
  #facet_wrap(~continent) +
  #gganimate
  labs(title = 'Año: {frame_time}', 
       x = 'GDP per capita expresado en US$ 2005', 
       y = 'Cobertura de Agua',
       caption="Fuente: World Health Organization\nPlot generated by Cristian Carrión") +
  transition_time(Year) +
  ease_aes('linear')

animate(p, height = 800, width =800)
anim_save("agua.gif")

anim_save("natal_AF.gif", p)
