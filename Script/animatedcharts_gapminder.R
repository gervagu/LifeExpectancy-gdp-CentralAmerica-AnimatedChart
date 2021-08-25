#Creating animated charts using tidyverse and gganimate packages

#Load packages
library(gapminder)
library(tidyverse)
library(gganimate)

#Create animated life expectancy chart
#Load data from gapminder
expectancyca <-gapminder%>%
  filter(country%in% c("Nicaragua","Costa Rica", "Honduras", "Guatemala", "El Salvador", "Panama"))%>% #Filter CA countries
  ggplot(aes(x=year,
             y=lifeExp,
             color=country))+
  scale_y_continuous(limits = c(40,80))+
  geom_line(size=2)+
  geom_point(size=4)+
  labs(title="Life expectancy in {frame_along}",
       subtitle = "Central American Countries 1952-2007",
       caption= "Gersán Vásquez Gutiérrez (@gervagu)\n Source: Gapminder",
       x=NULL,
       y=NULL)+
  facet_wrap(~country) +
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.border=element_blank(),
        plot.title=element_text(hjust = 0.5, size = 18, face="bold"),
        plot.subtitle=element_text(hjust = 0.5, size=14, face="italic"),
        plot.caption=element_text(size=11))+
  transition_reveal(year)

anim_save("lifeexpectancyCA.gif", expectancyca, width = 1000, height = 1000, res = 110)

#Create GPD per capita racing bar chart

#Create a ranking by year and GDP per capita

gdpca <- gapminder%>%
  filter(country%in% c("Nicaragua","Costa Rica", "Honduras", "Guatemala", "El Salvador", "Panama"))%>%
  group_by(year) %>%
  arrange(year, -gdpPercap) %>% 
  mutate(rank = 1: n())

#Create racing chart

racingchart <-gdpca %>%  
  ggplot(
    aes(rank, group = country,
        fill = as.factor(country), color = as.factor(country))) +  
  geom_tile(aes(y = gdpPercap/2, 
                height = gdpPercap,
                width = 0.9), color = NA)+
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  coord_flip(clip = "off", expand = FALSE)+
  labs(title="GDP Per Capita constant US$ in {closest_state}",
       subtitle = "Central American Countries 1952-2007",
       caption= "Gersán Vásquez Gutiérrez (@gervagu)\n Source: Gapminder",
       x=NULL,
       y=NULL)+
  scale_y_continuous(labels=scales::dollar_format(),
                     breaks = c(0, 2000, 4000, 6000, 9000))+
  scale_x_reverse() +
  guides(color = "none", fill = "none") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        plot.subtitle=element_text(hjust = 0.5, size=14, face="italic"),
        axis.ticks.y = element_blank(),  
        axis.text.y  = element_blank(),
        plot.caption=element_text(size=11),
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(
    year,
    transition_length = 3,
    state_length = 1) +
  ease_aes('sine-in-out')

anim_save("gdpca.gif", racingchart, width = 1000, height = 1000, res = 110)
