

library(nycflights13)
nycflights13::airlines
nycflights13::airports
nycflights13::flights
nycflights13::planes
library(ggplot2)
library(dplyr)


#The most popular airlines names from JFK
JFK_pops<-
  flights %>% 
  filter(origin=="JFK") %>% 
  group_by(carrier) %>%
  summarise(my_max=max(flight)) %>% 
  arrange(-my_max) %>% 
  head(10)

top_ten<-
  merge(x = JFK_pops,y = airlines,by.x = "carrier",by.y = "carrier" ) %>% 
  select(name, my_max)

ggplot(top_ten, aes(my_max,reorder(name, my_max, decreasing=F)))+
  geom_col(color= "blue", fill= "cyan4")+
  theme_bw()+
  labs(title = "Most popular airlines on JFK", x= "Most Flight", y= "Brands")


#arrival delay and distance by carrier
df <- flights %>% head(1000)

my_df<-
  df %>% 
  select(arr_delay, distance, carrier)

ggplot(my_df, aes(distance, arr_delay, color= carrier))+
  geom_point()+
  theme_bw()+
  labs(title = "Arrival delay and distance by carrier", 
       subtitle = "Theme=theme_bw, plot=geom_point"
       , x= "Distance", y= "Arrival Delay")

#Arrival delay and distance by air time

#"200"<-df["air_time">200]

#for (air_time in df) {
print(air_time)
if (air_time<200) {
  return("200")
} 
if (air_time<400) {
  return("400")
} 
if (air_time>400) {
  return("600")
}
#}

#my_df2<-
#df %>% 
filter("air_time"==1:200, "air_time"==201:399, "air_time"==400:600)
select(arr_delay, distance, air_time)

my_df2<-
  df %>% 
  select(arr_delay, distance, air_time)

ggplot(my_df2, aes(distance, arr_delay, factor(air_time)))+
  geom_point()+
  theme_classic()+
  labs(title= "Arrival delay and distance by air_time", 
       subtitle = "Theme=theme_classic, plot=geom_point", 
       x= "Distance", y= "Arrival delay")

#arrival delay and distance by carrier
my_df3<-
  df %>% 
  select(arr_delay, distance, carrier)

ggplot(my_df3, aes(distance,arr_delay, shape= carrier))+
  geom_point()+
  theme_dark()+
  labs(title = "Arrival delay and distance by carrier", 
       subtitle = "Theme=theme_dark, plot=geom_point", 
       x= "Distance", y= "Arrival delay")

#arrival delay and distance by carrier(separated)

my_df3<-
  df %>% 
  select(arr_delay, distance, carrier)

ggplot(my_df3, aes(distance,arr_delay))+
  geom_point()+
  theme_linedraw()+
  facet_wrap(~carrier, nrow=4)+
  labs(title = "Arrival delay and distance by carrier", 
       subtitle = "Theme=theme_dark, plot=geom_point", 
       x= "Distance", y= "Arrival delay")

#Arrival delay and distance

my_df4<-
  df %>% 
  select(arr_delay, distance)

ggplot(my_df4, aes(distance, arr_delay))+
  geom_point()+
  theme_minimal()+
  geom_smooth()+
  labs(title = "Arrival delay and distance", 
       subtitle = "Theme=theme_minimal, plot=geom_point with geom smooth", 
       x= "Distance", y= "Arrival delay")



