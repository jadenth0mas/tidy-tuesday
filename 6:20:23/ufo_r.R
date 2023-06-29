# ------ Load Libraries
library(tidyverse)
library(tidytuesdayR)
library(maps)
library(scales)
library(ggthemes)
library(cowplot)
library(geomtextpath)
library(patchwork)

# ------ Load Data
df <- tt_load("2023-06-20")

ufo_sightings <- df$ufo_sightings
places <- df$places
day_parts_map <- df$day_parts_map


# ------ Look at Data
str(ufo_sightings)
head(ufo_sightings)
colnames(ufo_sightings)

str(places)
colnames(places)
head(places) # What does alternate_city mean?

str(day_parts_map)
colnames(day_parts_map)
head(day_parts_map)

# ------ Make dfs
usMap <- map_data("state")
oh_map <- usMap[usMap$region=="ohio",]
us_sightings <- ufo_sightings %>% filter(country_code=="US")

us_occur <- us_sightings %>% count(city, state) %>%
  rename(count=n)
us_with_coords <- left_join(us_occur, places, by=c("city", "state"))
contiguous_us <- us_with_coords %>% filter(state!="AK" & state!="HI")

oh_occur <- contiguous_us %>% filter(state=="OH")
oh_occur <- oh_occur %>% mutate(percapita=count/population)

oh_sightings <- ufo_sightings %>% filter(state=="OH" & country_code=="US")

# ------ Plots
oh_time <- ggplot(oh_sightings, aes(x=reported_date_time_utc)) +
  geom_freqpoly(bins=50, color="#440154") +
  labs(x="Reported Date Time UTC", y="Count", title="Frequency of Ohio UFO reports")+
  scale_x_datetime(date_breaks="10 years",
                   labels=label_date(format="%Y", tz="UTC"))+
  theme(
    panel.background=element_blank(),
    plot.background=element_rect(fill="#F8FFF8"),
    panel.grid=element_blank(),
    axis.text=element_text(color="#440154"),
    title=element_text(color="#440154", size=14)
  )

shape_plot <- ggplot(oh_sightings, aes(x=shape)) +
  geom_bar(fill="#a0d29e") +
  coord_curvedpolar(clip="off") +
  scale_x_discrete(guide=guide_axis(n.dodge=2))+
  labs(title="Reported UFO Shapes in Ohio")+
  theme(
    plot.background=element_rect(fill="#F8FFF8"),
    panel.background=element_rect(fill="#F8FFF8"),
    panel.grid=element_line(color="#440154"),
    axis.text=element_text(color="#440154"),
    axis.title=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.text.x = element_text(vjust = -1.5, size=8),
    title=element_text(color="#440154", size=14)
  )

oh_hex <- ggplot(oh_occur, aes(longitude, latitude)) +
  geom_hex(bins=11) +
  scale_fill_gradient(name="Count", high="#225149", low="#a0d29e") +
  labs(title="Heatmap of Ohio UFO reports")+
  theme(
    plot.background=element_rect(fill="#F8FFF8"),
    panel.grid=element_blank(),
    panel.background=element_rect(fill="#F8FFF8"),
    legend.background=element_rect(fill="#F8FFF8"),
    legend.key = element_rect(fill="#F8FFF8"),
    axis.text=element_blank(),
    axis.title=element_blank(),
    title=element_text(color="#440154", size=14),
    legend.title=element_text(color="#440154"),
    legend.text=element_text(color="#440154"),
    axis.ticks=element_blank()
  )

experimental_top <- plot_grid(oh_hex, shape_plot, nrow=1, scale=1)
final_plot <- plot_grid(experimental_top, oh_time, nrow=2) +
  plot_annotation(title="Ohio UFO reports",
                  subtitle="The National UFO Reporting Center has collected data on UFO sightings and reports from all over the world since it's \nfounding in 1974. The earliest year included in the data is 1924, all the way to 2023. The UFO reports from Ohio in \nthe United States shows a high concentration of reports in highly populated areas, such as the Cincinnati, Columbus,\nand Cleveland areas. The most reported UFO shape descriptions were light, triangle, and circle. There also seems \nto be a spike in frequency of reports in 2013 with 303 reports during the caledar year.",
                  caption="@jadenth0mas | Data Source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2023/2023-06-20 | https://nuforc.org/",
                  theme=theme(
    plot.background=element_rect(fill="#f8FFF8"),
    title=element_text(face="bold", size=18, color="#440154"),
    plot.subtitle=element_text(face="plain", size=14, color="#440154"),
    plot.caption=element_text(face="plain", size=8, color="#440154", hjust=0)
  ))
final_plot
ggsave("ufo_plot", final_plot, device="png") # subtitle too small, and shapes too small

