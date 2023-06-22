# ------ Load Libraries
library(tidyverse)
library(ggthemes)
library(reshape2)
library(cowplot)
library(patchwork)

# ------ Load Data
data <- read.csv("Moz_SAFI_Survey_Final_results.csv")

# ------ Dataset First Look
summary(data)
str(data)

# ------ Remove Columns
colnames(data)
data.keep <- data %>% select(A06_province, A07_district, A08_ward, A09_village, A11_years_farm, B_no_membrs, 
                            B17_parents_liv, B18_sp_parents_liv, B19_grand_liv, B20_sp_grand_liv,
                            C05_buildings_in_compound, C06_rooms, C07_other_buildings,
                            D_no_plots, E01_water_use, E_no_group_count, E_yes_group_count, 
                            E17_no_enough_water, E18_months_no_water, E19_period_use, E25_fees_water, F04_need_money,
                            F05_money_source, F05_money_source_other, F06_crops_contr, F08_emply_lab, F09_du_labour,
                            F10_liv_owned, F10_liv_owned_other, F_liv_count, F12_poultry, F14_items_owned,
                            F14_items_owned_other, G01_no_meals, G02_months_lack_food, gps.Latitude, gps.Longitude,
                            gps.Altitude, gps.Accuracy)
new_cols <- c("province", "district", "ward", "village", "years_farm", "num_members", "parents_live", "sp_parents_live", 
              "grand_live", "sp_grand_live", "buildings_in_compound", "rooms", "other_buildings", "num_plots", 
              "water_use", "irr_count", "not_irr_count", "are_months_no_water", "months_no_water", "water_method_years", 
              "fees_water", "need_money_change", "money_source", "money_source_other", "crops_income", "employ_labor", 
              "house_labor", "livestock", "livestock_other", "livestock_count", "poultry", "items_owned", "items_owned_other",
              "num_meals", "months_lack_food", "gps_lat", "gps_long", "gps_alt", "gps_acc")
colnames(data.keep) <- new_cols

# ------ Data Cleaning
unique(data.keep$province)
unique(data.keep$district)
table(data.keep$district) #
table(data.keep$ward)
# Reasonable to believe that all repsonse came from province Monica, District Manica,
# and ward Bandula, but the villages differ
df_keep_village <- data.keep %>% select(!c(province, district, ward))
df_keep_village[df_keep_village$village=="Ruaca - Nhamuenda"|df_keep_village$village==
                  "Ruaca-Nhamuenda",]$village <- "Ruaca"
df_3 <- df_keep_village %>% filter(village!="49")
table(df_3$village)

# ----- NAs & NULLs & NANs
c_names <- colnames(df_3)
df_3[df_3=="NULL"] <- NA
sapply(df_3[c_names], function(x) sum(is.na(x)))

# ------ Dropping and encoding candidates
unique(df_3$months_no_water) #One hot enocde
unique(df_3$livestock) # One hot Enocde
unique(df_3$months_lack_food) # One hot encode

# ------ Dropping
df_nother <- df_3 %>% select(!c(money_source, money_source_other, livestock_other, items_owned, items_owned_other))

listify <- function(column_row) {
  column_row <- column_row %>% str_replace_all("\\[|\\]", "")
  column_row <- column_row %>% str_replace_all(";", "")
  column_row <- column_row %>% str_replace_all("'", "")
  return(column_row)
}

hasListElement <- function(element, y) {
  elementIn <- c()
  for (row in 1:length(y)) {
    elementIn[row] <- element %in% y[[row]]
  }
  return(elementIn)
}

# One Hot Livestock

df_nother$livestock <- listify(df_nother$livestock)
x <- strsplit(df_nother$livestock, "\\s+")


livestock_names <- unique(unlist(x))
for (name in livestock_names) {
  upp_name <- toupper(name)
  col_name <- paste("has", upp_name, sep="")
  df_nother[col_name] <- hasListElement(name, x)
}

# One Hot Months No Water
df_nother$months_no_water <- listify(df_nother$months_no_water)
m <- strsplit(df_nother$months_no_water, "\\s+")

months_names <- unique(unlist(m))
for (name in months_names) {
  upp_name <- toupper(name)
  col_name <- paste("noWater_", upp_name, sep="")
  df_nother[col_name] <- hasListElement(name, m)
}

# One Hot Months Lack Food
df_nother$months_lack_food <- listify(df_nother$months_lack_food)
l <- strsplit(df_nother$months_lack_food, "\\s+")

l_food_names <- unique(unlist(l))
for (name in l_food_names) {
  upp_name <- toupper(name)
  col_name <- paste("lackFood_", upp_name, sep="")
  df_nother[col_name] <- hasListElement(name, l)
}
df_nolist <- df_nother %>% select(!c(months_no_water, money_source, livestock, months_lack_food))


# poultry, oxen, cows, goats, pigs, donkeys, sheep
b_df <- df_nolist %>% 
  select(village, starts_with("has")) %>%
  mutate(across(!village, as.numeric)) %>%
  group_by(village) %>%
  summarise(across(where(is.numeric), sum)) %>%
  rename(Village=village, Poultry=hasPOULTRY, Oxen=hasOXEN, Cows=hasCOWS, Goats=hasGOATS, 
         Pigs=hasPIGS, Donkeys=hasDONKEYS, Sheep=hasSHEEP) %>%
  select(!hasNONE) %>%
  pivot_longer(cols=c(Poultry, Oxen, Cows, Goats, Pigs, Donkeys, Sheep), names_to="Animal", values_to="Count")


# ------ Plots

## ------ Animal Plot
animal_plot <- b_df %>%
  ggplot(aes(x=Village, y=Count, fill=Animal)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_hc() +
  theme_minimal()


## ------ Months dfs
water_df <- df_nolist %>% select(village, starts_with("noWater")) %>%
  mutate(across(!village, as.numeric)) %>%
  group_by(village) %>%
  summarise(across(where(is.numeric), sum)) %>%
  rename(None=noWater_NA, Aug=noWater_AUG, Sept=noWater_SEPT, Oct=noWater_OCT, 
         Nov=noWater_NOV, Dec=noWater_DEC, Jan=noWater_JAN, Apr=noWater_APR, May=noWater_MAY,
         June=noWater_JUNE, July=noWater_JULY) %>%
  select(!None) %>%
  pivot_longer(cols=c(Aug, Sept, Oct, Nov, Dec, Jan, Apr, May, June, July), names_to="Month", values_to="no_water_count") %>%
  mutate(across(Month, as.factor))
levels(water_df$Month) <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")


food_df <- df_nolist %>% select(village, starts_with("lack")) %>%
  mutate(across(!village, as.numeric)) %>%
  group_by(village) %>%
  summarise(across(where(is.numeric), sum)) %>%
  rename(None=lackFood_NONE, Jan=lackFood_JAN, Feb=lackFood_FEB, Mar=lackFood_MAR,
         Apr=lackFood_APR, May=lackFood_MAY, June=lackFood_JUNE, July=lackFood_JULY,
         Aug=lackFood_AUG, Sept=lackFood_SEPT, Oct=lackFood_OCT, Nov=lackFood_NOV, 
         Dec=lackFood_DEC) %>%
  select(!None) %>%
  pivot_longer(cols=c(Jan, Feb, Mar, Apr, May, June, July, Aug, Sept, Oct, Nov, Dec), names_to="Month", values_to="lack_food_count") %>%
  mutate(across(Month, as.factor))
levels(food_df$Month) <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")

w_f_df <- full_join(water_df, food_df)
w_f_df[is.na(w_f_df)] <- 0
wf_melt <- melt(w_f_df, value.name="Count")

# Months Plot
month_plot <- ggplot(wf_melt, aes(x=Month, y=Count, fill=variable)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_hc() +
  theme_minimal()

water_plot <- ggplot(w_f_df, aes(x=Month, y=no_water_count, fill=village)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title="Reports of No Water in each Village by Month", y="Count") +
  scale_fill_brewer(name="Village") +
  annotate("text", x=4, y=25, label="Ruaca seems to have the highest \ncount of respondants reporting no water in each month", color="grey39") +
  theme(
    plot.background = element_rect(color="cornsilk2", fill="cornsilk2"),
    panel.background = element_rect(color="cornsilk2", fill="cornsilk2"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linewidth=1, color="black", linetype=3),
    legend.background = element_rect(color="white", fill="cornsilk2"),
    plot.title=element_text(size=16))

food_plot <- ggplot(w_f_df, aes(x=Month, y=lack_food_count, fill=village)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(name="Village") +
  labs(y="Count", title="Monlthy Lack of Food by Village") +
  annotate("text", x=5, y=25, 
           label="Lack of food was prevelant \nin the months October through December for all three villages",
           color="grey39") +
  theme(
    plot.background = element_rect(color="cornsilk2", fill="cornsilk2"),
    panel.background = element_rect(color="cornsilk2", fill="cornsilk2"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linewidth=1, color="black", linetype=3),
    legend.background = element_rect(color="white", fill="cornsilk2"),
    plot.title = element_text(size=16)
  )


# ------ Final Plot
final_plot <- plot_grid(water_plot, food_plot, nrow=2) +
  plot_annotation(
    title="SAFI Survey",
    subtitle="Respondants from the SAFI survey interviews between November 2016 and June 2017 of three villages in Mozambique reported \non the availability of water and food by month. Their responses indicated that August through October where the months with the \nmost instances of no water, and October through December, as well as March and May having many instances of individuals \nreporting a lack of food available. Ruaca seemed to have water unavailabe the most, while God had the most instances of lack of \nfood for each month.",
    caption="@jadenth0mas | Source: Woodhouse et. al (2018): SAFI Survey Results. doi:10.6084/m9.figshare.6262019.v1",
    theme=theme(
      plot.background = element_rect(color="cornsilk2", fill="cornsilk2"),
      plot.title = element_text(size=25),
      plot.subtitle = element_text(size=14),
      plot.title.position="panel",
      plot.caption = element_text(color="grey39", hjust=0)
    )
  )
  
ggsave("safi_survey", final_plot, device="jpeg")
