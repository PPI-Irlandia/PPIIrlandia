library(tidyverse)
library(plotly)
library(openxlsx)
library(viridisLite)
library(tmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

data <- read.xlsx("Data_2025_2024.xlsx")
data$idx <- seq(1, nrow(data))

#### MAP ####
city_counts <- data.frame(table(data$City)) 
colnames(city_counts) <- c("City", "Freq")


Sys.setenv("SHAPE_RESTORE_SHX" = "YES")
ROI_counties = st_read("26_Counties/26 Counties/Census2011_Admin_Counties_generalised20m.shp", quiet = TRUE)
ROI_counties <- select(ROI_counties, c(Nation = NUTS1NAME, County = COUNTYNAME, pop = Total2011, ID = COUNTY))
ROI_counties$City <- gsub(" County", "", ROI_counties$County)

Ireland <- ROI_counties
Ireland <- left_join(Ireland, city_counts)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define bounding box around Ireland + neighboring areas
bbox <- st_bbox(c(xmin = -13, xmax = 0, ymin = 51, ymax = 56), crs = st_crs(world))

tmap_mode("plot")

png("png/geo_dist_2025.png", width = 10, height = 10, units = "in", res = 600)
tm_shape(world, bbox = bbox) +
  tm_polygons(col = "lightgrey", border.col = "white") +  # Background countries
  tm_shape(Ireland) +
  tm_fill(col = "Freq",
          palette = "YlOrRd",
          title = "Distribution",
          text.size = 0.5,
          style = "cont",
          position = c("top", "left")) +
  tm_borders(lwd = 0.8, col = "black") +
  tm_scale_bar(breaks = c(0, 50, 100),
               text.size = 0.5,
               position = c("right", "bottom")) +
  tm_layout(frame = FALSE,
            main.title = "Students Distribution by Geographical Location",
            main.title.size = 1,
            legend.position = c("right", "top"),  # force legend placement
            legend.outside = FALSE)    +
  tm_compass(type = "4star",
             position = c("left", "top"),
             size = 2)
dev.off()


#### University ####
data_summary <- data %>%
  count(University) %>%
  arrange(n) %>%                    
  mutate(University = factor(University, levels = unique(University)))

png("png/uni_dist_2025.png", width = 10, height = 6, units = "in", res = 600)
ggplot(data_summary, aes(x = n, y = University, fill = n)) +
  geom_col() +
  scale_fill_gradientn(colors = rev(c("red4", "red", "orange")), name = NULL) +
  labs(
    title = "Students Distribution in Irish Universities",
    x = "",
    y = ""
  ) +
  cowplot::theme_cowplot() +
  coord_cartesian(expand = FALSE)
dev.off()

#### Major ####
data_summary <- data %>%
  count(Major_General) %>%
  arrange(n) %>%                    
  mutate(Major = factor(Major_General, levels = unique(Major_General)))

png("png/major_dist_2025.png", width = 10, height = 6, units = "in", res = 600)
ggplot(data_summary, aes(x = n, y = Major, fill = n)) +
  geom_col() +
  scale_fill_gradientn(colors = rev(c("red4", "red", "orange")), name = NULL) +
  labs(
    title = "Students Majors in Irish Universities",
    x = "",
    y = ""
  ) +
  cowplot::theme_cowplot() +
  coord_cartesian(expand = FALSE)
dev.off()


#### Age ####
data_summary <- data %>%
  mutate(AgeRange = cut(
    Age,
    breaks = seq(15, max(Age, na.rm = TRUE) + 5, by = 5),
    right = TRUE,
    include.lowest = TRUE,
    labels = paste(seq(15, max(Age, na.rm = TRUE), by = 5),
                   seq(19, max(Age, na.rm = TRUE) + 4, by = 5),
                   sep = "-")
  )) %>%
  count(AgeRange)

png("png/age_dist_2025.png", width = 8, height = 8, units = "in", res = 600)
ggplot(data_summary, aes(y = n, x = AgeRange, fill = n)) +
  geom_col() +
  scale_fill_gradientn(colors = rev(c("red4", "red", "orange")), name = NULL) +
  labs(
    title = "Age distribution of Indonesian Students in Ireland",
    x = "",
    y = ""
  ) +
  cowplot::theme_cowplot() +
  coord_cartesian(expand = FALSE)
dev.off()


#### Gender ####
gender_counts <- data %>%
  count(Gender) %>%
  mutate(
    percent = round(100 * n / sum(n), 1),
    text_label = paste0(percent, "% (", n, ")")
  )

# Optional: set order if you want
desired_order <- c("Male", "Female", "Other")  # adjust as needed
gender_counts$Gender <- factor(gender_counts$Gender, levels = rev(desired_order))  # flip for top-to-bottom

# Horizontal bar chart
png("png/gender_dist_2025.png", width = 8, height = 3, units = "in", res = 600)
ggplot(gender_counts, aes(x = 1, y = n*-1, fill = Gender)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = text_label), 
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  coord_flip() +  # horizontal
  scale_fill_manual(values = c('navy', 'red4', "springgreen4")) +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top")+
  ggtitle("Gender Distribution of Indonesian Students in Ireland")
dev.off()

#### Degree ####
degree_counts <- data %>%
  count(Degree) %>%
  mutate(
    percent = round(100 * n / sum(n), 1),
    text_label = paste0(percent, "% (", n, ")")
  )

# Specify order
desired_order <- c("S1 (Undergraduate)", "S2 (Masters)", "S3 (PhD)")
degree_counts$Degree <- factor(degree_counts$Degree, levels = desired_order)

# Horizontal stacked bar chart

png("png/degree_dist_2025.png", width = 8, height = 3, units = "in", res = 600)
ggplot(degree_counts, aes(x = 1, y = n*-1, fill = Degree)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = text_label), 
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  coord_flip() +  # horizontal
  scale_fill_manual(values = c('navy', 'red4', "springgreen4")) +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top") +
  ggtitle("Education Level Distribution of Indonesian Students in Ireland")
dev.off()


#### Funding ####
data_summary <- data %>%
  count(Funding) %>%
  arrange(n) %>%                    
  mutate(Funding = factor(Funding, levels = unique(Funding)))

png("png/funding_dist_2025.png", width = 11, height = 6, units = "in", res = 600)
ggplot(data_summary, aes(x = n, y= Funding, fill = n)) +
  geom_col() +
  scale_fill_gradientn(colors = rev(c("red4", "red", "orange")), name = NULL) +
  labs(
    title = "Funding Types of Indonesian Students in Ireland",
    x = "",
    y = ""
  ) +
  cowplot::theme_cowplot() +
  coord_cartesian(expand = FALSE)
dev.off()