library(tidyverse)
library(plotly)
library(openxlsx)
library(viridisLite)


data <- read.xlsx("PPI_Irlandia/Pengurus_2025/PPIIrlandia/Data_Students/")
data$idx <- seq(1, nrow(data))



#### Age ####
data_summary <- data %>%
  count(Age_Range)

plot_ly(data = data_summary,
        x = ~Age_Range,
        y = ~n,
        type = 'bar',
        marker = list(
          color = ~n,
          colorscale = "Plasma"
        ),
        text = ~paste(n),
        hoverinfo = 'text') %>%
  layout(title = "Age Ranges",
         xaxis = list(title = "Age Range", tickangle = -45),
         yaxis = list(title = ""),
         bargap = 0.2,
         plot_bgcolor = "#ffffff",
         paper_bgcolor = "#ffffff")


#### Gender ####
gender_counts <- data %>%
  count(Gender) %>%
  mutate(
    percent = round(100 * n / sum(n), 1),
    text_label = paste0(Gender, "<br>", percent, "%<br>Count: ", n)
  )

plot_ly(gender_counts, labels = ~Gender, values = ~n, type = 'pie',
        hole = 0.4,  # this makes it a ring/donut chart
        text = ~text_label,
        textinfo = 'text',
        insidetextorientation = 'radial',
        marker = list(colors = c('green', 'blue', '#00CC96', '#AB63FA', '#FFA15A'))) %>%
  layout(title = 'Gender Distribution',
         showlegend = TRUE)

#### Degree ####
degree_counts <- data %>%
  count(Degree) %>%
  mutate(
    percent = round(100 * n / sum(n), 1),
    text_label = paste0(Degree, "<br>", percent, "%<br>Count: ", n)
  )

# Define desired order
desired_order <- c("S1 (Undergraduate)", "S2 (Masters)", "S3 (PhD)")
degree_counts$Degree <- factor(degree_counts$Degree, levels = desired_order)
degree_counts$text_label <- factor(degree_counts$text_label, levels = c("S1 (Undergraduate)<br>10.9%<br>Count: 10",
                                                                    "S2 (Masters)<br>65.2%<br>Count: 60",
                                                                    "S3 (PhD)<br>23.9%<br>Count: 22"))
degree_counts <- degree_counts %>% arrange(Degree)

plot_ly(degree_counts,
        labels = ~as.character(Degree), 
        hole = 0.4,
        values = ~n,
        type = 'pie',
        text = ~text_label,
        textinfo = 'text',
        insidetextorientation = 'horizontal',
        marker = list(colors = c('orange', 'blue', "green"))) %>%
  layout(title = 'Degree Distribution',
         showlegend = TRUE,
         legend = list(categoryorder = "array",
                       categoryarray = desired_order))

#### Cities ####
city_counts <- data.frame(table(data$City)) 
colnames(city_counts) <- c("City", "Freq")

library(tmap)
library(sf)

Sys.setenv("SHAPE_RESTORE_SHX" = "YES")
ROI_counties = st_read("PPI_Irlandia/Pengurus_2025/PPIIrlandia/Data_Students/26_Counties/26 Counties/Census2011_Admin_Counties_generalised20m.shp")
ROI_counties <- select(ROI_counties, c(Nation = NUTS1NAME, County = COUNTYNAME, pop = Total2011, ID = COUNTY))
ROI_counties$City <- gsub(" County", "", ROI_counties$County)

Ireland <- ROI_counties
Ireland <- left_join(Ireland, city_counts)

tmap_mode("view")

Ireland %>%
  tm_shape() +
  tm_fill(col = "Freq",
          palette = "YlOrRd",          # Yellow to Red palette
          title = "Distribution",
          text.size = 0.5, 
          style = "cont",
          position = c("top", "left")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 0.5,
               position = c("right", "bottom")) +
  tm_layout(frame = FALSE)
#### University ####
data_summary <- data %>%
  count(University) %>%
  arrange(n) %>%                    
  mutate(University = factor(University, levels = unique(University)))

plot_ly(data = data_summary,
        x = ~n,
        y = ~University,
        type = 'bar',
        orientation = 'h',                # horizontal bars to show universities on y-axis
        marker = list(
          color = ~n,
          colorscale = "Plasma"
        ),
        text = ~paste(n),
        hoverinfo = 'text') %>%
  layout(title = "Students Distribution in Irish Universities",
         xaxis = list(title = "Count"),
         yaxis = list(title = "", categoryorder = "array", categoryarray = levels(data_summary$University)),
         bargap = 0.2,
         plot_bgcolor = "#ffffff",
         paper_bgcolor = "#ffffff")

#### Major ####
data_summary <- data %>%
  count(Major_general) %>%
  arrange(n) %>%                    
  mutate(Major = factor(Major_general, levels = unique(Major_general)))

plot_ly(data = data_summary,
        x = ~n,
        y = ~Major,
        type = 'bar',
        orientation = 'h',                # horizontal bars to show universities on y-axis
        marker = list(
          color = ~n,
          colorscale = "Plasma"
        ),
        text = ~paste(n),
        hoverinfo = 'text') %>%
  layout(title = "Students Majors in Irish Universities",
         xaxis = list(title = "Count"),
         yaxis = list(title = "", categoryorder = "array", categoryarray = levels(data_summary$University)),
         bargap = 0.2,
         plot_bgcolor = "#ffffff",
         paper_bgcolor = "#ffffff")