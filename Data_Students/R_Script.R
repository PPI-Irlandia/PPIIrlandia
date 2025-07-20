library(tidyverse)
library(plotly)
library(openxlsx)
library(viridisLite)


data <- read.xlsx("PPI_Irlandia/Pengurus_2025/PPIIrlandia/Data_Students/Students_2025_2026_Edition_July2025.xlsx")
data$idx <- seq(1, nrow(data))

data_summary <- data %>%
  count(Age_Range)

# Age
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


# Gender
gender_counts <- data %>%
  count(Gender) %>%
  mutate(
    percent = round(100 * n / sum(n), 1),
    text_label = paste0(Gender, "<br>", percent, "%<br>Count: ", n)
  )

plot_ly(gender_counts, labels = ~Gender, values = ~n, type = 'pie',
        text = ~text_label,
        textinfo = 'text',
        insidetextorientation = 'radial',
        marker = list(colors = c('#636EFA', '#EF553B', '#00CC96', '#AB63FA', '#FFA15A'))) %>%
  layout(title = 'Gender Distribution',
         showlegend = TRUE)

# Cities
city_counts <- data.frame(table(data$City)) 
colnames(city_counts) <- c("city", "Freq")

library(plotly)
library(rjson)

data_map <- fromJSON(file="https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json")

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)
url2<- "https://raw.githubusercontent.com/plotly/datasets/master/fips-unemp-16.csv"
df <- read.csv(url2, colClasses=c(fips="character"))
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
fig <- plot_ly()
fig <- fig %>% add_trace(
  type="choropleth",
  geojson=counties,
  locations=df$fips,
  z=df$unemp,
  colorscale="Viridis",
  zmin=0,
  zmax=12,
  marker=list(line=list(
    width=0)
  )
)
fig <- fig %>% colorbar(title = "Unemployment Rate (%)")
fig <- fig %>% layout(
  title = "2016 US Unemployment by County"
)

fig <- fig %>% layout(
  geo = g
)

fig