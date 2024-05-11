# Load necessary libraries
rm(list=ls())  # Clear the environment
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(ggthemes)
library(RColorBrewer)
library(ggmap)
library(ggrepel)

# Data overview
# The data is separated into 5 files: cities.csv, lines.csv, stations.csv, systems.csv, and tracks.csv
# Each file contains different aspects of the transport systems like coordinates, start year, line ids, names, and geometry.

# Load the data files
set.seed(1234)
tracks <- read.csv('D:/transportation/transportation_visualization/transportation/tracks.csv', sep = ',')
cities <- read.csv('D:/transportation/transportation_visualization/transportation/cities.csv', sep = ',')
lines <- read.csv('D:/transportation/transportation_visualization/transportation/lines.csv', sep = ',')
systems <- read.csv('D:/transportation/transportation_visualization/transportation/systems.csv', sep = ',')
stations <- read.csv('D:/transportation/transportation_visualization/transportation/stations.csv', sep = ',')

# Fixing the incorrect dates in the data
fixDates <- function(x) {
  if (!is.na(x)) {
    if (x %in% c(0, 9999, 99999, 999999, 999999999)) {
      return(NA)
    } else if (x == 19160) {
      return(1916)
    } else {
      return(x)
    }
  } else {
    return(NA)
  }
}

tracks$buildstart <- sapply(tracks$buildstart, fixDates)
tracks$opening <- sapply(tracks$opening, fixDates)
tracks$closure <- sapply(tracks$closure, fixDates)
stations$buildstart <- sapply(stations$buildstart, fixDates)
stations$opening <- sapply(stations$opening, fixDates)

# Function to extract longitude and latitude of tracks per city
makeTracks <- function(id_city) {
  selectedLines <- lines %>% filter(city_id == id_city)
  selectedIds <- selectedLines$id
  res <- data.frame(Long = double(), Lat = double(), ids = integer(), buildDATE = integer(), openingDATE = integer(), marker = integer())

  for (line in 1:length(selectedIds)) {
    scanLine <- selectedIds[line]
    currentLine <- tracks %>% filter(line_id == scanLine)
    if (exists('currentLine') && nrow(currentLine) > 0) {
      res_2 <- data.frame(Long = double(), Lat = double(), ids = integer())

      for (tr in 1:nrow(currentLine)) {
        x <- as.character(currentLine$geometry[tr])
        x <- gsub('LINESTRING\\(', '', x)
        x <- gsub('\\)', '', x)
        id <- currentLine$id[tr]
        cur_long <- c()
        cur_lat <- c()
        cur_id <- c()
        openingDate <- currentLine$opening[tr]
        buildDate <- currentLine$buildstart[tr]

        for (i in 1:length(strsplit(x, ',')[[1]])) {
          cur_long[i] <- as.numeric(strsplit(strsplit(x, ',')[[1]][[i]], ' ')[[1]][1])
          cur_lat[i] <- as.numeric(strsplit(strsplit(x, ',')[[1]][[i]], ' ')[[1]][2])
          cur_id[i] <- id
        }
        build <- rep.int(buildDate, length(cur_id))
        opening <- rep.int(openingDate, length(cur_id))
        tempo <- data.frame('Long' = cur_long, 'Lat' = cur_lat, 'ids' = cur_id, 'buildDATE' = build, 'openingDATE' = opening)
        res_2 <- rbind(res_2, tempo)
      }
    }
    if (exists('res_2') && nrow(res_2) > 0) {
      res_2$line <- as.factor(rep.int(scanLine, nrow(res_2)))
      temp <- c(1)
      temp <- append(temp, rep(0, nrow(res_2) - 1))
      res_2$marker <- temp
      res <- rbind(res, res_2)
    }
  }
  name <- (cities %>% filter(id == id_city))$url_name
  country <- (cities %>% filter(id == id_city))$country
  res$name <- rep.int(name, nrow(res))
  res$country <- rep.int(country, nrow(res))
  return(res)
}

# Function to calculate total length of lines per city
makeStats <- function(id_city) {
  selectedLines <- lines %>% filter(city_id == id_city)
  selectedIds <- selectedLines$id
  current <- tracks %>% filter(line_id %in% selectedIds)
  statisticsDF <- as.data.frame(
    merge(
      current %>% group_by(line_id) %>% select(line_id, length) %>% summarize(totLength = sum(length) / 1000) %>% rename(id = line_id),
      lines %>% filter(city_id == id_city), by = 'id'))
  return(statisticsDF)
}

# Function to plot data by built and opened date
makePlots <- function(currentDF) {
  listPlot <- list()
  listPlot[[1]] <- ggplot(data = currentDF, aes(x = Long, y = Lat, color = buildDATE)) + geom_point(size = 1, alpha = 1) + theme_fivethirtyeight() + theme(legend.position = 'top', legend.text = element_text(angle = 45, hjust = 1, size = 8)) + xlab('') + ylab('') + scale_color_gradientn(name = 'built in ', colors = rev(brewer.pal(10, 'Spectral')))
  listPlot[[2]] <- ggplot(data = currentDF, aes(x = Long, y = Lat, color = openingDATE)) + geom_point(size = 1, alpha = 1) + theme_fivethirtyeight() + theme(legend.position = 'top', legend.text = element_text(angle = 45, hjust = 1, size = 8)) + xlab('') + ylab('') + scale_color_gradientn(name = 'opened in ', colors = rev(brewer.pal(10, 'Spectral')))
  return(listPlot)
}

# Custom color palette for Lines display
mycols <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#B15928")

print(class(lines))
allCities <- sort(unique(lines$city_id))

# Analyzing all lines across cities

TOT <- data.frame(Long = double(), Lat = double(), ids = integer(), buildDATE = integer(), openingDATE = integer(), city = character(), country = character())


for (i in 1:length(allCities)) {
  temp <- makeTracks(allCities[i])
  if (nrow(temp) > 0) {
    TOT <- rbind(TOT, temp)
  }
}

allCitiesName <- unique(TOT$name)
listCity <- list()
for (i in 1:length(allCitiesName)) {
  listCity[[i]] <- ggplot(data = filter(TOT, name == allCitiesName[i]), aes(x = Long, y = Lat, color = as.factor(line))) +
    geom_point(size = .2, alpha = 1) +
    theme_fivethirtyeight() +
    theme(legend.position = 'none', legend.text = element_text(angle = 45, hjust = 1, size = 8), plot.title = element_text(size = 12), axis.title = element_blank(), axis.text = element_blank()) + ggtitle(allCitiesName[i])
}

# Relationship between number of lines, stations and total length of Transport system
lines$line_id <- lines$id
tempo <- merge(tracks, lines, by = 'id')
cities$city_id <- cities$id
tempo <- merge(tempo, cities, by = 'city_id')



# Group the tracks and stations
all_tracks <- merge(tracks %>% select(line_id, length), tempo %>% select(country, name.y, id.x) %>% rename(line_id = id.x), by = 'line_id')
all_stations <- merge(stations %>% select(line_id), tempo %>% select(country, name.y, id.x) %>% rename(line_id = id.x), by = 'line_id')






# Analyze total length per tracks
tot_length <- c()
tot_lines <- c()
names <- c()
for (i in 1:length(unique(all_tracks$name.y))) {
  currentCity <- unique(all_tracks$name.y)[i]
  temp <- all_tracks %>% dplyr::filter(name.y == currentCity) %>% dplyr::group_by(line_id) %>% dplyr::summarize(totLine = sum(length) / 1000)
  names[i] <- as.character(currentCity)
  tot_length[i] <- sum(temp$totLine)
  tot_lines[i] <- length(unique(temp$line_id))
}

summary_tracks <- data.frame(city = names, lines = tot_lines, length = tot_length)
summary_tracks %>% ggplot(aes(x = reorder(city, length), y = length, fill = length)) + geom_bar(stat = 'identity') + coord_flip() + theme_fivethirtyeight() + scale_fill_gradientn(name = '', colors = rev(brewer.pal(10, 'Spectral'))) + theme(legend.position = 'none') + ggtitle("Length(km) of Transport System per City")








# Analyze total stations per city
tot_stations <- c()
names <- c()
for (i in 1:length(unique(all_stations$name.y))) {
  currentCity <- unique(all_stations$name.y)[i]
  temp <- all_stations %>% dplyr::filter(name.y == currentCity)
  names[i] <- as.character(currentCity)
  tot_stations[i] <- nrow(temp)
}
summary_stations <- data.frame(city = names, stations = tot_stations)
summary_stations %>% ggplot(aes(x = reorder(city, stations), y = stations, fill = stations)) + geom_bar(stat = 'identity') + coord_flip() + theme_fivethirtyeight() + scale_fill_gradientn(name = '', colors = rev(brewer.pal(10, 'Spectral'))) + ggtitle("Number of stations per City")









# Correlation analysis
RES <- as.data.frame(merge(summary_tracks, summary_stations, by = 'city'))
g2 <- ggplot(data = RES, aes(x = length, y = lines, fill = city)) + geom_point(aes(size = stations)) +
  geom_label_repel(aes(label = city),
                   fontface = 'bold', color = 'black',
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines"),
                   segment.color = 'grey50', alpha = .5) +
  theme(legend.position = 'bottom') +
  scale_fill_manual(values = colorRampPalette(mycols)(length(unique(RES$city))))
g2 + guides(fill = FALSE) + xlab('total length of Transport System (km)') + ylab("Number of lines")


ggplot(data = RES, aes(x = length, y = lines)) +
  geom_point(aes(size = stations)) +
  theme(legend.position = 'none') +
  geom_smooth(method = 'lm', color = '#F21A00', alpha = .25, size = .5, lty = 2) +
  theme_fivethirtyeight() + ggtitle('Number of stations vs. Total Length')







# Map visualization
cities_data <- data.frame(city = unique(RES$city))
cities_data$city <- as.character(cities_data$city)
# Fix a typo for Lyon (no 's')
cities_data$city[18] <- 'Lyon'
# Append longitude and latitude (done offline)
cities_data$long <- c(2.173404, 116.4074, 13.40495, -2.934985, -74.07209, 19.04023, -58.38156, -87.6298, -73.04439, -3.188267, 8.946256, 15.4395, -103.3496, 11.4041, -77.04275, -9.139337, -0.1277583, 4.835659, -3.70379, 5.36978, -99.13321, 9.189982, -1.553621, -81.79481, -1.61778, 2.352222, 14.4378, -1.677793, 12.49637, -1.981231, -70.66927, -46.63331, 121.4737, 18.06858, 139.6917, 12.31552, -120.7401, -0.8890853)
cities_data$lat <- c(41.38506, 39.9042, 52.52001, 43.26301, 4.710989, 47.49791, -34.60368, 41.87811, -36.82014, 55.95325, 44.40565, 47.07071, 20.6597, 47.26921, -12.04637, 38.72225, 51.50735, 45.76404, 40.41678, 43.29648, 19.43261, 45.4642, 47.21837, 26.14204, 54.97825, 48.85661, 50.07554, 48.11727, 41.90278, 43.31833, -33.44889, -23.55052, 31.23039, 59.32932, 35.68949, 45.44085, 47.75107, 41.64882)
RES <- data.frame(merge(RES, cities_data, by = 'city'))
# Get a world map
countries_map <- map_data("world")
world_map <- ggplot() +
  geom_map(data = countries_map,
           map = countries_map, aes(x = long, y = lat, map_id = region, group = group),
           fill = "white", color = "black", size = 0.1)

world_map +
  geom_point(data = RES, aes(x = long, y = lat, size = stations, color = length), alpha = .85) +
  theme_fivethirtyeight() +
  scale_color_gradientn(name = '', colors = rev(brewer.pal(10, 'Spectral'))) +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none', legend.text = element_text(size = 8, angle = 45)) +
  guides(size = F) +
  geom_label_repel(data = RES, aes(x = long, y = lat, label = city), force = 5, alpha = .75, color = 'black') +
  ggtitle('Cities where data is available')
















# London: Transport system analysis
londonData <- makeTracks(69)
londonData$label <- ifelse(londonData$marker == 1, as.character(londonData$line), "")

# Visualization per lines in London
get_map(location = c(lon = median(londonData$Long), lat = median(londonData$Lat)), color = "bw", zoom = 10) %>%
  ggmap() +
  geom_point(data = londonData, aes(x = Long, y = Lat, color = line), size = .5, alpha = .75) +
  theme(legend.position = 'right') +
  xlab('') + ylab('')

ggplot(data = londonData, aes(x = Long, y = Lat, color = as.factor(line))) +
  geom_point(size = 1, alpha = 1) +
  geom_label_repel(data = londonData, aes(x = Long, y = Lat, color = as.factor(line), label = label)) +
  theme_fivethirtyght() +
  theme(legend.position = 'top', legend.text = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab('') + ylab('') + theme(legend.position = 'none') +
  ggtitle(paste0('Transport System by Lines : ', londonData$name)) +
  scale_color_manual(values = colorRampPalette(mycols)(length(unique(londonData$line))))

# Visualization per built/opening date in London
do.call(grid.arrange, c(makePlots(londonData), ncol = 2))












# Visualization per system_id in London
londonData <- as.data.frame(merge(lines %>% rename(line = id), londonData, by = 'line'))
ggplot(data = londonData, aes(x = Long, y = Lat, color = as.factor(system_id))) +
  geom_point(size = 1, alpha = 1) +
  theme_fivethirtyeight() +
  theme(legend.position = 'top', legend.text = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab('') + ylab('') + theme(legend.position = 'none') + ggtitle('London Transport System by system_id') +
  facet_wrap(~system_id) +
  scale_color_manual(values = colorRampPalette(mycols)(length(unique(londonData$system_id))))







# Lines length distribution in London
ggplot(data = makeStats(69), aes(x = reorder(url_name, totLength), y = totLength, fill = totLength)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_fivethirtyeight() +
  scale_fill_gradientn(name = 'km', colors = rev(brewer.pal(10, 'Spectral')))





# Tokyo: Transport system analysis
tokyoData <- makeTracks(114)
tokyoData$label <- ifelse(tokyoData$marker == 1, as.character(tokyoData$line), "")
# Visualization per lines in Tokyo
ggplot(data = tokyoData, aes(x = Long, y = Lat, color = as.factor(line))) +
  geom_point(size = 1, alpha = 1) +
  geom_label_repel(data = tokyoData, aes(x = Long, y = Lat, color = as.factor(line), label = label)) +
  theme_fivethirtyeight() +
  theme(legend.position = 'top', legend.text = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab('') + ylab('') + theme(legend.position = 'none') + ggtitle('Tokyo Transport System by Lines') +
  scale_color_manual(values = colorRampPalette(mycols)(length(unique(tokyoData$line))))

# Visualization per built/opening date in Tokyo
do.call(grid.arrange, c(makePlots(tokyoData), ncol = 2))









# Visualization per system_id in Tokyo
tokyoData <- as.data.frame(merge(lines %>% rename(line = id), tokyoData, by = 'line'))
ggplot(data = tokyoData, aes(x = Long, y = Lat, color = as.factor(system_id))) +
  geom_point(size = 1, alpha = 1) +
  theme_fivethirtyeight() +
  theme(legend.position = 'top', legend.text = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab('') + ylab('') + theme(legend.position = 'none') + ggtitle('Tokyo Transport System by system_id') +
  facet_wrap(~system_id) +
  scale_color_manual(values = colorRampPalette(mycols)(length(unique(tokyoData$system_id))))







# Lines length distribution in Tokyo
df <- makeStats(114)[1:50, ]

# Adjust the plot
ggplot(data = df, aes(x = reorder(url_name, totLength), y = totLength, fill = totLength)) +
  geom_bar(stat = 'identity') +
  coord_flip() +  # Flips the coordinates to make labels horizontal
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(size = 8, angle = 0)) +  # Adjust text size and angle
  scale_fill_gradientn(name = 'km', colors = rev(brewer.pal(10, 'Spectral'))) +
  labs(y = NULL)  # Removes y-axis label for clarity






# Beijing: Transport system analysis
beijingData <- makeTracks(15)
beijingData$label <- ifelse(beijingData$marker == 1, as.character(beijingData$line), "")

# Visualization per lines in Beijing
ggplot(data = beijingData, aes(x = Long, y = Lat, color = as.factor(line))) +
  geom_point(size = 1, alpha = 1) +
  geom_label_repel(data = beijingData, aes(x = Long, y = Lat, color = as.factor(line), label = label)) +
  theme_fivethirtyeight() +
  theme(legend.position = 'top', legend.text = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab('') + ylab('') + theme(legend.position = 'none') + ggtitle('Beijing Transport System by Lines') +
  scale_color_manual(values = colorRampPalette(mycols)(length(unique(beijingData$line))))



# Visualization per built/opening date in Beijing
do.call(grid.arrange, c(makePlots(beijingData), ncol = 2))

# Visualization per system_id in Beijing
beijingData <- as.data.frame(merge(lines %>% rename(line = id), beijingData, by = 'line'))
ggplot(data = beijingData, aes(x = Long, y = Lat, color = as.factor(system_id))) +
  geom_point(size = 1, alpha = 1) +
  theme_fivethirtyeight() +
  theme(legend.position = 'top', legend.text = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab('') + ylab('') + theme(legend.position = 'none') + ggtitle('Beijing Transport System by system_id') +
  facet_wrap(~system_id) +
  scale_color_manual(values = colorRampPalette(mycols)(length(unique(beijingData$system_id))))





# Lines length distribution in Beijing
ggplot(data = makeStats(15), aes(x = reorder(url_name, totLength), y = totLength, fill = totLength)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_fivethirtyeight() +
  scale_fill_gradientn(name = 'km', colors = rev(brewer.pal(10, 'Spectral')))




# Paris: Transport system analysis
parisData <- makeTracks(95)
parisData$label <- ifelse(parisData$marker == 1, as.character(parisData$line), "")

# Visualization per lines in Paris
ggplot(data = parisData, aes(x = Long, y = Lat, color = as.factor(line))) +
  geom_point(size = 1, alpha = 1) +
  geom_label_repel(data = parisData, aes(x = Long, y = Lat, color = as.factor(line), label = label)) +
  theme_fivethirtyeight() +
  theme(legend.position = 'top', legend.text = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab('') + ylab('') + theme(legend.position = 'none') + ggtitle('Paris Transport System by Lines') +
  scale_color_manual(values = colorRampPalette(mycols)(length(unique(parisData$line))))



# Visualization per built/opening date in Paris
do.call(grid.arrange, c(makePlots(parisData), ncol = 2))

# Visualization per system_id in Paris
parisData <- as.data.frame(merge(lines %>% rename(line = id), parisData, by = 'line'))
ggplot(data = parisData, aes(x = Long, y = Lat, color = as.factor(system_id))) +
  geom_point(size = 1, alpha = 1) +
  theme_fivethirtyeight() +
  theme(legend.position = 'top', legend.text = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab('') + ylab('') + theme(legend.position = 'none') + ggtitle('Paris Transport System by system_id') +
  facet_wrap(~system_id) +
  scale_color_manual(values = colorRampPalette(mycols)(length(unique(parisData$system_id))))

# Lines length distribution in Paris
ggplot(data = makeStats(95), aes(x = reorder(url_name, totLength), y = totLength, fill = totLength)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_fivethirtyeight() +
  scale_fill_gradientn(name = 'km', colors = rev(brewer.pal(10, 'Spectral')))

# Stations data analysis
# This dataset is similar to 'tracks' so the decoding of data will be more or less the same.
# No internal loop over the length of station (as it was for the length of section), meaning 1 station = 1 geo coordinate
# Some station have a name, some have empty strings

# Function to analyze stations data
makeStations <- function(id_city) {
  selectedLines <- lines %>% filter(city_id == id_city)
  selectedIds <- selectedLines$id
  res <- data.frame(Long = double(), Lat = double(), line_id = integer(), buildDATE = integer(), openingDATE = integer(), name = character())

  for (line in 1:length(selectedIds)) {
    scanStation <- selectedIds[line]
    currentStation <- stations %>% filter(line_id == scanStation)
    if (exists('currentStation') && nrow(currentStation) > 0) {
      res_2 <- data.frame(Long = double(), Lat = double(), line_id = integer(), buildDATE = integer(), openingDATE = integer(), name = character())
      cur_long <- c()
      cur_lat <- c()
      cur_id <- c()
      cur_name <- c()
      build <- c()
      opening <- c()
      for (st in 1:nrow(currentStation)) {
        x <- as.character(currentStation$geometry[st])
        x <- gsub('POINT\\(', '', x)
        x <- gsub('\\)', '', x)
        id <- currentStation$line_id[st]
        openingDate <- currentStation$opening[st]
        buildDate <- currentStation$buildstart[st]
        cur_long[st] <- as.numeric(strsplit(x, ' ')[[1]][[1]])
        cur_lat[st] <- as.numeric(strsplit(x, ' ')[[1]][[2]])
        cur_id[st] <- id
        build[st] <- buildDate
        opening[st] <- openingDate
        cur_name[st] <- as.character(currentStation$name[st])
      }
      tempo <- data.frame('Long' = cur_long, 'Lat' = cur_lat, 'line_id' = cur_id, 'buildDATE' = build, 'openingDATE = opening', 'name' = cur_name)
      res_2 <- rbind(res_2, tempo)
    }
    if (exists('res_2') && nrow(res_2) > 0) {
      res <- rbind(res, res_2)
    }
  }
  return(res)
}



# London: Analysis of tracks + stations
londonStations <- makeStations(69)

# Visualization of London's transport system by lines with station data
g1 <- ggplot(data = londonData, aes(x = Long, y = Lat, color = as.factor(line))) +
  geom_point(size = .25, alpha = .75) +
  theme_fivethirtyeight() +
  theme(legend.position = 'top', legend.text = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab('') + ylab('') +
  theme(legend.position = 'none') +
  ggtitle('Transport System by Lines : London') +
  scale_color_manual(values = colorRampPalette(mycols)(length(unique(londonData$line))))

g1 + geom_point(data = londonStations, aes(x = Long, y = Lat), color = 'black', size = 1)

# Time evolution analysis
# Another interesting feature to look at is how the number of tracks, the total length of the transport system increased during time.
# The code is similar to the previous sections:
# Select a 'city' by its 'id' in order to get the corresponding 'line_id'
# 'group_by(line_id, year)'
# Calculate the sum (in case a given line had opened more than 1 section per year), then calculate the cumulated sum by 'line'
# Plot overall increase and breakdown by lines






# Function to analyze time evolution of lines per city
makeLinesBreakdown <- function(id_city, city_name) {
  listPlot <- list()
  selectedLines <- lines %>% filter(city_id == id_city)
  selectedIds <- selectedLines$id
  listPlot[[1]] <- tracks %>%
    filter(line_id %in% selectedIds) %>%
    select(line_id, opening, length) %>%
    group_by(opening) %>%
    summarize(totlength = sum(length) / 1000) %>%
    mutate(cumulativeTot = cumsum(totlength)) %>%
    ggplot(aes(x = opening)) + geom_line(aes(y = cumulativeTot)) +
    theme(plot.title = element_text(size = 14), legend.position = 'top') +
    geom_point(aes(y = cumulativeTot), color = 'black') +
    theme_fivethirtyeight() +
    ggtitle(paste0("Transport system's length (km) per opening's year\n for ", city_name))

  listPlot[[2]] <- tracks %>%
    filter(line_id %in% selectedIds) %>%
    select(line_id, opening, length) %>%
    group_by(line_id, opening) %>%
    summarize(totlength = sum(length) / 1000) %>%
    mutate(cumulativeTot = cumsum(totlength)) %>%
    ggplot(aes(x = opening)) + geom_line(aes(y = cumulativeTot, color = factor(line_id))) +
    theme_fivethirtyeight() +
    theme(plot.title = element_text(size = 14), legend.position = 'none') +
    geom_point(aes(y = cumulativeTot), color = 'black') +
    ggtitle(paste0("Tracks's length (km) per opening's year\n for ", city_name))
  return(listPlot)
}

# Time evolution analysis for London
do.call(grid.arrange, c(makeLinesBreakdown(69, "London"), ncol = 2))







# Interesting findings for London:
# Quick increase at the end of the 19th century (industrial revolution)
# A 'plateau' starting around 1950. According to [wikipedia](https://en.wikipedia.org/wiki/History_of_London#1945.E2.80.932000):
# 'In the immediate postwar years housing was a major issue in London, due to the large amount of housing which had been destroyed in the war'. So it looks that the slowdown was maybe due to the urgency to rebuild the city itself (buildings) rather than the Transport system.
# Another quick increase in the last 10 years.

# Time evolution analysis for Tokyo
do.call(grid.arrange, c(makeLinesBreakdown(114, "Tokyo"), ncol = 2))











# Updated insights for Tokyo (as of '2017-08-05'):
# Quick increase during the pre WW-II era (~1925)
# Some key dates from [wikipedia](https://en.wikipedia.org/wiki/History_of_rail_transport_in_Japan):
# 1925 – Introduction of automatic couplers to national network
# 1925 – Inauguration of the Yamanote Loop Line
# 1927 – Opening of Tokyo subway, the first subway in the East
# It's interesting to notice that these lines created during before WW-II did not receive an update, i.e., the increase of the whole transportation has been through new lines
# A plateau during the WW-II period

# Time evolution: Comparison by cities
allCities <- sort(unique(lines$city_id))
RES <- data.frame(opening = double(), totLength = double(), cumulativeLength = double(), city = character())
for (i in 1:length(allCities)) {
  selectedLines <- lines %>% filter(city_id == allCities[i])
  selectedIds <- selectedLines$id
  temp <- as.data.frame(tracks %>%
    filter(line_id %in% selectedIds) %>%
    select(line_id, opening, length) %>%
    group_by(opening) %>%
    summarize(totlength = sum(length) / 1000) %>%
    mutate(cumulativeTot = cumsum(totlength)))
  if (nrow(temp) > 0) {
    name <- (cities %>%
      filter(id == allCities[i]))$url_name
    temp$city <- rep.int(name, nrow(temp))
    RES <- rbind(RES, temp)
  }
}

# Plot time evolution of transport systems across cities
ggplot(data = RES, aes(x = opening, y = cumulativeTot, color = city)) +
  geom_line() + geom_point(color = 'black', size = .25) +
  theme_fivethirtyeight() +
  theme(legend.position = 'top') +
  ggtitle("Transport System's length (km) per opening's year") +
  labs(color = '')

# Comments: We observe different behavior:
# Some older cities have a quick start (London, Tokyo) during the second part of the 19th century, somehow a plateau around WW2, then another quick increase in their Transport system length
# Some 'younger' cities for which the length of their Transport System is totally exploding in recent years (Shanghai, Beijing)
