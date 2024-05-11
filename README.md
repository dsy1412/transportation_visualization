# transportation_visualization

    using ggplot2, dplyr, and others are loaded to handle data manipulation, 
    visualization, and geographic mapping


## 1 date source
### Data from five CSV files 
    tracks, 
    cities, 
    lines, 
    systems, 
    stations

## 2 image explanation
### 2.1 Data Preparation and Sources
    several CSV files such as tracks.csv, cities.csv, and lines.csv, which include key details about the transportation systems like lines, stations, and tracks.

### 2.2 Date Correction:

    The fixDates function is used to correct wrong dates in tracks and stations, ensuring the accuracy of temporal data.
    Extraction of Tracks and Lines:
    
    The makeTracks function extracts track data for each city and merges the results into the TOT data frame.
    Analysis of the Relationship between Lines, Stations, and Cities:
    
    Through merging operations, the data from tracks and stations are combined with lines and cities to create the all_tracks and all_stations data frames.
### 2.3 Generation Process
    Calculate the Total Length of Transportation Systems for Each City:
    
    The total length of the transportation systems for each city is calculated using the line length data in the all_tracks frame, utilizing dplyr's filter, group_by, and summarize functions.
    The outcome is the total length of each city's transportation system in kilometers, with the number of lines per city also recorded.
    Creating the summary_tracks Data Frame:
    
    A new data frame, summary_tracks, is created containing the city name, number of lines, and total length.

#### Analyze total length per tracks
![img.png](img.png)

#### Analyze total stations per city
![img_1.png](img_1.png)

#### Correlation analysis
![img_2.png](img_2.png)
![img_3.png](img_3.png)

#### Map visualization
![img_4.png](img_4.png)


#### London: Transport system analysis
![img_5.png](img_5.png)

#### Visualization per system_id in London
![img_6.png](img_6.png)

#### Lines length distribution in London
![img_7.png](img_7.png)


#### Tokyo: Transport system analysis
![img_8.png](img_8.png)


#### Visualization per system_id in Tokyo
![img_9.png](img_9.png)

#### Lines length distribution in Tokyo
![img_10.png](img_10.png)

#### Visualization per lines in Beijing
![img_11.png](img_11.png)

#### Lines length distribution in Beijing
![img_12.png](img_12.png)

#### London: Analysis of tracks + stations
![img_13.png](img_13.png)

#### Time evolution analysis for London
![img_14.png](img_14.png)

#### Time evolution analysis for Tokyo
![img_15.png](img_15.png)