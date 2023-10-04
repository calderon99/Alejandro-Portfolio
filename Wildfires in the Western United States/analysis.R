library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(mapproj)

library(assertive, warn.conflicts = FALSE)

usda_data_1 <- data.frame(read.csv('data/FS USDA Dataset/us_fires/us_fires_1.csv'))
usda_data_2 <- data.frame(read.csv('data/FS USDA Dataset/us_fires/us_fires_2.csv'))

usda_data <- rbind(usda_data_1, 
                   usda_data_2)
columns_of_interest_usda <- c("fire_year",
                              "stat_cause_descr",
                              "fire_size",
                              "latitude",
                              "longitude",
                              "state")
usda_data <- usda_data[columns_of_interest_usda]

stanford_data <- data.frame(read.csv('./data/Stanford Dataset/Fires.csv'))
columns_of_interest_stanford <- c("INCIDENT_NAME",
                                  "POO_LATITUDE",
                                  "POO_LONGITUDE",
                                  "POO_STATE_CODE",
                                  "CAUSE_DESCRIPTION",
                                  "COMMERCIAL_DESTROYED",
                                  "COMMERCIAL_DAMAGED",
                                  "COMMERCIAL_THREATENED_72",
                                  "MFR_DESTROYED",
                                  "MFR_DAMAGED",
                                  "MFR_THREATENED_72",
                                  "MIXED_USE_DESTROYED",
                                  "MIXED_USE_DAMAGED",
                                  "MIXED_USE_THREATENED_72",
                                  "SFR_DESTROYED",
                                  "SFR_DAMAGED",
                                  "SFR_THREATENED_72",
                                  "OUTBUILDINGS_DESTROYED",
                                  "OUTBUILDINGS_DAMAGED",
                                  "OUTBUILDINGS_THREATENED_72")
stanford_data <- stanford_data[columns_of_interest_stanford]

## Subsection 2.2 Part 1

#Stanford descriptive Statistics

commercial_destroyed_summary <- list(mean(stanford_data$COMMERCIAL_DESTROYED, na.rm = T), min(stanford_data$COMMERCIAL_DESTROYED, na.rm = T), max(stanford_data$COMMERCIAL_DESTROYED, na.rm = T))
commercial_damaged_summary <- list(mean(stanford_data$COMMERCIAL_DAMAGED, na.rm = T), min(stanford_data$COMMERCIAL_DAMAGED, na.rm = T), max(stanford_data$COMMERCIAL_DAMAGED, na.rm = T))
commercial_threatened_72_summary <- list(mean(stanford_data$COMMERCIAL_THREATENED_72, na.rm = T), min(stanford_data$COMMERCIAL_THREATENED_72, na.rm = T), max(stanford_data$COMMERCIAL_THREATENED_72, na.rm = T))

mfr_destroyed_summary <- list(mean(stanford_data$MFR_DESTROYED, na.rm = T), min(stanford_data$MFR_DESTROYED, na.rm = T), max(stanford_data$MFR_DESTROYED, na.rm = T))
mfr_damaged_summary <- list(mean(stanford_data$MFR_DAMAGED, na.rm = T), min(stanford_data$MFR_DAMAGED, na.rm = T), max(stanford_data$MFR_DAMAGED, na.rm = T))
mfr_threatened_72_summary <- list(mean(stanford_data$MFR_THREATENED_72, na.rm = T), min(stanford_data$MFR_THREATENED_72, na.rm = T), max(stanford_data$MFR_THREATENED_72, na.rm = T))

mixed_use_destroyed_summary <- list(mean(stanford_data$MIXED_USE_DESTROYED, na.rm = T), min(stanford_data$MIXED_USE_DESTROYED, na.rm = T), max(stanford_data$MIXED_USE_DESTROYED, na.rm = T))
mixed_use_damaged_summary <- list(mean(stanford_data$MIXED_USE_DAMAGED, na.rm = T), min(stanford_data$MIXED_USE_DAMAGED, na.rm = T), max(stanford_data$MIXED_USE_DAMAGED, na.rm = T))
mixed_use_threatened_summary <- list(mean(stanford_data$MIXED_USE_THREATENED_72, na.rm = T), min(stanford_data$MIXED_USE_THREATENED_72, na.rm = T), max(stanford_data$MIXED_USE_THREATENED_72, na.rm = T))

sfr_destroyed_summary <- list(mean(stanford_data$SFR_DESTROYED, na.rm = T), min(stanford_data$SFR_DESTROYED, na.rm = T), max(stanford_data$SFR_DESTROYED, na.rm = T))
sfr_damaged_summary <-list(mean(stanford_data$SFR_DAMAGED, na.rm = T), min(stanford_data$SFR_DAMAGED, na.rm = T), max(stanford_data$SFR_DAMAGED, na.rm = T))
sfr_threatened_summary <- list(mean(stanford_data$SFR_THREATENED_72, na.rm = T), min(stanford_data$SFR_THREATENED_72, na.rm = T), max(stanford_data$SFR_THREATENED_72, na.rm = T))

outbuildings_destroyed_summary <- list(mean(stanford_data$OUTBUILDINGS_DESTROYED, na.rm = T), min(stanford_data$OUTBUILDINGS_DESTROYED, na.rm = T), max(stanford_data$OUTBUILDINGS_DESTROYED, na.rm = T))
outbuildings_damaged_summary <-list(mean(stanford_data$OUTBUILDINGS_DAMAGED, na.rm = T), min(stanford_data$OUTBUILDINGS_DAMAGED, na.rm = T), max(stanford_data$OUTBUILDINGS_DAMAGED, na.rm = T))
outbuildings_threatened_summary <-list(mean(stanford_data$OUTBUILDINGS_THREATENED_72, na.rm = T), min(stanford_data$OUTBUILDINGS_THREATENED_72, na.rm = T), max(stanford_data$OUTBUILDINGS_THREATENED_72, na.rm = T))

stanford_descriptive_statistics <- list("commercial_summary" = c(commercial_damaged_summary, commercial_destroyed_summary, commercial_threatened_72_summary),
                                        "mfr_summary" = c(mfr_damaged_summary, mfr_destroyed_summary, mfr_threatened_72_summary),
                                        "mixed_use_summary" = c(mixed_use_damaged_summary, mixed_use_destroyed_summary, mixed_use_threatened_summary),
                                        "sfr_summary" = c(sfr_damaged_summary, sfr_destroyed_summary, sfr_threatened_summary),
                                        "outbuildings_summary" = c(outbuildings_damaged_summary, outbuildings_destroyed_summary, outbuildings_threatened_summary))
#USDA Descriptive Statistics

fire_year_range <- range(usda_data$fire_year)
fire_size_summary <- list(mean(usda_data$fire_size), min(usda_data$fire_size), max(usda_data$fire_size))

usda_descriptive_statistics <- list(fire_size_summary, fire_year_range)

# Subsection 2.2 #2

fire_size_bplot <- ggplot(usda_data, aes(x=fire_size, y="")) +
                        geom_boxplot(notch=TRUE, color="#de6600") +
  labs(title="Distribution of Fire Size", x="Fire Size (acres)") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(color="white"),
        axis.text.x = element_text(color="white"),
        plot.background = element_rect(fill="#003f5a"),
        plot.title = element_text(color="white")) +
  scale_x_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))

homes_destroyed_bplot <- ggplot(stanford_data, aes(x=SFR_DESTROYED, y="")) +
                        geom_boxplot(notch=TRUE, color="#de6600") +
  labs(title="Distribution of Number of Homes Destroyed", x="Number of Homes Destroyed") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(color="white"),
        axis.text.x = element_text(color="white"),
        plot.background = element_rect(fill="#003f5a"),
        plot.title = element_text(color="white")) +
  scale_x_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))

homes_damaged_bplot <- ggplot(stanford_data, aes(x=SFR_DAMAGED, y="")) +
                geom_boxplot(notch=TRUE, color="#de6600") +
  labs(title="Distribution of Number of Homes Damaged", x="Number of Homes Damaged") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(color="white"),
        axis.text.x = element_text(color="white"),
        plot.background = element_rect(fill="#003f5a"),
        plot.title = element_text(color="white")) +
  scale_x_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))

homes_threatened_bplot <- ggplot(stanford_data, aes(x=SFR_THREATENED_72, y="")) +
                geom_boxplot(notch=TRUE, color="#de6600") +
                labs(title="Distribution of Number of Homes Threatened", x="Number of Homes Threatened") +
                theme(axis.title.y = element_blank(),
                      axis.title.x = element_text(color="white"),
                      axis.text.x = element_text(color="white"),
                      plot.background = element_rect(fill="#003f5a"),
                      plot.title = element_text(color="white")) +
                scale_x_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))

# Subsection 2.2 #3

# Extracted just the data of interest for our two data set under analysis_DATA_NAME for convenience.

analysis_usda <- usda_data %>% 
  select(fire_year, stat_cause_descr, fire_size, 
         latitude, longitude, state) 

analysis_stanford <- stanford_data %>% 
  select(INCIDENT_NAME, POO_LATITUDE, POO_LONGITUDE, POO_STATE_CODE,
         COMMERCIAL_DESTROYED, COMMERCIAL_DAMAGED, COMMERCIAL_THREATENED_72,
         MFR_DESTROYED, MFR_DAMAGED, MFR_THREATENED_72, 
         MIXED_USE_DESTROYED, MIXED_USE_DAMAGED, MIXED_USE_THREATENED_72,
         SFR_DESTROYED, SFR_DAMAGED, SFR_THREATENED_72, 
         OUTBUILDINGS_DESTROYED, OUTBUILDINGS_DAMAGED, OUTBUILDINGS_THREATENED_72)


# Section for outliers in the data set

outlier_usda <- analysis_usda %>% 
  select(fire_size) %>%   
  summarize(max = max(fire_size),
            min = min(fire_size),
            mean = mean(fire_size))


outlier_stanford <- analysis_stanford %>% 
  select(COMMERCIAL_DESTROYED, COMMERCIAL_DAMAGED, COMMERCIAL_THREATENED_72, 
         MFR_DESTROYED, MFR_DAMAGED, MFR_THREATENED_72,
         MIXED_USE_DESTROYED, MIXED_USE_DAMAGED, MIXED_USE_THREATENED_72,
         SFR_DESTROYED, SFR_DAMAGED, SFR_THREATENED_72, 
         OUTBUILDINGS_DESTROYED, OUTBUILDINGS_DAMAGED, OUTBUILDINGS_THREATENED_72) %>%   
  select(COMMERCIAL_DESTROYED, 
         MFR_DAMAGED, MFR_THREATENED_72, 
         MIXED_USE_DESTROYED, MIXED_USE_DAMAGED, MIXED_USE_THREATENED_72,
         OUTBUILDINGS_DAMAGED, OUTBUILDINGS_THREATENED_72)

stanford_outlier_list <- list(
  outlier_COMMERCIAL_DESTROYED <- max(stanford_data$COMMERCIAL_DESTROYED, na.rm = T),
  outlier_MFR_DAMAGED <- max(stanford_data$MFR_DAMAGED, na.rm = T),
  outlier_MFR_THREATENED_72 <- max(stanford_data$MFR_THREATENED_72, na.rm = T),
  outlier_MIXED_USE_DESTROYED <- max(stanford_data$MIXED_USE_DESTROYED, na.rm = T),
  outlier_MIXED_USE_DAMAGED <- max(stanford_data$MIXED_USE_DAMAGED, na.rm = T),
  outlier_MIXED_USE_THREATENED_72 <- max(stanford_data$MIXED_USE_THREATENED_72, na.rm = T),
  outlier_OUTBUILDINGS_DAMAGED <- max(stanford_data$OUTBUILDINGS_DAMAGED, na.rm = T),
  outlier_OUTBUILDINGS_THREATENED_72 <- max(stanford_data$OUTBUILDINGS_THREATENED_72, na.rm = T)
)


usda_grouped_by_year <- group_by(usda_data, fire_year)
usda_fires_by_year <- count(usda_grouped_by_year)
usda_longer <- pivot_longer(usda_fires_by_year, -fire_year, names_to="num_fires", values_to="value")
usda_fires_over_time_plot <- ggplot(usda_longer) +
  geom_point(aes(x=fire_year, y=value), color="#fea02f") +
  geom_smooth(aes(x=fire_year, y=value), color="#de6600", method='lm', formula=y~x) +
  labs(title="WILDFIRES OVER TIME",
       x="YEAR",
       y="NUMBER OF FIRES") +
  theme(plot.background = element_rect(fill="#003f5a"),
        axis.title.x = element_text(color="white"),
        axis.title.y = element_text(color="white"),
        axis.text = element_text(color="white"),
        title = element_text(color="white")) +
  scale_y_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))

western_states <- c("washington", "oregon", "california", "nevada", "idaho", "arizona", "utah", "montana", "wyoming", "colorado", "new mexico")
usda_data_large_fires <- usda_data[usda_data$fire_size > 500.0,]
us_map <- map_data("state")
us_map <- filter(us_map, is.element(region, western_states))
us_fires_map <- ggplot(data = us_map, mapping=aes(x=long, y=lat)) +
  geom_polygon(aes(group=group), fill="#ebd9c8", color="#007a7a") +
  geom_point(data = usda_data_large_fires, aes(x=longitude, y=latitude), size=.1, color="#007a7a") +
  coord_map() +  
  xlim(-125, -102) +
  ylim(30, 50) +
  theme(plot.background = element_rect(fill="#003f5a"),
        panel.background = element_rect(fill="#007a7a"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        title = element_text(color="white")) +
  labs(title="Western US Wildfires* from 1990-2018",
       caption="*This plot only displays wildfires of 500 acres or more.")

destruction <- c("COMMERCIAL_DESTROYED", "MFR_DESTROYED", "MIXED_USE_DESTROYED", "SFR_DESTROYED", "OUTBUILDINGS_DESTROYED")
damaged <- c("COMMERCIAL_DAMAGED", "MFR_DAMAGED", "MIXED_USE_DAMAGED", "SFR_DAMAGED", "OUTBUILDINGS_DAMAGED")
threatened <- c("COMMERCIAL_THREATENED_72", "MFR_THREATENED_72", "MIXED_USE_THREATENED_72", "SFR_THREATENED_72", "OUTBUILDINGS_THREATENED_72")
stanford_destruction <- stanford_data[destruction]
stanford_damaged <- stanford_data[damaged]
stanford_threatened <- stanford_data[threatened]

total_stanford_destruction <- summarise(na.omit(stanford_destruction), across(c("COMMERCIAL_DESTROYED", 
                                                                                "MFR_DESTROYED", 
                                                                                "MIXED_USE_DESTROYED", 
                                                                                "SFR_DESTROYED", 
                                                                                "OUTBUILDINGS_DESTROYED"), sum))

total_stanford_damaged <- summarise(na.omit(stanford_damaged), across(c("COMMERCIAL_DAMAGED", 
                                                                        "MFR_DAMAGED", 
                                                                        "MIXED_USE_DAMAGED", 
                                                                        "SFR_DAMAGED", 
                                                                        "OUTBUILDINGS_DAMAGED"), sum))

total_stanford_threatened <- summarise(na.omit(stanford_threatened), across(c("COMMERCIAL_THREATENED_72", 
                                                                              "MFR_THREATENED_72", 
                                                                              "MIXED_USE_THREATENED_72", 
                                                                              "SFR_THREATENED_72", 
                                                                              "OUTBUILDINGS_THREATENED_72"), sum))

colnames(total_stanford_destruction) <- c("Commerical", "Manufacturing", "Mixed Use", "Single Home", "Outbuildings")
destruction_for_plot <- data.frame(t(total_stanford_destruction))
colnames(destruction_for_plot) <- c("count")
destruction_for_plot <- tibble::rownames_to_column(destruction_for_plot, "group")
destruction_plot <- ggplot(destruction_for_plot, aes(x=reorder(group, -count), y=count, fill=group)) +
  geom_bar(stat="identity") +
  theme(plot.background = element_rect(fill="#003f5a"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=70, vjust = 0.6, hjust=0.5),
        axis.text = element_text(color="white"),
        legend.position = "none",
        title = element_text(color="white")) +
  scale_fill_brewer(name="Type of Building", palette="Dark2") +
  labs(title="Buildings Destroyed by Wildfires (2014-17)", x="Type of Building", y="Number of Buildings") +
  scale_y_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))

colnames(total_stanford_damaged) <- c("Commerical", "Manufacturing", "Mixed Use", "Single Home", "Outbuildings")
damaged_for_plot <- data.frame(t(total_stanford_damaged))
colnames(damaged_for_plot) <- c("count")
damaged_for_plot <- tibble::rownames_to_column(damaged_for_plot, "group")
damaged_plot <- ggplot(damaged_for_plot, aes(x=reorder(group, -count), y=count, fill=group)) +
  geom_bar(stat="identity") +
  theme(plot.background = element_rect(fill="#003f5a"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=70, vjust = 0.6, hjust=0.5),
        axis.text = element_text(color="white"),
        legend.position = "none",
        title = element_text(color="white")) +
  scale_fill_brewer(name="Type of Building", palette="Dark2") +
  labs(title="Buildings Damaged by Wildfires (2014-17)", x="Type of Building", y="Number of Buildings") +
  scale_y_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))

colnames(total_stanford_threatened) <- c("Commerical", "Manufacturing", "Mixed Use", "Single Home", "Outbuildings")
threatened_for_plot <- data.frame(t(total_stanford_threatened))
colnames(threatened_for_plot) <- c("count")
threatened_for_plot <- tibble::rownames_to_column(threatened_for_plot, "group")
threatened_plot <- ggplot(threatened_for_plot, aes(x=reorder(group, -count), y=count, fill=group)) +
  geom_bar(stat="identity") +
  theme(plot.background = element_rect(fill="#003f5a"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=70, vjust = 0.6, hjust=0.5),
        axis.text = element_text(color="white"),
        legend.position = "none",
        title = element_text(color="white")) +
  scale_fill_brewer(name="Type of Building", palette="Dark2") +
  labs(title="Buildings Threatened by Wildfires (2014-17)", x="Type of Building", y="Number of Buildings") +
  scale_y_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))

grouped_by_causes <- group_by(usda_data, stat_cause_descr)
grouped_by_causes <- mutate(grouped_by_causes, count = n())
causes_plot <- ggplot(grouped_by_causes, aes(factor(reorder(grouped_by_causes$stat_cause_descr, -count)), fill=grouped_by_causes$stat_cause_descr)) +
  geom_bar(stat="count", position="dodge", fill="#fea02f") +
  theme(plot.background = element_rect(fill="#003f5a"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=70, vjust = 0.6, hjust=0.5),
        axis.text = element_text(color="white"),
        legend.position = "none",
        title = element_text(color="white")) +
  labs(title="Causes of Wildfires (1990-2018)", x="Cause", y="Count") +
  scale_y_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))

human_causes <- c("Arson", "Equipment Use", "Campfire", "Children", "Smoking", "Railroad", "Powerline", "Fireworks", "Structure", "Debris Burning")
other_causes <- c("Miscellaneous", "Lightning", "Missing/Undefined")

grouped_by_year_causes <- data.frame("fire_year", "stat_cause_descr", "human", "other")
grouped_by_year_causes <- data.frame(lapply(usda_data, as.character), stringsAsFactors=FALSE)
grouped_by_year_causes$human[grouped_by_year_causes$stat_cause_descr %in% human_causes] <- 1
grouped_by_year_causes$nature[grouped_by_year_causes$stat_cause_descr %in% other_causes] <- 1

grouped_by_year_causes <- grouped_by_year_causes[c("fire_year", "human", "nature")]
grouped_by_year_causes$human <- replace_na(grouped_by_year_causes$human, 0)
grouped_by_year_causes$nature <- replace_na(grouped_by_year_causes$nature, 0)
grouped_by_year_causes <- grouped_by_year_causes %>%
  group_by(fire_year) %>%
  summarise_each(funs(sum))

grouped_by_year_causes_longer <- pivot_longer(grouped_by_year_causes, -fire_year, names_to="Category", values_to = "value")
causes_by_year_plot <- ggplot(grouped_by_year_causes_longer, aes(x=fire_year, y=value, group=Category, fill=Category)) +
  geom_col(position='dodge') +
  theme(plot.background = element_rect(fill="#003f5a"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=70, vjust = 0.6, hjust=0.5),
        axis.text = element_text(color="white"),
        legend.title = element_blank(),
        title = element_text(color="white")) +
  scale_fill_manual(labels=c("Human", "Nature"), values=c("#de6600", "#007a7a")) +
  labs(title="Causes of Wildfires Over Time", x="Year", y="Instances of Cause")







# Plot a bar graph of the total number of buildings destroyed/damaged/threatened from each of the five building 
# types for each Western US State (use the states in western_states). Use the Stanford dataset for this. You 
# might need to match "POO_STATE_CODE" to actual states. Look up state codes and match from there.

buildings_plot <- stanford_data %>% 
  group_by(POO_STATE_CODE) %>% 
  filter(POO_STATE_CODE == 4 | POO_STATE_CODE == 6 
         | POO_STATE_CODE == 8 | POO_STATE_CODE == 16 | POO_STATE_CODE == 30 
         | POO_STATE_CODE == 32 | POO_STATE_CODE == 35 
         | POO_STATE_CODE == 41 | POO_STATE_CODE == 49 
         | POO_STATE_CODE == 53 | POO_STATE_CODE == 56 ) %>% 
  summarize(
    total_buildings_destroyed = sum(
      sum(COMMERCIAL_DESTROYED, na.rm = TRUE),
      sum(MFR_DESTROYED, na.rm = TRUE),
      sum(MIXED_USE_DESTROYED, na.rm = TRUE),
      sum(SFR_DESTROYED, na.rm = TRUE),
      sum(OUTBUILDINGS_DESTROYED, na.rm = TRUE)),
    
    total_buildings_damaged = sum(
      sum(COMMERCIAL_DAMAGED, na.rm = TRUE),
      sum(MFR_DAMAGED, na.rm = TRUE),
      sum(MIXED_USE_DAMAGED, na.rm = TRUE),
      sum(SFR_DAMAGED, na.rm = TRUE),
      sum(OUTBUILDINGS_DAMAGED, na.rm = TRUE)),
    
    total_buildings_threatened = sum(
      sum(COMMERCIAL_THREATENED_72, na.rm = TRUE),
      sum(MFR_THREATENED_72, na.rm = TRUE),
      sum(MIXED_USE_THREATENED_72, na.rm = TRUE),
      sum(SFR_THREATENED_72, na.rm = TRUE),
      sum(OUTBUILDINGS_THREATENED_72, na.rm = TRUE))) %>% 
pivot_longer(total_buildings_destroyed:total_buildings_threatened)

building_cause_plot <- ggplot(data = buildings_plot, group=group, fill=name) +
  geom_col(mapping = aes(x = name, y = value, fill=name)) +
  labs(x = "Western U.S. Wildfire Building Effects",
       y = "Number of Buildings") +
  scale_y_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE), trans="log10") +
  scale_fill_manual(labels=c("Total Buildings Damaged", "Total Buildings Destroyed", "Total Buildings Threatened"), 
                    values=c("#de6600", "007a7a", "003f5a")) +
  facet_wrap(~POO_STATE_CODE) +
  theme(plot.background = element_rect(fill="#003f5a"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color="white"),
        legend.title = element_blank(),
        title = element_text(color="white"))






# Plot a map of fire locations from the USDA dataset in the Western US. Change the point colors to indicate 
# whether the fire was human started or nature started. Use the USDA dataset for 
# this one, and only include datapoints where fire_size > 500.0.

usda_data_human_causes <- usda_data %>% 
  filter(
    stat_cause_descr == "Arson" | 
      stat_cause_descr == "Equipment Use" | 
      stat_cause_descr == "Campfire" | 
      stat_cause_descr == "Children" | 
      stat_cause_descr == "Smoking" | 
      stat_cause_descr == "Railroad" | 
      stat_cause_descr == "Powerline" | 
      stat_cause_descr == "Fireworks" | 
      stat_cause_descr == "Structure" | 
      stat_cause_descr == "Debris Burning"
    )
usda_data_other_causes <- usda_data %>% 
  filter(
    stat_cause_descr == "Miscellaneous" | 
      stat_cause_descr == "Lightning" | 
      stat_cause_descr == "Missing/Undefined"
    )

western_states <- c("washington", "oregon", "california", "nevada", "idaho", "arizona", "utah", "montana", "wyoming", "colorado", "new mexico")
usda_data_human_fires <- usda_data_human_causes[usda_data_human_causes$fire_size > 500.0,]
usda_data_other_fires <- usda_data_other_causes[usda_data_human_causes$fire_size > 500.0,]
grab_us_map <- map_data("state")
filter_us_map <- filter(grab_us_map, is.element(region, western_states))
us_fires_causes_map <- ggplot(data = grab_us_map, mapping=aes(x=long, y=lat)) +
  geom_polygon(aes(group=group), fill="#ebd9c8", color="#007a7a") +
  geom_point(data = usda_data_human_fires, aes(x=longitude, y=latitude, color="Human"), size=1.1) + # Orange
  geom_point(data = usda_data_other_fires, aes(x=longitude, y=latitude, color="Nature"), size=1.1) + # Purple
  coord_map() +  
  xlim(-125, -102) +
  ylim(30, 50) +
  theme(plot.background = element_rect(fill="#003f5a"),
        panel.background = element_rect(fill="#007a7a"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank(),
        title = element_text(color="white"),
        plot.title = element_text(size=10)) +
  scale_color_manual(values = c("#FF6C00", "#9800FF")) +
  labs(title="Western US Wildfires* Caused by Either Humans or Nature",
       caption="*This plot only displays wildfires of 500 acres or more.")


regions_affected_summary <- usda_data %>% 
  select(fire_size, state) %>% 
  filter(
    state == "WA"|
    state == "OR"|
    state == "CA"|
    state == "NV"|
    state == "ID"|
    state == "AZ"|
    state == "UT"|
    state == "MT"|
    state == "WY"|
    state == "CO"|
    state == "NM"
         ) %>% 
  group_by(state) %>% 
  summarize("500" = sum(fire_size >= 500),
            "1000" = sum(fire_size >= 1000),
            "50000" = sum(fire_size >= 50000),
            "100000" = sum(fire_size >= 100000)) 
  






