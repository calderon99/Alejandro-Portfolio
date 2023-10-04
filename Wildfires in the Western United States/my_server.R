library(shiny)
library(dplyr)
library(tidyr)
library(assertive, warn.conflicts = FALSE)


source("analysis.R")

server <- function(input, output) {
  
  selected_time_year_range_fires_over_time <- reactive({
    usda_grouped_by_year <- group_by(usda_data, fire_year)
    usda_fires_by_year <- count(usda_grouped_by_year)
    usda_fires_by_year <- usda_fires_by_year %>%
      filter(fire_year >= as.integer(substring(input$yearrange[1], 0, 4))) %>%
      filter(fire_year <= as.integer(substring(input$yearrange[2], 0, 4)))
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
    usda_fires_over_time_plot
  })
  
  output$wildfires_over_time_plot <- renderPlot({
    selected_time_year_range_fires_over_time()
  }, height=500, width=500)
  
  output$wildfires_over_time_text <- renderText({
    start <- "Plotting wildfires in the United States from "
    if (isTruthy(input$yearrange[1]) && isTruthy(input$yearrange[2])) {
      result <- paste(start, substring(input$yearrange[1], 0, 4), " to ", substring(input$yearrange[2], 0, 4), ", with a regression line showing the trend in wildfires.", sep="")
    } else {
      result <- paste(start, " 1992 to 2015.", sep="")
    }
    result
  })
  output$wildfires_over_time_text_2 <- renderText({
    start <- "This graph shows the number of fires every year between "
    if (isTruthy(input$yearrange[1]) && isTruthy(input$yearrange[2])) {
      result <- paste(start, substring(input$yearrange[1], 0, 4), " and ", substring(input$yearrange[2], 0, 4), ".", sep="")
    } else {
      result <- paste(start, " ____ and ____.", sep="")
    }
    result
  })  
  

###  paste("This graph shows the number of fires every year between ", as.integer(substring(input$yearrange2[1], 0, 4))," and ", as.integer(substring(input$yearrange2[2], 0, 4)), ". ", "As you can see from the graph, in ", as.integer(substring(input$yearrange2[1], 0, 4)), "the number of fires was much lower than in ", as.integer(substring(input$yearrange2[2], 0, 4)), ".")
  selected_time_year_range_causes_effects <- reactive({
    grouped_by_year_causes <- data.frame("fire_year", "stat_cause_descr", "human", "other")
    grouped_by_year_causes <- data.frame(lapply(usda_data, as.character), stringsAsFactors=FALSE)
    grouped_by_year_causes$human[grouped_by_year_causes$stat_cause_descr %in% human_causes] <- 1
    grouped_by_year_causes$nature[grouped_by_year_causes$stat_cause_descr %in% other_causes] <- 1
    
    grouped_by_year_causes <- grouped_by_year_causes[c("fire_year", "human", "nature")]
    grouped_by_year_causes$human <- replace_na(grouped_by_year_causes$human, 0)
    grouped_by_year_causes$nature <- replace_na(grouped_by_year_causes$nature, 0)
    grouped_by_year_causes <- grouped_by_year_causes %>%
      group_by(fire_year) %>%
      summarise_each(funs(sum)) %>%
      filter(fire_year >= as.integer(substring(input$yearrange2[1], 0, 4))) %>%
      filter(fire_year <= as.integer(substring(input$yearrange2[2], 0, 4)))
    
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
    causes_by_year_plot
  })
    
  
  human_and_nature_range <- reactive({
    grouped_by_year_causes <- data.frame("fire_year", "stat_cause_descr", "human", "other")
    grouped_by_year_causes <- data.frame(lapply(usda_data, as.character), stringsAsFactors=FALSE)
    grouped_by_year_causes$human[grouped_by_year_causes$stat_cause_descr %in% human_causes] <- 1
    grouped_by_year_causes$nature[grouped_by_year_causes$stat_cause_descr %in% other_causes] <- 1
    
    grouped_by_year_causes <- grouped_by_year_causes[c("fire_year", "human", "nature")]
    grouped_by_year_causes$human <- replace_na(grouped_by_year_causes$human, 0)
    grouped_by_year_causes$nature <- replace_na(grouped_by_year_causes$nature, 0)
    grouped_by_year_causes <- grouped_by_year_causes %>%
      group_by(fire_year) %>%
      summarise_each(funs(sum)) %>%
      filter(fire_year >= as.integer(substring(input$yearrange2[1], 0, 4))) %>%
      filter(fire_year <= as.integer(substring(input$yearrange2[2], 0, 4)))
    
    
    human_range <- first(grouped_by_year_causes$human) - last(grouped_by_year_causes$human)
    nature_range <- first(grouped_by_year_causes$nature) - last(grouped_by_year_causes$nature)
    
    paste("Between the years", as.integer(substring(input$yearrange2[1], 0, 4)), "and", as.integer(substring(input$yearrange2[2], 0, 4)), "the difference between the greatest number and least number of human causes was ", format(c(as.integer(human_range)),big.mark=",", trim=TRUE), "and for nature was", format(c(as.integer(nature_range)),big.mark=",", trim=TRUE), ". By having a negative result, it indicates a downward trend among the causes and effects of wildires between the two inputted years, while a positive result indicates an upward trend. The changing nature of the human vs nature/other causes was an expected result. Public information campaigns about preventing forest fires are famous in the United States (everyone knows Smokey Bear after all). The decrease in the proportion of wildfires that are caused by humans is evidence that these information campaigns work. The increase in the proportion of wildfires caused by nature/other is likely an indication of climate change.")
  })
    
  output$rangeMessage <- renderText(human_and_nature_range())
  
  
  
  
  output$causes_effects_over_time_plot <- renderPlot({
    selected_time_year_range_causes_effects()
  }, height=500, width=500)
  
  output$causes_effects_over_time_text <- renderText({
    start <- "Plotting causes of wildfires in the United States from "
    if (isTruthy(input$yearrange2[1]) && isTruthy(input$yearrange2[2])) {
      result <- paste(start, substring(input$yearrange2[1], 0, 4), " to ", substring(input$yearrange2[2], 0, 4), ".", sep="")
    } else {
      result <- paste(start, " 1990 to 2018.", sep="")
    }
    result
  })
  
  output$destruction_total <- renderPlot({
    destruction_plot
  }, height=500, width=500)
  
  output$damaged_total <- renderPlot({
    damaged_plot
  }, height=500, width=500)
  
  output$threatened_total <- renderPlot({
    threatened_plot
  }, height=500, width=500)
  
  regions_acres_selected <- reactive({
    western_states <- c("washington", "oregon", "california", "nevada", "idaho", "arizona", "utah", "montana", "wyoming", "colorado", "new mexico")
    usda_data_large_fires <- usda_data[usda_data$fire_size > as.integer(input$min_fire_size),]
    us_map <- map_data("state")
    us_map <- filter(us_map, is.element(region, western_states))
    us_fires_map_display <- ggplot(data = us_map, mapping=aes(x=long, y=lat)) +
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
           caption=paste("*This plot only displays wildfires of ", input$min_fire_size, " acres or more.", sep=""))
    us_fires_map_display
    
  })
  
  output$states_table <- renderTable({
    table <- regions_affected_summary 
    return(table)})

  
  
  
  output$regions_plot <- renderPlot({
    regions_acres_selected()
  }, height=500, width=500)
  
  output$regions_text <- renderText({
    paste("Displaying wildfires in the Western United States that burned more than ", input$min_fire_size, " acres.", sep="")
  })
  
  output$effect_by_region <- renderPlot({
    building_cause_plot
  }, height=500, width=500)
  
  proximity_acres_selected <- reactive({
    western_states <- c("washington", "oregon", "california", "nevada", "idaho", "arizona", "utah", "montana", "wyoming", "colorado", "new mexico")
    usda_data_human_fires <- usda_data_human_causes[usda_data_human_causes$fire_size > as.integer(input$min_fire_size2),]
    usda_data_other_fires <- usda_data_other_causes[usda_data_human_causes$fire_size > as.integer(input$min_fire_size2),]
    grab_us_map <- map_data("state")
    filter_us_map <- filter(grab_us_map, is.element(region, western_states))
    us_fires_causes_map_display <- ggplot(data = grab_us_map, mapping=aes(x=long, y=lat)) +
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
           caption=paste("*This plot only displays wildfires of ", input$min_fire_size2, " acres or more.", sep=""))
    us_fires_causes_map_display
  })
  
  output$proximity_plot <- renderPlot({
    proximity_acres_selected()
  }, height=500, width=500)
  
  output$proximity_text <- renderText({
    paste("Displaying wildfires categorized by Cause in the Western United States that burned more than ", input$min_fire_size2, " acres.", sep="")
  })
  
  output$total_causes_plot <- renderPlot({
    causes_plot
  })
}
