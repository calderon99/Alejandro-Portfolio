library(shiny)
library(dplyr)
library(tidyr)
library(assertive, warn.conflicts = FALSE)

data_description <- tabPanel("Problem Domain",
                             mainPanel(h2("Data Description"),
                                       h4("Group G3: Winston Bullen, Alejandro Calderon, Julius Saba Cecilia, Noah Hong"),
                                       h4(a("See our Exploratory Report.", href="https://info201b-wi21.github.io/project-WinstonBullen/")),
                                       p("NOTE: A SIGNIFICANT AMOUNT OF OUR DATA WAS REMOVED FROM OUR ANALYSIS IN ORDER TO MEET THE MEMORY CONSTRAINTS OF THE SHINY SERVER, PLEASE SEE OUR EXPLORATORY REPORT FOR A MORE COMPLETE VIEW OF OUR DATA."),
                                       p("A wildfire is a fire occurring in nature that is unplanned and uncontrolled. They are a natural occurrence that bring new life to the ecosystem and carry other surprising benefits to the environment. There are unnatural causes of wildfires, however, most notably from human activity. Whether natural or man-made, wildfires can be dangerous to nearby communities and ecosystems if they grow too large."),
                                       p("The prevalence of wildfires in the Western United States seems to be growing each year. Such an observation is common among residents of Washington, including our group members. We will investigate if wildfires are indeed gaining prevalence, and if so, at what rate. Additionally, we intend to explore the trends in the causes and effects of these fires to help educate the public."),
                                       p("Presenting the causes and effects of such catastrophic events will heighten public awareness of the issue. This awareness will better prepare citizens to make informed decisions in their interaction with the environment and lawmaking. Understanding causes of wildfires will help prevent them, and learning the consequences of environmental misconduct will deter poor behavior."),
                                       p("Our group is interested in this subject because it hits close to home. Most of our group members are residents of the Western United States, a region that has brutal wildfire seasons. The smoke and ash that consumes our cities each summer are a constant reminder of the environmental battles we face. We are curious to know the trends of these wildfires, their causes, and their effects."),
                                       h2("Datasets"),
                                       h4(a("Fire Service USDA Wildfire Locations from ~1992-2015", href="https://github.com/BuzzFeedNews/2018-07-wildfire-trends")),
                                       p("The USDA dataset contains wildfire dates, causes, and locations between 1992 and 2015 in the United States. The dataset was provided by the USDA and was collected by the California Department of Forestry and Fire Protection's Fire and Resource Assessment Program."),
                                       h4(a("Stanford US Wildfire Reports from 2014-2017", href="https://searchworks.stanford.edu/view/xj043rd8767")),
                                       p("The Stanford dataset contains wildfire dates, causes, effects, and locations between 2014 and 2017 in the United States. This dataset was provided by Stanford University and was collected by the Big Local News, a project of the Stanford Journalism and Democracy Initiative.")))

wildfires_over_time <- tabPanel("Wildfires Over Time",
                                sidebarLayout(
                                  sidebarPanel(
                                    dateRangeInput("yearrange",
                                                   "Select the Range of Years to Plot:",
                                                   format="yyyy",
                                                   min = "1992-01-01",
                                                   max = "2015-01-01")
                                  ),
                                  mainPanel(h2("Is the prevalence of wildfires in the United States growing according to common metrics?"),
                                            h3("The prevalence of wildfires in the United States is indeed growing according to common metrics. This is our analysis:"),
                                            p("The prevalence of wildfires is typically measured in the number of fires, acres burned, and property destruction. Identifying the trend of wildfire prevalence over time might indicate environmental trends of interest. Better understanding these trends will better equip us to interact with the environment in a responsible way."),
                                            p("The data analysis method we used was a point plot of the number of fires each year from 1992-2015 using the USDA dataset. The plot also features a linear regression. Such a plot lets the user see the data for themselves and easily recognize the trend. This creates an engaging point of analysis that effectively addresses the analysis question."),
                                            p("The regression plot is seen below. You are encouraged to use the sidebar controls to change the year range of data to be displayed. Adjust the plot to 1992-2015 to follow along with our analysis. As seen in the plot, the number of wildfires increased from 1992 to 2015. Note that a 28 year period is not a long amount of time in environmental studies, but we still see a noticeable increase."),
                                            h4(textOutput("wildfires_over_time_text")),
                                            br(),
                                            plotOutput("wildfires_over_time_plot", width="500px", height="500px"),
                                            br(),
                                            h5(textOutput("wildfires_over_time_text_2")),
                                            p("Depending on your selection for the range of year, changes in the frequency of the graph are either positive or negative. However, when looking at the full range of data from 1992 to 1995, the regression line on the graph shines a light on the increase in wildfire during the last decades. The increase in the linear regression indicates that fires are indeed growing according to common metrics. Such a correlation was expected by our group. The metric in this case is the number of fires, and the region is the United States. This means that as the U.S. population grows, the number of wildfires in the United States increases. This is most likely a combination of two things: more people to accidentally start fires, and more people to contribute to the environmental footprint of humanity."))
                                ))

causes_effects_over_time <- tabPanel("Causes/Effects of Wildfires",
                                     sidebarLayout(
                                       sidebarPanel(
                                         dateRangeInput("yearrange2",
                                                        "Select the Range of Years to Plot:",
                                                        format="yyyy",
                                                        min = "1992-01-01",
                                                        max = "2015-01-01",)
                                       ),
                                       mainPanel(h2("What are the trends among the causes and effects of these wildfires?"),
                                                 h3("Humans seem to be causing fewer wildfires over time in comparison to nature. Additionally, single familty homes are the most affected by wildfires. Here is our analysis:"),
                                                 p("The causes of wildfires are typically recorded by a one/two word incident description. The effects of wildfires are typically recorded in terms of nature/civilization property damage, which could be monetary or physical destruction. Understanding the trends in the causes of wildfires reveals where our problems with the environment lie. Understanding the effects is a good measure of the significance of these environmental trends."),
                                                 p("The data analysis method we used was a bar plot of wildfire causes each year from 1992-2015 using the USDA dataset. We used the Stanford dataset to investigate the effects of the wildfires from 2014-2017. We used another set of bar charts showing the number of buildings destroyed, damaged, and threatened in five major categories. These categories are single homes, outbuildings, manufacturing, commercial, and mixed use."),
                                                 p("The plots we created are seen below. As seen in the causes plot, the proportion of wildfires caused by humans seems to be decreasing inversely to those caused by nature. This means that the share of wildfires caused by humans seems to be decreasing while the share of wildfires caused by nature/other seems to be increasing. Such a trend is most easily seen when plotting data from 1992-2015, so please adjust the year range sidebar accordingly. You are encouraged to experiment with different year ranges as well."),
                                                 p("As seen in the effects plots, the vast majority of buildings threatened/damaged/destroyed are single family homes."),
                                                 h4(textOutput("causes_effects_over_time_text")),
                                                 br(),
                                                 plotOutput("causes_effects_over_time_plot", width="500px", height="500px"),
                                                 br(),
                                                 plotOutput("destruction_total", width="500px", height="500px"),
                                                 br(),
                                                 plotOutput("damaged_total", width="500px", height="500px"),
                                                 br(),
                                                 plotOutput("threatened_total", width="500px", height="500px"),
                                                 br(),
                                                 textOutput(outputId = "rangeMessage"),
                                                 p("Discovering that single family homes are most impacted by wildfires was an expected result. Homes are the building type most likely to be close to more rural areas in large numbers. Rural areas are where wildfires thrive, particularly in dry wooded areas with high temperatures. This shows that the people most at risk of wildfires are average citizens and their belongings, not just nature/arbitrary buildings."))
                                     ))

regions_affected <- tabPanel("Regions most affected by Wildfires",
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("min_fire_size", "Minimum Fire Size (acres):",
                                             c("500" = 500.0,
                                               "1,000" = 1000.0,
                                               "50,000" = 50000.0,
                                               "100,000" = 100000.0))
                               ),
                               mainPanel(h2("What States/Regions are affected the most by these fires?"),
                                         h3("Regions with hot and dry summers with an abundance of forest are the most affected by wildfires. Here is our analysis:"),
                                         p("The longitude and latitude of a wildfire is the geographical recording standard for wildfires. Understanding the regions affected most by these fires offers insight into what type of climate wildfires thrive in. This could also raise awareness for people living in affected areas."),
                                         p("The data analysis method we used was a geographical point map of the Western United States that plots the location of wildfires using the USDA dataset. Only wildfires that burned more than 500 acres are included in the visualization. To put that into perspective, UW campus is 493 acres! This visualization effectively shows which regions are most affected by number of location points. We also made a set of bar charts counting the number of buildings destroyed, damaged, and threatened in each Western State. We used the Stanford dataset for this."),
                                         p("The plots we created are seen below. As seen in the wildfire locations map, there are quite a few fires in the Western United States. The visualization is even more staggering when you remember that only fires that burned 500 acres or more are included in the visualization. Plotting every wildfire covered most of the map, and even crashed RStudio upon frequent loading! You are encouraged to use the sidebar select tool to filter fires by different acreage."),
                                         h4(textOutput("regions_text")),
                                         br(),
                                         plotOutput("regions_plot", width="500px", height="500px"),
                                         br(),
                                         h5(tableOutput("states_table")),
                                         br(),
                                         p("The wildfire locations map shows some expected results: wildfires tend to cluster around wooded areas with hot and dry seasons, such as California and neighboring states. The unexpected result was Idaho, which has many wildfires despite being a northern state. Upon further research, we discovered that Idaho has extreme seasons with summers that rival the sunbelt in terms of heat and low humidity."),
                                         br(),
                                         plotOutput("effect_by_region", width="500px", height="500px"),
                                         br(),
                                         p("The plots of total buildings affected in every Western U.S. State revealed an interesting trend. First, notice the logarithm scale of the y axis. Second, recognize that the labels for each state plot is the corresponding FIPS code for each state. The trend we discovered was that more heavily wooded states such as California, Oregon, and Washington had a higher proportion of buildings destroyed and damaged out of all buildings affected. In other words, of all buildings destroyed/damaged/threatened, the proportion of which that were damaged/destroyed were higher in more wooded states. This is likely because wildfires in more wooded areas are harder to contain than in less wooded areas. The woods have more fuel, such as wood and brush, after all. This would cause more buildings to be damaged/destroyed in wooded areas because fires are harder to control in such conditions."))
                             ))

proximity_relationship <- tabPanel("Locations of Wildfires categorized by Cause",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("min_fire_size2", "Minimum Fire Size (acres):",
                                                   c("500" = 500.0,
                                                     "1,000" = 1000.0,
                                                     "50,000" = 50000.0,
                                                     "100,000" = 100000.0))
                                     ),
                                     mainPanel(h2("Is there a relationship between wildfire proximity to a city and its cause?"),
                                               h3("Wildfires closer to the city tend to be caused more by humans. Here is our analysis:"),
                                               p("Combining our longitude/latitude and cause data allows us to create this awesome visualization. Naturally occuring wild fires, especially in California, tend to occur more inland, while human caused appear closer to the coast. The same goes for other states, but with respect to their cities rather than a coast. The further away from the city you move, the less likelyhood of human-caused fires. Understanding the relationship between the location of a wildfire and its cause will better equip the public to prevent human caused fires."),
                                               p("The data analysis method we used was a geographical point map of the Western United States that plots the location of wildfires. Each point is categorized as either human caused or natural. The concetration of wildfires is also clear in the graph. Most wildfire occur within relatively close distance of other fires. The wildfires are from 1992-2015 using the USDA dataset."),
                                               h4(textOutput(("proximity_text"))),
                                               br(),
                                               plotOutput("proximity_plot", width="500px", height="500px"),
                                               br(),
                                               p(""),
                                               p("Below is a plot displaying the total counts of each wildfire cause in the dataset. This offers insight into the specific categorization of causes, and gives the viewer a better understanding of how wildfires start. This is particularly relevant information when put in the context of human caused wildfires in more populated areas."),
                                               br(),
                                               plotOutput("total_causes_plot", width="500px", height="500px"),
                                               br(),
                                               p("As seen in the proximity plot, the Wildfires closer to more populated cities tend to be started more by humans rather than nature. Look at the areas around the following cities: Los Angeles CA, Phoenix AZ, Tucson AZ, and Boise ID. These cities are just a few examples of this trend. I encourage you to examine the area around other cities of your choice! The reason this trend exists is obvious and we expected it: the more humans in a geographical area, the more they start fires in that area."))
                                   ))

ui <- navbarPage(
    "Wildfires in the Western United States",
    data_description,
    wildfires_over_time,
    causes_effects_over_time,
    regions_affected,
    proximity_relationship
)
