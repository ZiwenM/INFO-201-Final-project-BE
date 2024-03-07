library("shiny")
library("shinythemes")
library("plotly")
source("education.csv")
source("unemployment.csv")
## OVERVIEW TAB INFO 
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Employment Rates by Educational Attainment From 2015 To 2019"),w
  tabsetPanel(
    tabPanel("Project Overview", fluid = TRUE, sidebarLayout(
      sidebarPanel(
        h1("BACKGROUND | The Unseen Dynamics of Education and Employment In America"),
        p("In a rapidly evolving society, where the connection between education 
          and employment plays a pivotal role in shaping individual destinies, our
          project aims to delve into the intricacies of the American education system 
          and its profound impact on career trajectories."),
        p("Our project addresses fundamental life issues like education and employment
          which impact millions of Americans. The struggles and successes of individuals 
          navigating the education-to-employment pipeline resonate with a broad audience,
          making the project universally relatable."),
        h1("RESEARCH QUESTION | Questions that We Should Consider"),
        p("The", strong("overarching research question"), "for this project is:
          How Much Are People Making Per Year Based On How Educated They Are From 2015 To 2019?"),
        p("Income VS Education Levels:"),
        p("- What is the average annual income for individuals with different levels of education, 
          such as less than a high school diploma, a high school diploma only,
          some college or associate's degree, and a bachelor's degree?"),
        p("Timeline:"),
        p("- Over the past two decades, how has the evolution of educational attainment levels
          within counties influenced their median wages and unemployment rates"),
        p("Location:"),
        p("- How does the distribution of education levels vary across different regions, and 
          what impact does this have on regional economic development?"),
        p("What is the correlation between the proportion of adults with a bachelor's degree or higher 
          and median household income in different countries, and how does this vary across states?"),
        p("How have changes in education levels over time impacted median household income across different
          rural and urban areas?"),
        h1("OUR PROJECT | Our Goal and Why Do We Care"),
        p("Our exploration doesn't shy away from questioning or contradicting common assumptions. By
          challenging preconceived notions about the direct correlation between education levels and 
          employment outcomes, our project will encourage a reevaluation of the beliefs of society."),
        p("Our project aims to provide intelligent insights into the complexities of the education-
          employment chain. By doing so, we offer a thought-provoking examination that can spark discussions 
          and inspire positive change."),
        h1("THE DATA | 2015-2019 Employment Rates by Educational Attainment In the United States",
        p("The first dataset used throughout this project is called Education from kaggle.com. This document 
          lists every county within every state in the United States. Additionally, it details the population and the
          percentage of adults with educational qualifications ranging from less than a high school diploma, a high school diploma only,
          some college or associate's degree, and a bachelor's degree from the year 2015 to 2019."),
        p("The second dataset used throughout this project is called Income from kaggle.com. This document presents the mean income of 
          individuals from each county across all states in the US."), 
        h1("ABOUT US | Authors and Affiliation"),
             p(strong("Authors:"),"Joel Elangovan, Ziwen Meng, Lily Ngo, Connor Yan"),
             p(strong("Affiliation:"),"INFO 201: Foundational Skills for Data Science"),
             p("The Information School, The University of Washington"),
             p("Winter 2024")
          ),
          
          mainPanel(
            img(src = "https://files.stlouisfed.org/files/htdocs/publications/images/uploads/2016/POE201701icon_20161228040109.png",
                width = "95%", height = "95%"),
            p(""),
            img(src="https://scholasticadministrator.typepad.com/.a/6a00e54f8c25c9883401348777dc53970c-popup",
                width="95%", height="95%"),
            p(""),
            img(src="https://bachelors-completion.northeastern.edu/wp-content/uploads/2020/06/iStock-1162366190-1-1.jpg",
                width="95%", height="95%"),
          ) 
        )
      )
    )
  )
)
## VIZ 1 TAB INFO

ibrary(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

viz_1_sidebar <- sidebarPanel(
  h2("Graph Options"),
  selectInput("selectYearViz1", "Select Year", choices = c("2015", "2016", "2017", "2018", "2019"))
)

viz_1_main_panel <- mainPanel(
  h2("Educational Attainment Over Time"),
  plotlyOutput(outputId = "viz1Graph")
)

viz_1_tab <- tabPanel("Trends Over Time",
                      sidebarLayout(
                        viz_1_sidebar,
                        viz_1_main_panel
                      )
)

## VIZ 2 TAB INFO

viz_2_sidebar <- sidebarPanel(
  h2("Graph Options"),
  selectInput("selectRegion", "Select Region", choices = c("Northeast", "Midwest", "South", "West"))
)

viz_2_main_panel <- mainPanel(
  h2("Regional Educational Attainment"),
  plotlyOutput(outputId = "viz2Graph")
)

viz_2_tab <- tabPanel("Regional Analysis",
                      sidebarLayout(
                        viz_2_sidebar,
                        viz_2_main_panel
                      )
)

## VIZ 3 TAB INFO

viz_3_sidebar <- sidebarPanel(
  h2("Graph Options"),
  sliderInput("incomeRange", "Select Income Range", min = 30000, max = 100000, value = c(50000, 75000))
)

viz_3_main_panel <- mainPanel(
  h2("Income vs. Educational Attainment"),
  plotlyOutput(outputId = "viz3Graph")
)

viz_3_tab <- tabPanel("Income Analysis",
                      sidebarLayout(
                        viz_3_sidebar,
                        viz_3_main_panel
                      )
)

## CONCLUSIONS TAB INFO

conclusion_tab <- tabPanel("Conclusions",
                           h1("Key Findings and Implications"),
                           p("")
                         
