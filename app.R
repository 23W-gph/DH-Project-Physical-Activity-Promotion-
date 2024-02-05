pacman::p_load(
  shiny,
  bslib,
  ggplot2,
  here,
  readr,
  shinylive,
  archive
)

source(here("clean.R"))

ui <- fluidPage(
  includeCSS("style.css"),  
  
  titlePanel("Physical activity  promotion for different age groups and ethnicities in the UK"),
  
  fluidRow(
    column(width = 12, 
           
    h2("Physical activity in general", class = "physical-activity-head"),
    h3("Physical inactivity as a social challenge worldwide", class = "physical-activity-heading"),
    tags$ul(
    br(),
    tags$li("Recent estimates show that 1.4 billion adults worldwide (27.5% of the global population) 
                do not meet the recommended level of physical activity"),
    br(),
    tags$li("More than 80% of the world's adolescent population is insufficiently physically active"),
    br(),
    tags$li("Physical activity is defined as any bodily movement produced by skeletal muscles
                that requires energy expenditure and can be done at a variety of intensities, and
                accumulated through work, domestic chores, transportation or during leisure time, or
                when participating in sport, walking, cycling, active recreation, and active play"),
    br(),
    tags$li("Physical activity has not only an impact on
                individuals and their families, but also on health
                services and society as a whole"),
    br(),
    tags$li("wide differences
                in levels of physical activity between regions,
                countries, age groups and sexes do exist")
      ),
    br(),
    p("Further Reading:", a("Global status report on physical activity 2022", href = "https://www.who.int/publications/i/item/9789240059153")),
    br(),
  
    h3("Physical activity in the UK", class = "physical-activity-heading"),
    br(),
    tags$ul(
    br(),
    tags$li(" A high burden of physical inactivity is also visible in the UK"),
    br(),
    tags$li(" in the year to November 2022, only 63.1% of people aged 16 and over were physically active"),
    br(),
    tags$li("there are also differences between different age groups and ethnic groups. For example: the percentages of physically active people in the Asian, black, and 'other' ethnic groups were consistently lower than the national average and that people became less active the older they were (72.8% were physically active aged 16-24; 41.2% were physically active aged 75+"),
     ),
    br(),
    p("Further Reading:", a("Physical activity in the UK", href = "https://www.ethnicity-facts-figures.service.gov.uk/health/diet-and-exercise/physical-activity/latest/#:~:text=in%20the%20year%20to%20November%202022%2C%2063.1%25%20of%20people%20aged,in%20England%20were%20physically%20active")),
    br(),
  
    h3("Benefits of physical acitivity", class = "physical-activity-heading"),
    br(),
    tags$table(
    style = "border-collapse: collapse; border: 2px solid black; padding: 10px;",
      tags$tr(
        tags$td("Cardiometabolic Conditions"),
        tags$td("Cancer"),
        tags$td("Brain Health"),
        tags$td("Weight Status"),
      ),
      tags$tr(
        tags$td(
          tags$ul(
            tags$li("Lower cardiovascular incidence and mortality (including heart disease and stroke)"),
            tags$li("Lower incidence of hypertension"),
            tags$li("Lower incidence of type 2 diabetes"))),
        tags$td(
          tags$ul(
            tags$li("Lower incidence of bladder, breast, colon, endometrium,
            esophagus, kidney, stomach, and lung cancers"))),
        tags$td(
            tags$ul(
              tags$li("Reduced risk of dementia"),
              tags$li("Improved cognitive function"),
              tags$li("Improved cognitive function following bouts of aerobic activity"),
              tags$li("Improved quality of life"),
              tags$li("Improved sleep"),
              tags$li("Reduced feelings of anxiety and depression in healthy people and in people with existing clinical syndromes"),
              tags$li("Reduced incidence of depression"))),
        tags$td(
          tags$ul(
            tags$li("Reduced risk of excessive weight gain"),
            tags$li("Weight loss and the prevention of weight regain following initial weight loss when a sufficient dose of moderate-to-vigorous physical activity is attained"),
            tags$li("An additive effect on weight loss when combined with moderate dietary restriction")),
            ))),
    br(),
    p("Further Reading:", a("2018 Physical Activity Guidelines Advisory Committee Scientific Report", href = "https://health.gov/our-work/nutrition-physical-activity/physical-activity-guidelines/current-guidelines/scientific-report")),
    br(),
  
    h3("WHO guidelines on physical activity and sedentary behaviour", class = "physical-activity-heading"),
    br(),
    p("In 2020, the WHO published its guidelines on physical activity, which contain exercise 
            recommendations for different age groups. As our physical activity promotion is mainly 
            aimed at adults, the guidelines for the age group 18-64 are explained:"),
  
    tags$ul(
    tags$li("Adults should do at least 150-300 minutes of moderate-intensity aerobic physical activity; or at least 75–150 minutes of vigorous-intensity aerobic physical activity or an equivalent combination of moderate- and vigorous-intensity activity throughout the week, for substantial health benefits."),
    br(),
    tags$li("Adults should also do muscle-strengthening activities at moderate or greater intensity that involve all major muscle groups on 2 or more days a week, as these provide additional health benefits."),
    br(),
    tags$li("Adults may increase moderate-intensity aerobic physical activity to more than 300 minutes; or do more than 150 minutes of vigorous-intensity aerobic physical activity; or an equivalent combination of moderate- and vigorous-intensity activity throughout the week for additional health benefits."),
    br(),
    tags$li("Adults should limit the amount of time spent being sedentary. Replacing sedentary time with physical activity of any intensity (including light intensity) provides health benefits."),
    br(),
    tags$li("To help reduce the detrimental effects of high levels of sedentary behaviour on health, adults should aim to do more than the recommended levels of moderate- to vigorous-intensity physical activity.")
    ),
    br(),
    p("Further Reading:", a("WHO guidelines on physical activity and sedentary behavior", href = "https://www.who.int/publications/i/item/9789240015128")),
    br(),
    h2("Visualization of physical activity between age groups and ethnicities in the UK", class = "physical-activity-head")),
    p("The following interactive graphic shows the percentage of active people of different ethnicities. In addition, the age groups can be selected and the respective year can be selected to check the development from 2016 to 2022:"),
    br()),

  sidebarLayout(
    sidebarPanel(selectInput("age_group", "Select Age Group", choices = c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74"))),
    mainPanel(
      navset_card_underline(
        title = "Year-selection to visualize differences between 2016 and 2022",
        nav_panel("Plot 2016", titlePanel("Physical activity 2016"), plotOutput("distPlot_2016")),
        nav_panel("Plot 2022", titlePanel("Physical activity 2022"), plotOutput("distPlot_2022")),
      )
    )
  )
)

server <- function(input, output) {
  
  output$distPlot_2016 <- renderPlot({
    selected_age_group <- input$age_group
    
    Percentage_selected_age_2016 <- Physical_activity %>%
      filter(Degree_of_activity == "active", Age == selected_age_group, Timepoint == as.Date("01.01.2016", format = "%d.%m.%Y"))
    ggplot(Percentage_selected_age_2016, aes(x = Ethnicity, y = Percentage, fill = Ethnicity)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("Percentage of Active People (Age", selected_age_group, ") by Ethnicity"),
           x = "Ethnicity", y = "Percentage") +
      theme_minimal()
  })
  
  output$distPlot_2022 <- renderPlot({
    selected_age_group <- input$age_group
    
    Percentage_selected_age_2022 <- Physical_activity %>%
      filter(Degree_of_activity == "active", Age == selected_age_group, Timepoint == as.Date("01.01.2022", format = "%d.%m.%Y"))
    
    ggplot(Percentage_selected_age_2022, aes(x = Ethnicity, y = Percentage, fill = Ethnicity)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("Percentage of Active People (Age", selected_age_group, ") by Ethnicity - 2022"),
           x = "Ethnicity", y = "Percentage") +
      theme_minimal()
  })
}

shinylive::export(appdir = ".", destdir ="output")

shinyApp(ui = ui, server = server)