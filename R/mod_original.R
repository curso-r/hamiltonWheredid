#' original UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_original_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::bs4DashPage(
      sidebar_collapsed = TRUE,
      sidebar_mini = FALSE,
      body = bs4Dash::bs4DashBody(
        fresh::use_theme(hamiltonCovid19::theme_bs4Dash()),
        fluidRow(
          bs4Dash::column(
            width = 4,
            h4(div(style = "color: black;", "Assumptions")),
            # Input: Selector for choosing dataset ----
            
            sliderInput(ns("R0"), div(style = "color: black;","Mortality rate %"), 0.5, 6, 3, step=0.1),
            
            sliderInput(ns("E"), div(style = "color: black;", "Planes' occupancy before lockdown %"), 20, 100, 85 ,step = 5),
            
            sliderInput(ns("D"), div("Planes' occupancy after lockdown %"), 20, 100, 35 ,step = 5)
          ),

          column(
            width = 8,

            # Output: HTML table with requested number of observations ----
            bs4Dash::bs4TabCard(
              width = 12,
              title = "",
              id = "tabcard",
              closable = FALSE,
              collapsible = FALSE,
              bs4Dash::bs4TabPanel(
                tabName = "Plot",
                plotOutput(ns("plot")) %>% hamiltonCovid19::with_load_spinner()
              ),
              bs4Dash::bs4TabPanel(
                tabName = "Assumptions",
                h5(HTML(style = "color: black;",
                  paste0("1- Mortality rates are accurate.",
                  "2- Plane occupancy rates are assumed (best guess).",
                  "3- Mortality rates do not change with time.",
                  "4- Virus carriers boarding are randomly sampled from each country's population.",
                  "5- 21 days lag is assumed from infection to death.",
                  "6- All values are means."))),
                )
              )
            )
           )
         )
       )
    )
  

    
}
    
#' original Server Function
#'
#' @noRd 
mod_original_server <- function(input, output, session){
  ns <- session$ns

  datasets <- reactive({
    merged3 <- hamiltonWheredid::merged3
    merged <- hamiltonWheredid::virusdata
    
    merged2 <- merged %>% 
      dplyr::mutate( country = as.factor(country)) %>% 
      dplyr::filter(country == "Austria"|  country == "Belgium" | country == "Switzerland" | country == "Czechia"|
               country == "Germany" | country == "Denmark" | country == "Spain" | country == "France"|
               country == "United Kingdom" | country == "Hungary" | country == "Iceland" | country == "Italy" |
               country == "Malta" | country == "Netherlands" | country == "Norway" | country == "Portugal" |
               country == "Russia" | country == "Sweden" | country == "Turkey" | country == "United States") %>% 
      dplyr::group_by(country) %>% 
      dplyr::mutate(new_cases = (dplyr::lead(deaths,21)- deaths)/(input$R0/100)) %>% 
      dplyr::select(country, day = date, new_cases) %>% 
      dplyr::mutate(
        new_cases = dplyr::if_else(is.na(new_cases), 0, new_cases)
      )
    
    merged4 <- dplyr::left_join(merged3, merged2) %>% 
      dplyr::mutate(
        new_cases = dplyr::if_else(is.na(new_cases), 0, new_cases),
        population = as.numeric(population),
        ratio = new_cases/population,
        carriers = capacity*ratio,
        day = as.Date(day)
      )

    merged4$carriers[which(merged4$day < "2020-04-01")] <- merged4$carriers[which(merged4$day < "2020-04-01")] *input$E*0.01
    merged4$carriers[which(merged4$day >= "2020-04-01")] <- merged4$carriers[which(merged4$day >= "2020-04-01")] *input$D*0.01
    
    merged4 <- stats::na.omit(merged4)
    
    merged4
  })
  
  output$plot <- renderPlot({
    merged4 <- datasets()

    x <- merged4 %>% 
      dplyr::group_by(country) %>% 
      dplyr::summarise(carriers = sum(carriers)) %>% 
      dplyr::arrange( dplyr::desc(carriers)) %>% 
      dplyr::mutate(
        country = as.character(country)
      )
    
    ggplot2::ggplot(x, ggplot2::aes(stats::reorder(country, -carriers),carriers, fill = "red")) + 
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(x = NULL, y = "Number of infected") +
      ggplot2::ggtitle("Number of infected people coming to Ireland")

    y <-merged4 %>% 
      dplyr::mutate(
        country = as.character(country),
        country = dplyr::if_else(
         !(country %in% dplyr::pull(x[1:4,1])),
         "Others",
         country
        ),
        day = lubridate::floor_date(as.Date(day), "week")
      ) %>%
      dplyr::group_by(day, country) %>% 
      dplyr::summarise(carriers = sum(carriers))
    
    ggplot2::ggplot() + 
      ggplot2::geom_col(data = y, ggplot2::aes(day, carriers, fill = country)) + 
      ggplot2::labs(x = NULL ,y = "Virus carriers per week", fill = "Country") +
      ggplot2::ggtitle("Estimated COVID-19 cases per week imported to Ireland") +
      ggplot2::scale_x_date(breaks = "2 weeks", date_labels = "%d-%b") +
      #scale_x_date(date_breaks = "2 week") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title = ggplot2::element_text(size=15,face = "bold"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)) 
    
    
  })
 
}
    
## To be copied in the UI
# mod_original_ui("original_ui_1")
    
## To be copied in the server
# callModule(mod_original_server, "original_ui_1")
 
