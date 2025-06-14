library(shiny)
library(tidyverse)
library(plotly)
library(viridis)
library(sysfonts)
library(showtext)

font_add_google("Space Grotesk", "spacegrotesk")
showtext_auto()

df_kombineeritud <- readRDS("df_kombineeritud.rds")

theme_custom <- function(base_size = 16) {
  theme_minimal(base_size = base_size, base_family = "spacegrotesk") +
    theme(
      plot.background = element_rect(fill = "#FAF8F5", color = NA),
      panel.background = element_rect(fill = "#FAF8F5", color = NA),
      legend.background = element_rect(fill = "#FAF8F5", color = NA),
      legend.key = element_rect(fill = "#FAF8F5", color = NA),
      axis.title = element_text(color = "#2B2B2B", family = "Space Grotesk"),
      axis.text.x = element_text(angle = 45, hjust = 1, family = "Space Grotesk", color = "#2B2B2B"),
      axis.text.y = element_text(color = "#2B2B2B", family = "Space Grotesk"),
      legend.text = element_text(color = "#2B2B2B", family = "Space Grotesk"),
      legend.title = element_text(color = "#2B2B2B")
    )
}

ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@400;600&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      body {
        background-color: #FAF8F5;
        font-family: 'Space Grotesk', sans-serif;
      }
      .shiny-input-container {
        font-family: 'Space Grotesk', sans-serif;
      }
    "))
  ),
  titlePanel("Eluaseme taskukohasus 24-35a vanuserühmas ja tehingute arv"),
  sidebarLayout(
    sidebarPanel(
      selectInput("omavalitsused", "Vali omavalitsused:",
                  choices = unique(df_kombineeritud$omavalitsus),
                  multiple = TRUE)
    ),
    mainPanel(plotlyOutput("taskukohasusPlot", height = "700px"))
  )
)

server <- function(input, output) {
  output$taskukohasusPlot <- renderPlotly({
    df_filtreeritud <- df_kombineeritud %>% 
      filter(omavalitsus %in% input$omavalitsused)
    
    validate(need(nrow(df_filtreeritud) > 0, "Vali vähemalt üks omavalitsus."))
    
    p <- ggplot(df_filtreeritud, aes(x = aasta, y = palk_ruutmeeter_suhe)) +
      geom_line(aes(color = omavalitsus, group = omavalitsus), size = 1) +
      geom_point(aes(
        fill = tehingute_arv,
        text = paste0(
          "Omavalitsus: ", omavalitsus, "<br>",
          "Aasta: ", aasta, "<br>",
          "Kuupalk: ", round(kuupalk, 2), "€<br>",
          "Ruutmeetrihind: ", round(mediaan_pinnaühiku_hind_eur_m2, 2), "€<br>",
          "Palga ja ruutmeetrihinna suhe: ", round(palk_ruutmeeter_suhe, 1), "%<br>",
          "Tehingute arv: ", tehingute_arv
        )
      ), shape = 21, size = 4, color = "black", alpha = 0.85, show.legend = TRUE) +
      scale_fill_viridis_c(option = "rocket", name = "") +
      scale_color_discrete(name = "") +
      scale_x_continuous(breaks = seq(2004, 2023, by = 2)) +
      labs(x = "Aasta", y = "Palga ja ruutmeetrihinna suhe (%)") +
      theme_custom(base_size = 16)
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        annotations = list(
          list(
            text = "Tehingute arv",
            x = 1.02, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            xanchor = "left", yanchor = "middle",
            font = list(family = "Space Grotesk", size = 14, color = "#2B2B2B")
          )
        )
      )
  })
}

shinyApp(ui = ui, server = server)
