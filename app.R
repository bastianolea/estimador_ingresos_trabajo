library(shiny)
library(htmltools)
library(bslib)
library(shades)
# library(thematic)

# crear colores a partir de un color base
color_base = "#BC4F21"
color_principal = color_base |> saturation(delta(-0.05)) |> as.character()
color_fondo = color_base |> brightness(delta(-0.65)) |> saturation(delta(-0.4)) |> as.character()
color_detalle = color_base |> brightness(delta(-0.5)) |> saturation(delta(-0.5)) |> as.character()
color_texto = color_base |> chroma(70) |> lightness(95) |> as.character()

# # previsualizar colores
# swatch(c(color_fondo,
#        color_texto,
#        color_detalle,
#        color_principal), bg = "#181818")

ui <- fluidPage(
    
    theme = bs_theme(
        bg = color_fondo,
        fg = color_texto,
        primary = color_principal,
        font_scale = 1.3,
        base_font = bslib::font_google("Pacifico")
    ),
    
    tags$style(".irs-min, .irs-max, .irs-single { font-size: 60% !important ;}"),
    
    div(style = css(max_width = "300px", 
                    margin = "auto", 
                    margin_top = "60px"),
        fluidRow(
            column(12,
                   
                   h1("Presupuesto por horas", 
                      style = css(color = color_detalle)),
                   br(),
                   
                   sliderInput("horas_diarias",
                               label = "Horas diarias",
                               min = 1, max = 12, value = 8, ticks = F
                   ) |> 
                       div(style = css(margin_bottom = "30px")),
                   
                   sliderInput("dias_semana",
                               label = "Días a la semana",
                               min = 1, max = 7, value = 5, ticks = F
                   ) |> 
                       div(style = css(margin_bottom = "30px")),
                   
                   
                   # numericInput("precio_hora",
                   #              label = "Precio por hora",
                   #              value = 10000, step = 1000,
                   #              min = 3000, max = 60000,
                   # ),
                   
                   sliderInput("precio_hora",
                               label = "Precio por hora",
                               value = 20000, step = 1000,
                               min = 3000, max = 30000, 
                               ticks = F, sep = ".", pre = "$"
                   ) |> 
                       div(style = css(margin_bottom = "30px")),
            )
        ),
        
        fluidRow(
            column(12, 
                   style = css(padding = "18px", font_size = "130%", 
                               background_color = color_fondo, 
                               border = paste("3.5px", color_detalle, "solid"), 
                               margin_top = "24px",
                               margin_bottom = "64px",
                               padding_bottom = "0px",
                               text_align = "center",
                               color = color_texto,
                               border_radius = "6px"),
                   
                   div(strong("Ingreso mensual:"),
                       p("$", textOutput("texto", inline = T))
                   )
            )
        )
    )
)

server <- function(input, output, session) {
    
    ingreso_mensual <- reactive({
        
        ingreso_diario = input$precio_hora * input$horas_diarias
        
        ingreso_mensual = (ingreso_diario * input$dias_semana) * 4
        
        return(ingreso_mensual)
    })
    
    output$texto <- renderText({
        format(ingreso_mensual(), 
               big.mark = ".", decimal.mark = ",",
               scientific = FALSE)
    })
}

shinyApp(ui = ui, server = server)
