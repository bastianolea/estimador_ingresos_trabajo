library(shiny)
library(htmltools)
library(bslib)
library(shades)
library(glue)

# crear colores a partir de un color base
color_base = "#BC4F21"
color_principal = color_base |> saturation(delta(-0.05)) |> as.character()
color_fondo = color_base |> brightness(delta(-0.65)) |> saturation(delta(-0.4)) |> as.character()
color_detalle = color_base |> brightness(delta(-0.3)) |> saturation(delta(-0.5)) |> as.character()
color_texto = color_base |> chroma(70) |> lightness(95) |> as.character()

# # previsualizar colores
# swatch(c(color_fondo,
#        color_texto,
#        color_detalle,
#        color_principal), bg = "#181818")

# ui ----
ui <- fluidPage(
    title = "Presupuesto por horas", lang = "es",
    
    # tema de la app
    theme = bs_theme(
        bg = color_fondo,
        fg = color_texto,
        primary = color_principal,
        font_scale = 1.3,
        base_font = bslib::font_google("Pacifico")
    ),
    
    # estilo de textos de sliders
    tags$style(".irs-min, .irs-max, .irs-single { font-size: 60% !important ;}"),
    
    div(
        # ancho máximo de la app
        style = css(max_width = "320px", 
                    margin = "auto", margin_top = "60px"),
        fluidRow(
            # inputs
            column(12,
                   
                   # título de la app
                   h1("Presupuesto por horas", 
                      style = css(color = color_detalle)),
                   br(),
                   
                   # sliders
                   
                   sliderInput("horas_diarias",
                               label = "Horas diarias",
                               min = 1, max = 12, value = 8, ticks = F) |> 
                       div(style = css(margin_bottom = "30px")),
                   
                   sliderInput("dias_semana",
                               label = "Días a la semana",
                               min = 1, max = 7, value = 5, ticks = F) |> 
                       div(style = css(margin_bottom = "30px")),
                   
                   sliderInput("precio_hora",
                               label = "Precio por hora",
                               value = 4000, step = 1000,
                               min = 3000, max = 30000, 
                               ticks = F, sep = ".", pre = "$") |> 
                       div(style = css(margin_bottom = "30px")),
            )
        ),
        
        # cuadro de cifra
        fluidRow(
            column(12, 
                   style = css(padding = "18px", font_size = "130%", 
                               background_color = color_fondo, 
                               border = paste("3.5px", color_detalle, "solid"), 
                               margin_top = "24px",
                               padding_bottom = "0px",
                               padding_top = "14px",
                               text_align = "center",
                               color = color_texto,
                               border_radius = "6px"),
                   
                   # titular y cifra
                   div(
                       strong("Ingreso mensual:"),
                       p(textOutput("texto", inline = T))
                   )
            ),
            
            # redacción del texto
            column(12,
                   style = css(margin_top = "42px"),
                   div(style = css(color = color_detalle, 
                                   font_size = "85%"),
                       textOutput("texto_horas")
                   )
            )
        ),
        
        # firma
        fluidRow(
            column(12,
                   style = css(margin_top = "50px",
                               margin_bottom = "60px",
                               font_size = "60%",
                               border_radius = "12px",
                               background_color = color_fondo |> brightness(delta(-0.025)),
                               padding = "16px",
                               padding_bottom = "0px",
                               font_family = "Tahoma",
                               opacity = "50%"),
                   markdown("App desarrollada en R por [Bastián Olea Herrera.](https://bastianolea.github.io/shiny_apps/) 
                            [Código de fuente en GitHub.](https://github.com/bastianolea/estimador_ingresos_trabajo)")
            )
        )
    )
)

server <- function(input, output, session) {
    
    # cálculo del monto mensual
    ingreso_mensual <- reactive({
        
        ingreso_diario = input$precio_hora * input$horas_diarias
        
        ingreso_mensual = (ingreso_diario * input$dias_semana) * 4
        
        return(ingreso_mensual)
    })
    
    # formatear el monto mensual con signo peso y separadores de miles
    ingreso_mensual_formato <- reactive({
        monto <- format(ingreso_mensual(), 
                        big.mark = ".", decimal.mark = ",",
                        scientific = FALSE)
        
        return(paste0("$", monto))
    })
    
    # output de la cifra grande del monto mensua
    output$texto <- renderText({
        ingreso_mensual_formato()
    })
    
    # redacción del texto explicativo usando todas las variables
    output$texto_horas <- renderText({
        precio <- format(input$precio_hora,
                         big.mark = ".", decimal.mark = ",",
                         scientific = FALSE)
        
        horas_semanales <- (input$horas_diarias * input$dias_semana)
        glue("Considerando un cobro por hora de ${precio}, 
        y una carga de trabajo estimada de {input$horas_diarias} horas diarias, 
        {input$dias_semana} días a la semana, 
        se estima un total de {horas_semanales} horas semanales de trabajo, 
        resultando en un monto mensual aproximado de {ingreso_mensual_formato()}.")
    })
}

shinyApp(ui = ui, server = server)