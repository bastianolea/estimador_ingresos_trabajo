mil <- function(x) format(x, big.mark = ".", 
                          decimal.mark = ",")

horas_diarias = 4
dias_semana = 4
precio_hora = 18000

ingreso_diario = precio_hora * horas_diarias

ingreso_mensual = (ingreso_diario * dias_semana) * 4

ingreso_total = ingreso_mensual * 2

cat("Ingreso total:", ingreso_total |> mil(),
    "Ingreso mensual:", ingreso_mensual |> mil(), 
    sep = "\n")




