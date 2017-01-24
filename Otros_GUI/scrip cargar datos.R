library(gWidgets)
library(gWidgetstcltk) #or gWidgetsGtk2 or gWidgetsrJava 

win <- gwindow("Tab delimited file upload example") # Crea nueva ventana ("")->Nombre de la ventana
grp_name <- ggroup(container = win) #Crea un container

lbl_data_frame_name <- glabel( #Le da nombre al container creado anteriormente (grp_name)
  "Variable to save data to: ",
  container = grp_name
)

txt_data_frame_name <- gedit("dfr", container = grp_name) #Crea cuadro de texto en el container, "texto inicial"

grp_upload <- ggroup(container = win) #Agrupar los containers

btn_upload <- gbutton( #Crea un botón
  text = "Upload tab delimited file",
  container = grp_upload,
  handler = function(h, ...) #Activar botón con un click
  {
    # TODO!
  }
)


use_comma_for_decimal <- function()
{
  unname(Sys.localeconv()["decimal_point"] == ",")
}
chk_eurostyle <- gcheckbox( #Crea un cuadro para chequear una condicion deseada
  text = "Use comma for decimal place",
  checked = use_comma_for_decimal(),
  container = grp_upload
)


status_bar <- gstatusbar("", container = win) #Crea un mensaje para el usuario



function(h, ...)
{
  gfile(
    text = "Upload tab delimited file",
    type = "open",
    action = ifelse(svalue(chk_eurostyle), "read.delim2", "read.delim"),
    handler = function(h, ...)
    {
      tryCatch(
{
  data_frame_name <- make.names(svalue(txt_data_frame_name))
  the_data <- do.call(h$action, list(h$file))
  assign(data_frame_name, the_data, envir = globalenv())
  svalue(status_bar) <-
    paste(nrow(the_data), "records saved to variable", data_frame_name)
},
error = function(e) svalue(status_bar) <- "Could not upload data"
      )
    },
    filter = list(
      "Tab delimited" = list(patterns = c("*.txt","*.dlm","*.tab")),
      "All files" = list(patterns = c("*"))
    )
  )
  
}
  