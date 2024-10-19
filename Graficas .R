# importar bases de datos Excel

file.choose()
ventas = read_excel("C:\Users\wjcor\OneDrive - PUJ Cali\Escritorio\Maestria en Ciencia de Datos\Serie de tiempo\AnalisisSeriesTiempo-Grupo4Ventas_t.xlsx")
View(ventas)

head(ventas)

# Asegurarse de que la columna 'semana' tiene formato fecha
ventas$Fecha <- as.Date(ventas$Fecha, format = "%Y-%m-%d")  # Ajusta el formato según sea necesario

# Verificar el formato de la columna
str(ventas$Fecha)

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

# Agrupar por categorias 
AVR = ventas%>% select(`Fecha`, `Item 10`, `Item 26`, `Item 30`, `Item 39`, `Item 45`, `Item 49`,
                       `Item 55`, `Item 62`, `Item 66`, `Item 79`)


Data_Center = ventas%>% select(`Fecha`, `Item 53`)


Line_UPS = ventas%>% select(`Fecha`, `Item 1`, `item 2`, `Item 3`, `Item 6`, `Item 9`, 
                            `Item 15`, `Item 17`, `Item 22`, `Item 34`, `Item 48`, 
                            `Item 51`, `Item 59`, `Item 63`, `Item 68`, `Item 70`,
                            `Item 71`, `Item 75`)


Online_UPS = ventas%>% select(`Fecha`, `Item 4`, `Item 5`, `Item 7`, `Item 8`, `Item 12`, 
                            `Item 13`, `Item 14`, `Item 16`, `Item 19`, `Item 20`, 
                            `Item 21`, `Item 23`, `Item 25`, `Item 27`, `Item 28`,
                            `Item 29`, `Item 31`, `Item 32`, `Item 33`, `Item 38`, `Item 41`, 
                            `Item 43`, `Item 44`, `Item 46`, `Item 50`, `Item 56`, `Item 57`, 
                           `Item 58`, `Item 60`, `Item 65`, `Item 72`, `Item 76`, `Item 77`, 
                           `Item 80`)


Solar = ventas%>% select(`Fecha`, `Item 36`, `Item 52`, `Item 69`, `Item 74`)


Storage_Battery = ventas%>% select(`Fecha`, `Item 11`, `Item 18`, `Item 24`, `Item 35`, `Item 37`, 
                            `Item 40`, `Item 64`, `Item 67`, `Item 78`)



Surge = ventas%>% select(`Fecha`, `Item 47`, `Item 54`, `Item 61`)


Accesories = ventas%>% select(`Fecha`, `Item 42`, `Item 73`)


# Crear una columna del total de items por catgoria

AVR = AVR%>% mutate(Total = rowSums(select(.,`Item 10`, `Item 26`, `Item 30`, `Item 39`, `Item 45`, `Item 49`,
                                           `Item 55`, `Item 62`, `Item 66`, `Item 79`), na.rm = TRUE))

Data_Center = Data_Center%>% mutate(Total = rowSums(select(.,`Item 53`), na.rm = TRUE))


Line_UPS = Line_UPS%>% mutate(Total = rowSums(select(.,`Item 1`, `item 2`, `Item 3`, `Item 6`, `Item 9`, 
                                                     `Item 15`, `Item 17`, `Item 22`, `Item 34`, `Item 48`, 
                                                     `Item 51`, `Item 59`, `Item 63`, `Item 68`, `Item 70`,
                                                     `Item 71`, `Item 75`), na.rm = TRUE))


Online_UPS = Online_UPS%>% mutate(Total = rowSums(select(.,`Item 4`, `Item 5`, `Item 7`, `Item 8`, `Item 12`, 
                                                         `Item 13`, `Item 14`, `Item 16`, `Item 19`, `Item 20`, 
                                                         `Item 21`, `Item 23`, `Item 25`, `Item 27`, `Item 28`,
                                                         `Item 29`, `Item 31`, `Item 32`, `Item 33`, `Item 38`, `Item 41`, 
                                                         `Item 43`, `Item 44`, `Item 46`, `Item 50`, `Item 56`, `Item 57`, 
                                                         `Item 58`, `Item 60`, `Item 65`, `Item 72`, `Item 76`, `Item 77`, 
                                                         `Item 80`), na.rm = TRUE))


Solar = Solar%>% mutate(Total = rowSums(select(.,`Item 36`, `Item 52`, `Item 69`, `Item 74`), na.rm = TRUE))


Storage_Battery = Storage_Battery%>% mutate(Total = rowSums(select(.,`Item 11`, `Item 18`, `Item 24`, `Item 35`, `Item 37`, 
                                                                   `Item 40`, `Item 64`, `Item 67`, `Item 78`), na.rm = TRUE))


Surge = Surge%>% mutate(Total = rowSums(select(., `Item 47`, `Item 54`, `Item 61`), na.rm = TRUE))


Accesories = Accesories%>% mutate(Total = rowSums(select(., `Item 42`, `Item 73`), na.rm = TRUE))


# Despues de un analidid de los datos decidomos que nos enfocaremos en 4 categoias que son : 
# AVR, Line UPS, Online UPS, Battery Storage; Un total de 70 Items.
# No se consideran las categorias Data Center, Solar, Surge, Accesories ya que podemos observar
# gran cantidad de ceros por lo qie podemos incluir estos item a los que solo de vender por 
# pedido especial y no es necesario tenerlos en inventario. 




# Crear el gráfico de series de tiempo por categoria 

ggplot(AVR, aes(x= Fecha, y= Total)) + 
  geom_line()+
  labs(title = "Ventas Categoria AVR", x= "Semana", y= "Ventas")
theme_minimal()
  

ggplot(Line_UPS, aes(x= Fecha, y= Total)) + 
  geom_line()+
  labs(title = "Ventas Categoria Line UPS", x= "Semana", y= "Ventas")
theme_minimal()


ggplot(Online_UPS, aes(x= Fecha, y= Total)) + 
  geom_line()+
  labs(title = "Ventas Categoria Online UPS", x= "Semana", y= "Ventas")
theme_minimal()


ggplot(Storage_Battery, aes(x= Fecha, y= Total)) + 
  geom_line()+
  labs(title = "Ventas Categoria Storage Battery", x= "Semana", y= "Ventas")
theme_minimal()

