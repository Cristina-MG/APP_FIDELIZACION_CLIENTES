# Install and load necessary packages
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
if (!requireNamespace("shinydashboard", quietly = TRUE)) {
  install.packages("shinydashboard")
}
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("DT", quietly = TRUE)) {
  install.packages("DT")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
if (!requireNamespace("shinydashboardPlus", quietly = TRUE)) {
  install.packages("shinydashboardPlus")
}
if (!requireNamespace("flexdashboard", quietly = TRUE)) {
  install.packages("flexdashboard")
}
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}
if (!requireNamespace("shinyalert", quietly = TRUE)) {
  install.packages("shinyalert")
}
if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
  install.packages("shinyWidgets")
}
if (!requireNamespace("shinyjs", quietly = TRUE)) {
  install.packages("shinyjs")
}


# Cargar librerías
library(shiny)              # Librería principal de Shiny
library(shinydashboard)     # Para crear dashboards en Shiny
library(plotly)             # Gráficos interactivos
library(dplyr)              # Manipulación de datos
library(ggplot2)            # Visualización avanzada
library(DT)                 # Tablas interactivas
library(readr)              # Lectura de datos
library(shinydashboardPlus) # Mejoras adicionales para dashboards en Shiny
library(flexdashboard)      # Para crear dashboards flexibles
library(rsconnect)          # Conectar con Shinyapps.io u otros servidores
library(shinyalert)         # Alertas en aplicaciones Shiny
library(shinyWidgets)       # Widgets personalizados para Shiny
library(shinyjs)            # Ejecutar JavaScript en Shiny desde R



# Load datasets
datos <- read_csv("datos_segmentacion.csv")
modelo_arbol <- readRDS("modelo_arbol_decision_v1.rds")


# Define function gauge_pais
gauge_pais <- function(country) {
  churn_rate <- datos %>%
    filter(Geography == country) %>%
    summarise(ChurnRate = mean(Exited) * 100) %>%
    pull(ChurnRate)
  
  plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = churn_rate,
    title = list(text = paste("Tasa Churn", country)),
    type = "indicator",
    mode = "gauge+number",
    number = list(suffix =  "%"),
    gauge = list(
      axis = list(range = list(0, 100)),
      bar = list(color = "white"),
      steps = list(
        list(range = c(0, 30), color = "#006400"),
        list(range = c(30, 60), color = "#FCAE1E"),
        list(range = c(60, 100), color = "#bd2c54")
      ),
      threshold = list(
        line = list(color = "white", width = 4),
        thickness = 0.75,
        value = churn_rate
      )
    )
  )
}



# Texto breve para Cliente Celeste
texto_cliente_celeste <- '<div style="background-color: #f0f8ff; padding: 10px; border-radius: 5px;">
    <h2 style="color: #87CEEB; font-size: 24px; font-weight: bold;">Cliente <span style="color: #000080;">Celeste</span></h2>
    <p style="font-size: 16px; line-height: 1.5;">
        <strong>Descripción breve:</strong><br>
        Los Clientes Celeste son predominantemente <span style="font-weight: bold; color: #87CEEB;">jóvenes y de mediana edad</span>, <span style="font-weight: bold; color: #87CEEB;">equilibrados</span> y <span style="font-weight: bold; color: #87CEEB;">solventes</span>, con un buen <span style="font-weight: bold; color: #87CEEB;">puntaje de crédito</span> y <span style="font-weight: bold; color: #87CEEB;">saldos de cuenta considerables</span>. La mayoría son <span style="font-weight: bold; color: #87CEEB;">activos</span> y tienen <span style="font-weight: bold; color: #87CEEB;">tarjeta de crédito</span>, con una distribución equilibrada de género.
    </p>
</div>'

texto_cliente_oro <- '<div style="background-color: #fffacd; padding: 10px; border-radius: 5px;">
    <h2 style="color: #FCAE1E; font-size: 24px; font-weight: bold;">Cliente <span style="color: #8b7500;">Oro</span></h2>
    <p style="font-size: 16px; line-height: 1.5;">
        <strong>Descripción breve:</strong><br>
        Los Clientes Dorado tienen <span style="font-weight: bold; color: #FCAE1E;">puntajes de crédito medios</span> y <span style="font-weight: bold; color: #FCAE1E;">edades variadas</span>, siendo <span style="font-weight: bold; color: #FCAE1E;">prudentes</span> y <span style="font-weight: bold; color: #FCAE1E;">diversificados</span>, pero tienden a tener <span style="font-weight: bold; color: #FCAE1E;">balances bancarios bajos o nulos</span>. Una mayoría posee <span style="font-weight: bold; color: #FCAE1E;">tarjeta de crédito</span> y hay un <span style="font-weight: bold; color: #FCAE1E;">equilibrio razonable</span> entre clientes activos e inactivos.
    </p>
</div>'

texto_cliente_esmeralda <- '<div style="background-color: #e0eee0; padding: 10px; border-radius: 5px;">
    <h2 style="color: #2E8B57; font-size: 24px; font-weight: bold;">Cliente <span style="color: #006400;">Esmeralda</span></h2>
    <p style="font-size: 16px; line-height: 1.5;">
        <strong>Descripción breve:</strong><br>
        Los Clientes Esmeralda son <span style="font-weight: bold; color: #2E8B57;">adultos jóvenes y de mediana edad</span>, <span style="font-weight: bold; color: #2E8B57;">confiables</span> y <span style="font-weight: bold; color: #2E8B57;">diversos</span>, con <span style="font-weight: bold; color: #2E8B57;">puntajes de crédito moderados</span> y <span style="font-weight: bold; color: #2E8B57;">balances bancarios generalmente bajos</span>. Poseen <span style="font-weight: bold; color: #2E8B57;">múltiples productos financieros</span> y una <span style="font-weight: bold; color: #2E8B57;">distribución geográfica similar</span> a otros clusters.
    </p>
</div>'

texto_cliente_purpura <- '<div style="background-color: #e6e6fa; padding: 10px; border-radius: 5px;">
    <h2 style="color: #800080; font-size: 24px; font-weight: bold;">Cliente <span style="color: #4b0082;">Púrpura</span></h2>
    <p style="font-size: 16px; line-height: 1.5;">
        <strong>Descripción breve:</strong><br>
        Los Clientes Púrpura son predominantemente de <span style="font-weight: bold; color: #800080;">mediana edad</span>, <span style="font-weight: bold; color: #800080;">estables</span> y <span style="font-weight: bold; color: #800080;">prósperos</span>, con <span style="font-weight: bold; color: #800080;">buenos puntajes de crédito</span> y <span style="font-weight: bold; color: #800080;">balances bancarios elevados</span>. Tienen una <span style="font-weight: bold; color: #800080;">leve inclinación</span> hacia la <span style="font-weight: bold; color: #800080;">diversificación de productos financieros</span> y presentan una <span style="font-weight: bold; color: #800080;">distribución equilibrada</span> entre clientes activos e inactivos.
    </p>
</div>'


texto_cliente_celeste_concreto <- '<div style="background-color: #f0f8ff; padding: 10px; border-radius: 5px;">
    <br>
        El Cliente Celeste presenta un <span style="font-weight: bold; color: #87CEEB;">puntaje de crédito promedio de 650.8</span>, lo cual indica una <span style="font-weight: bold; color: #87CEEB;">solvencia crediticia razonable</span>. La <span style="font-weight: bold; color: #87CEEB;">edad</span> de estos clientes varía desde los 18 hasta los 92 años, con una media de <span style="font-weight: bold; color: #87CEEB;">38.98 años</span>, mostrando una prevalencia de <span style="font-weight: bold; color: #87CEEB;">adultos jóvenes y de mediana edad</span>. La <span style="font-weight: bold; color: #87CEEB;">antigüedad</span> de estos clientes en la institución financiera es en promedio de <span style="font-weight: bold; color: #87CEEB;">5 años</span>. Los <span style="font-weight: bold; color: #87CEEB;">saldos de cuenta</span> fluctúan considerablemente, desde un mínimo de <span style="font-weight: bold; color: #87CEEB;">62,212</span> hasta un máximo de <span style="font-weight: bold; color: #87CEEB;">238,388 unidades monetarias</span>, con una media de <span style="font-weight: bold; color: #87CEEB;">121,934</span>. Este grupo suele tener en promedio <span style="font-weight: bold; color: #87CEEB;">1.4 productos financieros</span>, lo que sugiere una tendencia hacia la <span style="font-weight: bold; color: #87CEEB;">diversificación moderada de servicios bancarios</span>. La mayoría de estos clientes reciben un <span style="font-weight: bold; color: #87CEEB;">salario estimado alrededor de 149,730 unidades monetarias anuales</span>.
        <br><br>
        En cuanto a la <span style="font-weight: bold; color: #87CEEB;">distribución geográfica</span>, <span style="font-weight: bold; color: #87CEEB;">1258</span> de estos clientes residen en <span style="font-weight: bold; color: #87CEEB;">Francia</span>, <span style="font-weight: bold; color: #87CEEB;">1245</span> en <span style="font-weight: bold; color: #87CEEB;">Alemania</span> y <span style="font-weight: bold; color: #87CEEB;">629</span> en <span style="font-weight: bold; color: #87CEEB;">España</span>. La <span style="font-weight: bold; color: #87CEEB;">proporción de género</span> es bastante equilibrada con <span style="font-weight: bold; color: #87CEEB;">1447 mujeres</span> y <span style="font-weight: bold; color: #87CEEB;">1685 hombres</span>. Una mayoría significativa, <span style="font-weight: bold; color: #87CEEB;">2176</span> de estos clientes, posee una <span style="font-weight: bold; color: #87CEEB;">tarjeta de crédito</span>, mientras que <span style="font-weight: bold; color: #87CEEB;">956</span> no la tienen. En términos de <span style="font-weight: bold; color: #87CEEB;">actividad</span>, el grupo se divide casi a la mitad con <span style="font-weight: bold; color: #87CEEB;">1607 clientes activos</span> y <span style="font-weight: bold; color: #87CEEB;">1525 inactivos</span>. Este perfil sugiere que los Clientes Celeste son <span style="font-weight: bold; color: #87CEEB;">financieramente estables</span>, con una buena capacidad de <span style="font-weight: bold; color: #87CEEB;">manejo de crédito</span> y una <span style="font-weight: bold; color: #87CEEB;">predisposición hacia la utilización de servicios financieros variados</span>.
    </p>
</div>'

texto_cliente_oro_concreto <- '<div style="background-color: #fffacd; padding: 10px; border-radius: 5px;">
    <br>
        El Cliente Dorado tiene un <span style="font-weight: bold; color: #FCAE1E;">puntaje de crédito promedio de 650</span>, similar al del Cliente Celeste, y una <span style="font-weight: bold; color: #FCAE1E;">edad media de 38.55 años</span>, con un rango que va desde los 18 hasta los 88 años. Estos clientes tienen una <span style="font-weight: bold; color: #FCAE1E;">antigüedad media de 5 años</span> en la institución. Sin embargo, una característica distintiva de este grupo es su <span style="font-weight: bold; color: #FCAE1E;">bajo balance bancario</span>, que en muchos casos es cero, con un promedio de solo <span style="font-weight: bold; color: #FCAE1E;">2292 unidades monetarias</span>. Este dato sugiere que estos clientes no suelen mantener grandes sumas de dinero en sus cuentas bancarias.
        <br><br>
        El Cliente Dorado tiene en promedio <span style="font-weight: bold; color: #FCAE1E;">1.8 productos financieros</span>, lo que indica una leve tendencia a <span style="font-weight: bold; color: #FCAE1E;">diversificar sus inversiones y servicios bancarios</span>. El <span style="font-weight: bold; color: #FCAE1E;">salario estimado anual promedio</span> para este grupo es de <span style="font-weight: bold; color: #FCAE1E;">49776.23 unidades monetarias</span>, significativamente menor que el del Cliente Celeste. Geográficamente, <span style="font-weight: bold; color: #FCAE1E;">1263</span> clientes residen en <span style="font-weight: bold; color: #FCAE1E;">Francia</span>, <span style="font-weight: bold; color: #FCAE1E;">24</span> en <span style="font-weight: bold; color: #FCAE1E;">Alemania</span> y <span style="font-weight: bold; color: #FCAE1E;">626</span> en <span style="font-weight: bold; color: #FCAE1E;">España</span>, lo que muestra una fuerte concentración en Francia.
        <br><br>
        En cuanto a la <span style="font-weight: bold; color: #FCAE1E;">distribución por género</span>, hay <span style="font-weight: bold; color: #FCAE1E;">862 mujeres</span> y <span style="font-weight: bold; color: #FCAE1E;">1051 hombres</span>. Un número considerable, <span style="font-weight: bold; color: #FCAE1E;">1392</span>, posee <span style="font-weight: bold; color: #FCAE1E;">tarjeta de crédito</span>, mientras que <span style="font-weight: bold; color: #FCAE1E;">521</span> no la tienen. La <span style="font-weight: bold; color: #FCAE1E;">división entre clientes activos e inactivos</span> es bastante equitativa con <span style="font-weight: bold; color: #FCAE1E;">1004 activos</span> y <span style="font-weight: bold; color: #FCAE1E;">909 inactivos</span>. Este perfil sugiere que los Clientes Dorado, aunque tienen una capacidad crediticia similar a los Clientes Celeste, tienden a <span style="font-weight: bold; color: #FCAE1E;">mantener balances más bajos</span> y muestran una distribución más concentrada geográficamente en Francia.
    </p>
</div>'

texto_cliente_esmeralda_concreto <- '<div style="background-color: #e0eee0; padding: 10px; border-radius: 5px;">
    <br>
        El Cliente Esmeralda tiene un <span style="font-weight: bold; color: #2E8B57;">puntaje de crédito promedio de 648.8</span>, apenas inferior al de los otros clusters, y una <span style="font-weight: bold; color: #2E8B57;">edad media de 38.41 años</span>, con un rango de 19 a 85 años. La <span style="font-weight: bold; color: #2E8B57;">antigüedad promedio</span> en la institución es de aproximadamente <span style="font-weight: bold; color: #2E8B57;">5 años</span>. Este grupo se caracteriza por <span style="font-weight: bold; color: #2E8B57;">balances bancarios bajos</span>, con una media de <span style="font-weight: bold; color: #2E8B57;">2589 unidades monetarias</span>, similar a los Clientes Dorado, lo que sugiere una tendencia a no mantener grandes sumas en sus cuentas bancarias.
        <br><br>
        Estos clientes manejan en promedio <span style="font-weight: bold; color: #2E8B57;">1.8 productos financieros</span>, indicando una leve inclinación hacia la <span style="font-weight: bold; color: #2E8B57;">diversificación de servicios bancarios</span>. El <span style="font-weight: bold; color: #2E8B57;">salario estimado anual promedio</span> es de <span style="font-weight: bold; color: #2E8B57;">149274 unidades monetarias</span>, comparable al del Cliente Celeste. La <span style="font-weight: bold; color: #2E8B57;">distribución geográfica</span> incluye <span style="font-weight: bold; color: #2E8B57;">1244 clientes en Francia</span>, <span style="font-weight: bold; color: #2E8B57;">32 en Alemania</span> y <span style="font-weight: bold; color: #2E8B57;">613 en España</span>.
        <br><br>
        En términos de <span style="font-weight: bold; color: #2E8B57;">género</span>, hay <span style="font-weight: bold; color: #2E8B57;">881 mujeres</span> y <span style="font-weight: bold; color: #2E8B57;">1008 hombres</span> en este cluster. La mayoría, <span style="font-weight: bold; color: #2E8B57;">1341 clientes</span>, posee <span style="font-weight: bold; color: #2E8B57;">tarjeta de crédito</span>, mientras que <span style="font-weight: bold; color: #2E8B57;">548</span> no la tienen. La <span style="font-weight: bold; color: #2E8B57;">división entre clientes activos e inactivos</span> es casi equitativa, con <span style="font-weight: bold; color: #2E8B57;">965 activos</span> y <span style="font-weight: bold; color: #2E8B57;">924 inactivos</span>. Este perfil sugiere que los Clientes Esmeralda, aunque tienen una <span style="font-weight: bold; color: #2E8B57;">capacidad crediticia y salarial similar</span> a los Clientes Celeste, prefieren mantener <span style="font-weight: bold; color: #2E8B57;">balances bajos</span> en sus cuentas y <span style="font-weight: bold; color: #2E8B57;">diversificar ligeramente</span> sus productos financieros.
    </p>
</div>'

texto_cliente_purpura_concreto <- '<div style="background-color: #e6e6fa; padding: 10px; border-radius: 5px;">
    <br>
        El Cliente Púrpura presenta un <span style="font-weight: bold; color: #800080;">puntaje de crédito promedio de 651.6</span>, similar al de otros clusters, y una <span style="font-weight: bold; color: #800080;">edad media de 39.41 años</span>, con un rango de 18 a 92 años, lo que indica una predominancia de <span style="font-weight: bold; color: #800080;">adultos de mediana edad</span>. La <span style="font-weight: bold; color: #800080;">antigüedad</span> en la institución es de casi <span style="font-weight: bold; color: #800080;">5 años</span> en promedio. Este grupo se distingue por sus <span style="font-weight: bold; color: #800080;">altos balances bancarios</span>, con una media de <span style="font-weight: bold; color: #800080;">121880 unidades monetarias</span>, que va desde un mínimo de <span style="font-weight: bold; color: #800080;">62052</span> hasta un máximo de <span style="font-weight: bold; color: #800080;">250898 unidades monetarias</span>.
        <br><br>
        En términos de <span style="font-weight: bold; color: #800080;">productos financieros</span>, el Cliente Púrpura tiene en promedio <span style="font-weight: bold; color: #800080;">1.4 productos</span>, similar al Cliente Celeste, mostrando una <span style="font-weight: bold; color: #800080;">inclinación moderada hacia la diversificación</span>. El <span style="font-weight: bold; color: #800080;">salario estimado anual promedio</span> para este grupo es de <span style="font-weight: bold; color: #800080;">50472.48 unidades monetarias</span>, lo cual es menor que el del Cliente Celeste pero comparable a los otros clusters. La <span style="font-weight: bold; color: #800080;">distribución geográfica</span> incluye <span style="font-weight: bold; color: #800080;">1249 clientes en Francia</span>, <span style="font-weight: bold; color: #800080;">1208 en Alemania</span> y <span style="font-weight: bold; color: #800080;">609 en España</span>.
        <br><br>
        En cuanto a la <span style="font-weight: bold; color: #800080;">distribución por género</span>, hay <span style="font-weight: bold; color: #800080;">1353 mujeres</span> y <span style="font-weight: bold; color: #800080;">1713 hombres</span> en este cluster. Una mayoría significativa, <span style="font-weight: bold; color: #800080;">2146 clientes</span>, posee <span style="font-weight: bold; color: #800080;">tarjeta de crédito</span>, mientras que <span style="font-weight: bold; color: #800080;">920</span> no la tienen. La <span style="font-weight: bold; color: #800080;">división entre clientes activos e inactivos</span> es también equilibrada, con <span style="font-weight: bold; color: #800080;">1575 clientes activos</span> y <span style="font-weight: bold; color: #800080;">1491 inactivos</span>. Este perfil sugiere que los Clientes Púrpura son <span style="font-weight: bold; color: #800080;">financieramente estables</span>, con <span style="font-weight: bold; color: #800080;">buenos balances</span> y una <span style="font-weight: bold; color: #800080;">diversificación moderada</span> de productos financieros, manteniendo una <span style="font-weight: bold; color: #800080;">distribución geográfica y de género equilibrada</span>.
    </p>
</div>'



texto_autor <- '
<div style="background-color: #000000; color: #ffffff; padding: 10px; border-radius: 5px;">
    <p style="font-size: 16px; line-height: 1.5;">
    ¡Hola! Soy una <span style="color: #2473b3; font-size: 16px; font-weight: bold;">ingeniera química</span> apasionada que ha decidido dar un giro a su carrera profesional hacia el fascinante mundo de la <span style="color: #2473b3; font-size: 16px; font-weight: bold;">inteligencia artificial</span>. A lo largo de mi trayectoria, he descubierto que mi verdadera pasión está en crear <span style="color: #2473b3; font-size: 16px; font-weight: bold;">modelos de machine learning</span> para hacer <span style="color: #2473b3; font-size: 16px; font-weight: bold;">predicciones</span> precisas y significativas. Este campo me permite combinar mi formación técnica con mi curiosidad innata y mi amor por resolver problemas complejos.
  </p>
  <p style="font-size: 16px;">
    Actualmente, estoy profundizando en <span style="color: #2473b3; font-size: 16px; font-weight: bold;">Big Data</span> y <span style="color: #2473b3; font-size: 16px; font-weight: bold;">Machine Learning</span>, dos áreas que considero fundamentales para el futuro de la tecnología y la industria. Estoy siempre en busca de nuevas oportunidades para aprender y mejorar, y me entusiasman las posibilidades que estas habilidades me ofrecen para participar en proyectos innovadores y convertir datos en <span style="color: #2473b3; font-size: 16px; font-weight: bold;">insights</span> valiosos.
  </p>
  <p style="font-size: 16px;">
    Me encanta la idea de poder ayudar a empresas y organizaciones a tomar decisiones informadas basadas en datos. Creo firmemente en la capacidad de la <span style="color: #2473b3; font-size: 16px; font-weight: bold;">inteligencia artificial</span> para predecir <span style="color: #2473b3; font-size: 16px; font-weight: bold;">tendencias futuras</span>, descubrir <span style="color: #2473b3; font-size: 16px; font-weight: bold;">patrones ocultos</span> y mejorar la <span style="color: #2473b3; font-size: 16px; font-weight: bold;">eficiencia</span>.
  </p>
  <p style="font-size: 16px;">
    Disfruté mucho generando este <span style="color: #2473b3; font-size: 16px; font-weight: bold;">dashboard en Shiny</span> con lenguaje de programación R para la monitorización de los clientes. Este proyecto se ha realizado para el curso de especialista en <span style="color: #2473b3; font-size: 16px; font-weight: bold;">Big Data</span>.
  </p>
</div>'

foto_autor <- '
<div style="background-color: #000000; color: #ffffff; padding: 10px; border-radius: 5px;">
  <!-- Aquí se incluye la imagen -->
  <img src="https://media.licdn.com/dms/image/C4D03AQHZCXXEMt6wbg/profile-displayphoto-shrink_200_200/0/1622884107082?e=1725494400&v=beta&t=KNRAINejPp6wKwL4lwZlci_daJmOAuAM6u0_EyQCPBM" alt="Descripción de la imagen" style="max-width: 100%; height: auto; display: block; margin: 10px auto; object-fit: cover; width: 200px; height: 200px; border-radius: 50%;">

  <p style="font-size: 16px; font-weight: normal;text-align: center;">
  <span style="color: #2473b3; font-size: 16px; font-weight: bold;">Cristina Martínez García</span>
  </p>
   <p style="font-size: 16px; font-weight: normal;">
    Si deseas saber más sobre mí, te invito a visitar mi perfil de
    <a href="https://www.linkedin.com/in/cristina-mart%C3%ADnez-garc%C3%ADa-438209170/" style="color: #2473b3; font-size: 16px; font-weight: bold;">LinkedIn</a>,
    donde podrás conocer más acerca de mi experiencia y formación. Además, en mi
    <a href="https://cristina-mg.github.io/" style="color: #2473b3; font-size: 16px; font-weight: bold;">portfolio</a>
    encontrarás una selección de mis proyectos más recientes y desafiantes, donde aplico mis habilidades en
    <span style="color: #2473b3; font-size: 16px; font-weight: bold;">machine learning</span> y
    <span style="color: #2473b3; font-size: 16px; font-weight: bold;">análisis de datos</span>.
  </p>

  <!-- Iconos de LinkedIn y GitHub con reducción de separación -->
  <div style="text-align: center; margin-top: 20px;">
    <a href="https://www.linkedin.com/in/cristina-mart%C3%ADnez-garc%C3%ADa-438209170/" target="_blank" style="text-decoration: none; color: #FFFFFF; font-size: 24px; margin: 0 10px;">
      <i class="fab fa-linkedin"></i>
    </a>
    <a href="https://github.com/cristina-mg" target="_blank" style="text-decoration: none; color: #FFFFFF; font-size: 24px; margin: 0 10px;">
      <i class="fab fa-github"></i>
    </a>
  </div>
</div>'

# Texto del dataset Customer Churn
texto_dataset <- "
<p style='font-size: 17px; color: black; background-color: rgba(255, 0, 0, 0.0);'>
He utilizado el dataset <a href='https://www.kaggle.com/datasets/anandshaw2001/customer-churn-dataset' style='color: #1e90ff; text-decoration: underline;'>Customer Churn Dataset</a> de Kaggle, el cual proporciona una visión detallada del comportamiento de la fuga de clientes en diversas industrias. Este conjunto de datos global está meticulosamente curado para entender y predecir la conducta de los clientes que abandonan, ofreciendo perfiles detallados que incluyen datos demográficos, interacciones con productos y comportamientos bancarios.
</p>

<p style='font-size: 17px; color: black; background-color: rgba(255, 0, 0, 0.0);'>
En este estudio, he focalizado mi análisis en varias columnas clave del dataset. Primero, utilicé el <b>\"CreditScore\"</b> para evaluar la solvencia de los clientes en el momento de la recolección de datos, lo cual es crucial para entender su perfil financiero. La columna <b>\"Geography\"</b> me permitió explorar tendencias basadas en la ubicación geográfica de los clientes, proporcionando insights sobre cómo los factores regionales influyen en la fuga de clientes.
</p>

<p style='font-size: 17px; color: black; background-color: rgba(255, 0, 0, 0.0);'>
Además, consideré variables como <b>\"Gender\"</b> para analizar posibles disparidades en la fuga entre géneros, y <b>\"Age\"</b> para realizar análisis demográficos detallados. <b>\"Tenure\"</b> y <b>\"Balance\"</b> fueron fundamentales para entender la relación entre la antigüedad del cliente y su saldo en cuenta con la probabilidad de abandono.
</p>

<p style='font-size: 17px; color: black; background-color: rgba(255, 0, 0, 0.0);'>
También investigué cómo el número de productos <b>\"NumOfProducts\"</b>, la posesión de tarjeta de crédito <b>\"HasCrCard\"</b>, la actividad como miembro <b>\"IsActiveMember\"</b>, y el salario estimado <b>\"EstimatedSalary\"</b> influyen en la propensión a la fuga de clientes, utilizando <b>\"Exited\"</b> como variable objetivo para identificar aquellos clientes que abandonaron.
</p>

<p style='font-size: 17px; color: black; background-color: rgba(255, 0, 0, 0.0);'>
Este dataset es una valiosa herramienta para desarrollar modelos de aprendizaje automático destinados a identificar clientes en riesgo y diseñar estrategias de retención específicas, ayudando así a las empresas a mejorar la lealtad del cliente y reducir la fuga.
</p>

<p style='font-size: 17px; color: black; background-color: rgba(255, 0, 0, 0.0);'>
<img src='https://storage.googleapis.com/kaggle-datasets-images/4766255/8076321/d08605e3360b32526a2a71bce324be13/dataset-cover.jpeg?t=2024-04-09-18-44-56' style='width: 50%; height: auto; display: block; margin: 20px auto;' />
</p>
"


# Texto sobre clustering K-means
texto_kmeans <- "
<p style='font-size: 17px; color: black; background-color: rgba(255, 0, 0, 0.0);'>
Se ha aplicado el <strong>algoritmo de clustering K-means</strong> para segmentar a los clientes en cuatro grupos distintos: 
<strong>Cliente Celeste</strong>, <strong>Cliente Dorado</strong>, <strong>Cliente Esmeralda</strong> y <strong>Cliente Púrpura</strong>. 
La selección de estos cuatro clusters se basó en el <strong>método del codo</strong>, una técnica que ayuda a determinar el número 
óptimo de clusters en un conjunto de datos. Al analizar la gráfica resultante del método del codo, se observa claramente que el 
<strong>punto de inflexión</strong> se encuentra en cuatro clusters, lo que indica que este es el número óptimo para segmentar 
efectivamente a los clientes según sus características.
</p>

<p style='font-size: 17px; color: black; background-color: rgba(255, 0, 0, 0.0);'>
Cada grupo representa <strong>diferentes perfiles de clientes</strong> con <strong>comportamientos y características similares</strong>, 
permitiendo así a las empresas personalizar <strong>estrategias de marketing y retención</strong> más efectivas.
</p>

<p style='font-size: 17px; color: black; background-color: rgba(255, 0, 0, 0.0);'>
Este enfoque de clustering no solo ayuda a comprender mejor la <strong>base de clientes</strong>, sino que también facilita la implementación 
de <strong>estrategias específicas</strong> para cada grupo, mejorando así la <strong>satisfacción del cliente</strong> y reduciendo la 
<strong>tasa de abandono</strong>.
</p>
"


# Texto sobre el modelo de árbol de regresión
texto_arbol <- "
<p style='font-size: 17px; color: black; background-color: rgba(255, 0, 0, 0.0);'>
Se ha utilizado un <strong>modelo de árbol de regresión</strong> para predecir in situ la <strong>alta probabilidad de abandono de servicios</strong> por parte de clientes. Utilizando variables como la <strong>edad</strong>, <strong>género</strong>, <strong>si es miembro activo o no</strong>, <strong>si tiene tarjeta de crédito o no</strong>, <strong>país donde vive</strong>, el <strong>credit score</strong> y el <strong>número de productos contratados</strong>, el modelo ha demostrado una <strong>precisión del 85.93%</strong>. Durante el entrenamiento, se observó que la <strong>edad</strong> y el <strong>número de productos contratados</strong> fueron los <strong>predictores más importantes</strong>, seguidos por la <strong>membresía activa</strong> y la <strong>ubicación geográfica del cliente</strong>.
</p>

<p style='font-size: 17px; color: black; background-color: rgba(255, 0, 0, 0.0);'>
La <strong>matriz de confusión</strong> en el conjunto de prueba muestra que el modelo predijo correctamente <strong>2312 casos de clientes que no abandonaron el servicio</strong> (verdaderos negativos) y <strong>266 casos de clientes que sí abandonaron</strong> (verdaderos positivos). Sin embargo, se identificaron <strong>78 falsos positivos</strong> (clientes predichos como abandonados pero que no lo hicieron) y <strong>344 falsos negativos</strong> (clientes que abandonaron pero que el modelo predijo que no lo harían).
</p>
"


css <- "
#predict {
  background-color: blue;
  color: white;
  font-size: 20px;
  padding: 15px 32px;
  text-align: center;
  text-decoration: none;
  display: inline-block;
  margin: 4px 2px;
  cursor: pointer;
  border: none;
  border-radius: 12px;
}
"





# UI definition
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "FIDELIZACION"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visión General", tabName = "vision_general", icon = icon("dashboard")),
      menuItem("Segmentación de Clientes", tabName = "segmentacion", icon = icon("users")),
      menuItem("Predicción", tabName = "prediccion", icon = icon("chart-line")),
      menuItem("Información", tabName = "datos", icon = icon("table")),
      menuItem("Autor", tabName = "autor", icon = icon("user"))
    )
  ),
  dashboardBody(tags$head(
    tags$style(HTML("
        #predict {
          background-color: #2473b3;
          color: white;
          font-size: 20px;
          padding: 15px 32px;
          text-align: center;
          text-decoration: none;
          display: inline-block;
          margin: 4px 2px;
          cursor: pointer;
          border: none;
          border-radius: 12px;
        }
        /* Elimina el borde del encabezado del box */
        .box-header {display: none}



      "))
  ),
    tabItems(
      tabItem(tabName = "vision_general",
        fluidRow( 
          column(12,
                 shinydashboard::valueBoxOutput("total_no_churn"),
                 shinydashboard::valueBoxOutput("churn_rate"),
                 shinydashboard::valueBoxOutput("activity_rate")
        )),
        fluidRow(
          column(4, box(width = 12,headerBorder = FALSE, plotlyOutput("gauge_spain", height = "300px"))),
          column(4, box(width = 12,headerBorder = FALSE, plotlyOutput("gauge_france", height = "300px"))),
          column(4, box(width = 12,headerBorder = FALSE, plotlyOutput("gauge_germany", height = "300px")))
        ),
        fluidRow(
          column(8, box(width = 12,headerBorder = FALSE, plotlyOutput("plot_credit", height = "300px"))),
          column(4, box(width = 12,headerBorder = FALSE, plotlyOutput("plot_productos", height = "300px")))
        ),
        fluidRow(
          column(4, box(width = 12,headerBorder = FALSE, plotlyOutput("plot_age", height = "300px"))),
          column(4, box(width = 12,headerBorder = FALSE, plotlyOutput("plot_actividad", height = "300px"))),
          column(4, box(width = 12,headerBorder = FALSE, plotlyOutput("plot_salario", height = "300px")))
        ),
        fluidRow(
          column(4, box(width = 12,headerBorder = FALSE, plotlyOutput("plot_tenure", height = "300px"))),
          column(8, box(width = 12,headerBorder = FALSE, plotlyOutput("plot_balance", height = "300px")))
        ),
      ),
      
           tabItem(tabName = "segmentacion",
        fluidRow(
    column(4, selectInput("cluster", "Seleccione el Segmento", 
                          choices = c("Cliente Celeste" = 1, 
                                      "Cliente Oro" = 2, 
                                      "Cliente Esmeralda" = 3, 
                                      "Cliente Púrpura" = 4), 
                          selected = 1)),
    column(8, 
      shinydashboard::valueBoxOutput("segment_total_no_churn"),
      shinydashboard::valueBoxOutput("segment_churn_rate"),
      shinydashboard::valueBoxOutput("segment_activity_rate")
    )
  ),
        fluidRow(
          column(4, box(width = 12,title = NULL,headerBorder = FALSE,htmlOutput("texto_mostrado"), height = "300px")),
          column(8, box(width = 12,headerBorder = FALSE, htmlOutput("texto_mostrado_concreto", height = "300px")))),
  fluidRow(
          column(12, box(width = 12,headerBorder = FALSE, DTOutput("segment_data_table"), height = "200px"))
        ),
  fluidRow(
          column(6, box(width = 12,headerBorder = FALSE, plotlyOutput("S_plot_credit", height = "300px"))),
          column(6, box(width = 12,headerBorder = FALSE, plotlyOutput("S_plot_country", height = "300px")))
        ),
  fluidRow(
          column(6, box(width = 12, headerBorder = FALSE,plotlyOutput("S_plot_balance", height = "300px"))),
          column(6, box(width = 12,headerBorder = FALSE, plotlyOutput("S_plot_productos", height = "300px")))
        ),
  
  
  ),

      
      tabItem(tabName = "prediccion",
        fluidRow(
          column(6, selectInput("geography", "Geography", choices = c("Germany", "Spain", "France"), selected =  "Germany")),
          column(6, selectInput("gender", "Gender", choices = c("Male", "Female"), selected = "Male"))
        ),
        fluidRow(
          column(6, selectInput("hasCrCard", "Has Credit Card", choices = c("No" = 0, "Yes" = 1), selected = 1)),
          column(6, selectInput("isActiveMember", "Is Active Member", choices = c("No" = 0, "Yes" = 1), selected = 1))
        ),
        fluidRow(
          column(6, numericInput("creditScore", "Credit Score", value = 500, min = 0, max = 1000)),
          column(6, numericInput("tenure", "Años en la membresía", value = 1, min = 1, max = 10)),
        ),
        fluidRow(
          column(6, numericInput("age", "Age", value = 30, min = 18, max = 100)),
          column(6, numericInput("numOfProducts", "Number of Products", value = 1, min = 1, max = 4))),
        fluidRow(
          column(12,div(style = "text-align: center;"),actionButton("predict", "Predecir", style = "predict"))
        )
      ),
      tabItem(tabName = "datos", 
              fluidRow(
                column(4, selectInput("info", "Selecciona tipo de información", 
                                      choices = c("Información del Dataset" = 1,
                                                  "Tabla de datos" = 2,
                                                  "Segmentación de clientes" = 3,
                                                  "Predicción fidelización" = 4),
                                      selected = 1))),
              uiOutput("info_output")),
      tabItem(tabName = "autor",
              fluidRow(
          column(4, box(width = 12,headerBorder = FALSE,htmlOutput("sidebar_mostrado_autor"), height = "550px")),
           column(8, box(width = 12,headerBorder = FALSE,htmlOutput("texto_mostrado_autor"), height = "500px"))   
              )
    )
  )))


# Server logic
server <- function(input, output) {
  
  
  # General Overview
  output$total_no_churn <- shinydashboard::renderValueBox({
    
    shinydashboard::valueBox(
      sum(datos$Exited == 0),
      "Total Clientes actuales",
      icon = icon("user"),
      color = "olive"
    )
  })
  
  output$churn_rate <- shinydashboard::renderValueBox({
    churn_rate <- mean(datos$Exited) * 100
    shinydashboard::valueBox(
      paste0(round(churn_rate, 2), "%"),
      "Tasa de Churn",
      icon = icon("arrow-down"),
      color = "maroon"
    )
  })
  
  output$activity_rate <- shinydashboard::renderValueBox({
    active_rate <- mean(datos$IsActiveMember[datos$Exited == 0]) * 100
    shinydashboard::valueBox(
      paste0(round(active_rate, 2), "%"),
      "Tasa de Actividad clientes actuales",
      icon = icon("chart-line"),
      color = "teal"
    )
  })
  
  # Gauges by Country
  output$gauge_spain <- renderPlotly({ gauge_pais("Spain") })
  output$gauge_france <- renderPlotly({ gauge_pais("France") })
  output$gauge_germany <- renderPlotly({ gauge_pais("Germany") })
  
  # Scatter Plot: Age vs Exited
  output$plot_age <- renderPlotly({
    ggplotly(
      ggplot(datos, aes(x = Age, y = as.factor(Exited), color = as.factor(Exited))) +
        geom_jitter() +
        labs(title = "Retención por edad", 
             x = "Edad", 
             y = "Exited",
             color = "Exited") +
        scale_color_manual(values = c("0" = "#FCAE1E", "1" = "#bd2c54"))
    )
  })
  
  # Density Plot: CreditScore vs Exited
  output$plot_credit <- renderPlotly({
    ggplotly(
      ggplot(datos, aes(x = CreditScore, fill = as.factor(Exited))) +
        geom_density(alpha = 0.5) +
        labs(title = "Retención por Credit Score", 
             x = "Credit Score", 
             y = "Densidad",
             fill = "Exited") +
        scale_fill_manual(values = c("0" = "#87CEEB", "1" = "#bd2c54"))
    )
  })
  
  # Bar Plot: NumOfProducts vs Exited
  output$plot_productos <- renderPlotly({
    ggplotly(
      ggplot(datos, aes(x = as.factor(NumOfProducts), fill = as.factor(Exited))) +
        geom_bar(position = "fill") +
        labs(title = "Retención por nº de productos contratados", 
             x = "Número de productos", 
             y = "Proporción",
             fill = "Exited") +
        scale_fill_manual(values = c("0" = "#4aa25a", "1" = "#bd2c54"))
    )
  })
  
  # Boxplot: EstimatedSalary vs Exited
  output$plot_salario <- renderPlotly({
    ggplotly(
      ggplot(datos, aes(x = as.factor(Exited), y = EstimatedSalary, fill = as.factor(Exited))) +
        geom_boxplot() +
        labs(title = "Retención por Salario Estimado", 
             x = "Exited", 
             y = "Salario Estimado",
             fill = "Exited") +
        scale_fill_manual(values = c("0" = "#800080", "1" = "#bd2c54"))
    )
  })
  
  # Bar Plot: Tenure vs Exited
  output$plot_tenure <- renderPlotly({
    ggplotly(
      ggplot(datos, aes(x = as.factor(Tenure), fill = as.factor(Exited))) +
        geom_bar(position = "stack") +
        labs(title = "Retención por años en la membresía", 
             x = "Antigüedad (años)", 
             y = "Número de clientes",
             fill = "Exited") +
        scale_fill_manual(values = c("0" = "#1c649c", "1" = "#bd2c54"))
    )
  })
  
  # Bar Plot: IsActiveMember vs Exited
  output$plot_actividad <- renderPlotly({
    ggplotly(
      ggplot(datos, aes(x = as.factor(IsActiveMember), fill = as.factor(Exited))) +
        geom_bar(position = "fill") +
        labs(title = "Retención según su actividad", 
             x = "Es miembro activo", 
             y = "Proporción",
             fill = "Exited") +
        scale_fill_manual(values = c("0" = "#5ccbcb", "1" = "#bd2c54"))
    )
  })
  
  output$plot_balance <- renderPlotly({
    p <- ggplot(datos, aes(x = Balance, fill = factor(Exited))) +
      geom_histogram(position = "dodge") +
      scale_fill_manual(values = c("0" = "#5cb475", "1" = "#bd2c54"), labels = c("No Churn", "Churn")) +
      labs(title = "Retención según balance", x = "Balance", y = "Count")
    ggplotly(p)
  })
  
  
  
  # Texto dinámico para segmentación de clientes
  output$texto_mostrado <- renderUI({
    cluster <- input$cluster
    
    if (cluster == 1) {
      texto <- texto_cliente_celeste
    } else if (cluster == 2) {
      texto <- texto_cliente_oro
    } else if (cluster == 3) {
      texto <- texto_cliente_esmeralda
    } else {
      texto <- texto_cliente_purpura
    } 
    
    shiny::HTML(texto)  # Usar shiny::HTML en lugar de htmltools::HTML
  })
  
  # Texto dinámico para segmentación de clientes concreto
  output$texto_mostrado_concreto <- renderUI({
    cluster <- input$cluster
    
    if (cluster == 1) {
      texto <- texto_cliente_celeste_concreto
    } else if (cluster == 2) {
      texto <- texto_cliente_oro_concreto
    } else if (cluster == 3) {
      texto <- texto_cliente_esmeralda_concreto
    } else {
      texto <- texto_cliente_purpura_concreto
    } 
    
    shiny::HTML(texto)  # Usar shiny::HTML en lugar de htmltools::HTML
  })
  # Reactive function to filter data by selected segment
  segment_data <- reactive({
    cluster <- input$cluster
    datos[datos$Cluster == cluster, ]
  })

output$segment_total_no_churn <- shinydashboard::renderValueBox({
  col_seg <- switch(input$cluster,
                    "1" = "blue",
                    "2" = "orange",
                    "3" = "green",
                    "4" = "purple",
                    "black")  # Color por defecto si input$cluster no coincide con 1, 2, 3 o 4
  
  shinydashboard::valueBox(sum(segment_data()$Exited == 0), "Clientes actuales", icon = icon("user"), color = col_seg)
})

output$segment_churn_rate <- shinydashboard::renderValueBox({
  churn_rate <- mean(segment_data()$Exited) * 100
  shinydashboard::valueBox(paste0(round(churn_rate, 2), "%"), "Tasa de Churn", icon = icon("arrow-down"), color = "maroon")
})

output$segment_activity_rate <- shinydashboard::renderValueBox({
  active_rate <- mean(segment_data()$IsActiveMember[segment_data()$Exited == 0]) * 100
  shinydashboard::valueBox(paste0(round(active_rate, 2), "%"), "Actividad clientes actuales", icon = icon("chart-line"), color = "teal")
})

  # Bar Plot: NumOfProducts vs Exited for selected segment
  output$S_plot_productos <- renderPlotly({
    
      col_seg <- switch(input$cluster,
                    "1" = "#1c649c",
                    "2" = "#cd7434",
                    "3" = "#439a72",
                    "4" = "#524f8f",
                    "black")
    data <- segment_data()
    
    ggplotly(
      ggplot(data, aes(x = as.factor(NumOfProducts), fill = as.factor(Exited))) +
        geom_bar(position = "fill") +
        labs(title = "Retención por productos", 
             x = "Número de productos", 
             y = "Proporción",
             fill = "Exited") +
        scale_fill_manual(values = c("0" = col_seg , "1" = "#dc3464"))
    )
  })
  
  output$segment_data_table <- renderDT({
    datos %>%
      datatable(options = list(
        scrollX = TRUE,
        paginate = T,
        lengthMenu = c(5, 10, 15),
        pageLength = 5
      )) %>% DT::formatStyle(columns = names(data), color="purple")
  })
  

  output$S_plot_balance <- renderPlotly({
       col_seg <- switch(input$cluster,
                    "1" = "#1c649c",
                    "2" = "#cd7434",
                    "3" = "#439a72",
                    "4" = "#524f8f",
                    "black")
    data <- segment_data()
    ggplotly(
      ggplot(data, aes(x = Balance, fill = as.factor(Exited))) +
        geom_histogram(position = "dodge", bins = 30) +
        labs(title = "Retención por balance", 
             x = "Balance", 
             y = "Cantidad",
             fill = "Exited") +
        scale_fill_manual(values = c("0" = col_seg , "1" = "#dc3464"))
    )
  })
  
  output$S_plot_credit <- renderPlotly({
       col_seg <- switch(input$cluster,
                    "1" = "#1c649c",
                    "2" = "#cd7434",
                    "3" = "#439a72",
                    "4" = "#524f8f",
                    "black")
    data <- segment_data()
    ggplotly(
      ggplot(data, aes(x = CreditScore, fill = as.factor(Exited))) +
        geom_density(alpha = 0.5) +
        labs(title = "Retención por CreditScore", 
             x = "Credit Score", 
             y = "Densidad",
             fill = "Exited") +
        scale_fill_manual(values = c("0" = col_seg , "1" = "#dc3464"))
    )
  })
  
  output$S_plot_country <- renderPlotly({
    col_seg <- switch(input$cluster,
                    "1" = "#1c649c",
                    "2" = "#cd7434",
                    "3" = "#439a72",
                    "4" = "#524f8f",
                    "black")
    
    data <- segment_data()
    ggplotly(
      ggplot(data, aes(x = Geography, fill = as.factor(Exited))) +
        geom_bar(position = "fill") +
        labs(title = "Retención por pais", 
             x = "País", 
             y = "Proporción",
             fill = "Exited") +
        scale_fill_manual(values = c("0" = col_seg , "1" = "#dc3464"))
    )
  })
  
  


# Render data table
  output$data_table <- renderDataTable({
    datatable(datos, options = list(pageLength = 10, autoWidth = TRUE))
  })



  output$texto_mostrado_autor <- renderUI({
    shiny::HTML(texto_autor)  # Usar shiny::HTML en lugar de htmltools::HTML
  })
  
    output$sidebar_mostrado_autor <- renderUI({
      shiny::HTML(foto_autor)  # Usar shiny::HTML en lugar de htmltools::HTML
  })
    
  # Predicción
  observeEvent(input$predict, {
    new_data <- data.frame(
      CreditScore = input$creditScore,
      Geography = input$geography,
      Gender = input$gender,
      Age = input$age,
      Tenure = input$tenure,
      NumOfProducts = input$numOfProducts,
      HasCrCard = as.numeric(input$hasCrCard),
      IsActiveMember = as.numeric(input$isActiveMember)
    )
    
    pred <- predict(modelo_arbol, new_data)

      if (pred[1,1] >= 0.5) {
        prediction_messsage <- paste("Alta probabilidad de que el usuario continue con el servicio",round(pred[1,1]*100,2), "%")
        tipo <- "success"
      } else {
        prediction_messsage <- paste("Baja probabilidad de que el usuario continue con el servicio",round(pred[1,1]*100,2), "%")
        tipo <- "warning"
     }
    
    shinyalert(title = prediction_messsage,text="",  type = tipo)
    

 
  })
  
  output$info_output <- renderUI({
    info_type <- as.integer(input$info)
    
    if (info_type == 1) {
      # Cuadro de texto para información del dataset
      fluidRow(
        column(12, 
               HTML(texto_dataset)
        )
      )
    } else if (info_type == 2) {
      # Tabla de datos del dataset (ejemplo con tabla estática)
      fluidRow(
        column(12,
               box(width = 12, 
                   DT::dataTableOutput("data_table")
               )
        )
      )
    } else if (info_type == 3) {
      # Cuadro de texto para segmentación de clientes
      fluidRow(
        column(6, 
               HTML(texto_kmeans)
        ),column(6, tags$img(src = "metodo_del_codo.png", height = "380px"))
      )
    } else if (info_type == 4) {
      # Cuadro de texto para predicción de fidelización
      fluidRow(
        column(6, 
               HTML(texto_arbol)),
               
        column(6, tags$img(src = "img_mconfusion.png", height = "280px"), tags$img(src = "arbol.png", height = "500px"))
        )
      
    }
  })
  
  # Ejemplo de tabla estática para mostrar en "Tabla de datos"
  output$data_table <- DT::renderDataTable({
    data <- datos  
    DT::datatable(data)
  })
}


# Run the application
shinyApp(ui = ui, server = server)


