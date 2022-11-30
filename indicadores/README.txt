El código en esta carpeta genera los gráficos y tablas de los indicadores a usar en la web, con los siguientes pasos:

1. Abrir el proyecto de R DESCA-app
2. Abrir el script data-raw.R y correrlo (en el único caso en que se tiene que editar es si agregan nuevos cortes, 
simplemente agregan un nuevo tipo "text" cuando leen la base, al final)
3. El Paso 2 genera una nueva base, data_motor.rda que es el insumo para crear los gráficos y tablas
4. Abrir el script graficos-y-tablas.R y correrlo. Este generará visualizaciones y tablas para todos los indicadores y
que se guardarán en las carpetas "viz" y "fichas" respectivamente así como los de población



También está el código para generar las notas conceptuales en pdf, con los siguientes pasos:

1. Entrar a la carpeta Notas y abrir localmente el proyecto Notas_conceptuales.Rproj
2. Correr el archivo params.R
3. Revisar que los pdf hayan quedado ok, en caso de salto de página imprimr en pdf el html correspondiente ajustando zoom.
4. Copiar los pdf en el sitio wordpress (y enlazar en la línea de tiempo en caso de ser nuevos).