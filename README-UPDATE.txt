Para actualizar la shiny agregando nuevos valores, indicadores o cortes:

1. Copiar la base Base_mirador_desca.xlsx del repositorio de la Web del Mirador 
2. Abrir el proyecto de R Mirador-DESCA-app 
3. Desde ahí abrir el script data-raw.R y correrlo, genera la base data_motor
(si agregan un nuevo corte tienen que agregar un nuevo tipo de variable cuando leen el texto,
un "text" al final)
4. Abren el script "app" y le dan Run App (arriba a la derecha con una flecha verde) y chequean 
en la visualización previa que se hayan incorporado bien los cambios.
5. Para que los cambios se reflejen en el link (primero hacer el commit y push en el repo) y luego
arriba a la derecha (a la derecha de run app hay un ícono celeste) tocan eso y le dan publish
(a la versión dentro del servidor de Banco de Datos), esto demora un poco pero al final si todo salió
bien, abre la shiny en el buscador que usen y ya queda actualizada