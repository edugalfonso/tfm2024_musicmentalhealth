# TFM Estadística Aplicada
Desarrollo de un modelo estadístico para predecir el efecto de la música en la salud mental
## Herramientas utilizadas
Se utilizó una encuesta realizada en la Universidad de Washington en 2022 (obtenidos en kaggle.com), y otra hecha en España en verano de 2024
Lenguaje utilizado: R
Modelización estadística: Árboles de decisión, Boosted Trees, Random Forest, SVM, regresiones, Validación cruzada k-fold, kNN
## Metodología
- Limpieza y preparación de los datos
- Análisis exploratorio de las variables
- Análisis de relaciones entre las variables
- Modelos sobre la salud mental
- Modelos predictivos
- Aplicación de los modelos predictivos sobre la nueva encuesta
- Conclusiones
## Archivos del repositorio
scripts/
- mxmh.R: Primera parte del proyecto (análisis de la encuesta original)
- mxmh2.R: Segunda parte del proyecto (comparación de ambas encuestas y validación de los modelos sobre los nuevos datos)
data/
- mxmh_survey_results.csv: Datos de la encuesta original de 2022
- mxmh2.csv: Datos de la encuesta con compensación utilizada para evitar la sobrerrepresentación de una respuesta categórica
- mxnew.csv: Datos de la nueva encuesta realizada en 2024
MxMH24.pdf: Memoria del proyecto, con el procedimiento, resultados y conclusiones
