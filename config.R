# UTILS
# URLs 
url_casos = 'https://www3.gobiernodecanarias.org/noticias/la-consejeria-de-sanidad-constata-657-casos-acumulados-de-coronavirus-covid-19/'
datos_uci = 'https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_uci_long.csv'
datos_fallecidos = 'https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos_long.csv'
datos_altas = 'https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_altas_long.csv'
datos_hospitalizados = 'https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_hospitalizados_long.csv'
informe_epi = "https://www3.gobiernodecanarias.org/sanidad/scs/content/dcb400c5-6504-11ea-9a8e-719d4b52bf6c/InformeCasosCOVID-19.pdf"

# Deberia ser siempre FALSE. TRUE es solo para actualizar los datos (offline)
run_model  = FALSE

# A veces los datos de casos acumulados hay que construirlos manualmente
# en ese caso fijar a FALSE
build_data_from_web = FALSE