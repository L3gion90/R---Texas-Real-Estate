texasDS <- read.csv("realestate_texas.csv", sep = ",")
str(texasDS)
library(dplyr)
library(ggplot2)
# Analizziamo le variabili di cui è composto il dataset.
df_summary <- texasDS %>%
  summarise(
    city_type = typeof(city),
    year_type = typeof(year),
    month_type = typeof(month),
    sales_type = typeof(sales),
    volume_type = typeof(volume),
    median_price_type = typeof(median_price),
    listings_type = typeof(listings),
    months_inventory_type = typeof(months_inventory)
  )
# Trasformiamo in fattori le variabili categoriche e rendiamo la visualizzazione più chiara trasformando months.
texasDS$city <- as.factor(texasDS$city)
texasDS$year <- as.factor(texasDS$year)
texasDS$month <- factor(texasDS$month, levels = 1:12, labels = month.abb)

levels(texasDS$city)
levels(texasDS$month)
levels(texasDS$year)
library(summarytools)
library(e1071)

desc_table <- descr(texasDS, stats.exclude= c("city", "year", "month"))

options(scipen = 999)

#Manca la varianza, che è ottenibile elevando al quadrato la deviazione standard. 
#Per completezza mostriamola per tutte le variabili di nostro interesse:
variance_results <- texasDS %>%
  select(-city, -year, -month) %>%
  summarise_all(var)

print(desc_table)
print(variance_results)

tabella_frequenze_city <- as.data.frame(table(texasDS$city))
tabella_frequenze_year <- as.data.frame(table(texasDS$year))
tabella_frequenze_month <- as.data.frame(table(texasDS$month))

# Definisci i valori di Q3 e IQR
Q3 <- c(2128.00, 150100.00, 11.00, 248.00, 40.90)
IQR <- c(1029.50, 32750.00, 3.15, 120.00, 23.23)

# Calcola i limiti superiori per ciascuna variabile
limite_superiore <- Q3 + 1.5 * IQR

# Creazione di un dataframe con i valori MAX e limiti superiori
risultati_df <- data.frame(
  variabile = c("listings", "median_price", "months_inventory", "sales", "volume"),
  max_value = c(3296.00, 180000.00, 14.90, 423.00, 83.55),
  limite_superiore 
)

# Aggiungi una colonna per indicare se è un outlier o no
risultati_df$outlier <- ifelse(risultati_df$max_value > risultati_df$limite_superiore,
                               "POSSIBILI OUTLIER", "NESSUN OUTLIER")

# Mostra i risultati
print(risultati_df[, c("variabile", "max_value", "limite_superiore", "outlier")])

# Creazione del dataframe con i dati forniti
df <- data.frame(
  Variable = c("Listings", "Median_price", "Months_inventory", "Sales", "Volume"),
  CV = c(0.4330833, 0.1708218, 0.25060306, 0.4142203, 0.5370536)
)

# Estrai il coefficiente di variazione in una variabile separata moltiplicata per cento
df$CV_Percent <- df$CV * 100

# Stampa del dataframe con i risultati
print(df)

#Studiamo la variabile median price e costruiamo la distribuzione di frequenza.
median_price <- texasDS$median_price
num_classi <- ceiling(log2(length(median_price)) + 1)
intervalli_classe <- seq(min(median_price), max(median_price),
                         length.out = num_classi + 1)
etichette_classe <- paste(intervalli_classe[-length(intervalli_classe)],
                          intervalli_classe[-1], sep="-")
classi <- cut(median_price, breaks = intervalli_classe,
              labels = etichette_classe, include.lowest = TRUE)

# Creiamo un dataframe con i dati della tabella delle frequenze e rinominiamo le tabelle
tabella_frequenze_median_price <- as.data.frame(table(classi))

names(tabella_frequenze_median_price) <- c("Intervallo di Classe", "Frequenza")

# Costruiamo il grafico a barre utilizzando ggplot2
grafico_a_barre <- ggplot(data = tabella_frequenze_median_price, 
                          aes(x = `Intervallo di Classe`, y = Frequenza, fill = Frequenza)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frequenza, y = Frequenza), vjust = -0.2, size = 3, color = "white",
            position = position_stack(vjust = 0.5)) +  # Etichette dei totali all'interno delle colonne
  labs(x = "Intervallo di Classe", y = "Frequenza",
       title = "Distribuzione delle Frequenze di median_price") +  
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 11),
        axis.title = element_text(size = 12, face = "bold"),  # Stile dei titoli degli assi
        plot.title = element_text(size = 16, face = "bold"),  # Stile del titolo del grafico
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))  # Linee della griglia

# Visualizziamo il grafico
print(grafico_a_barre)

# Calcoliamo l'indice di Gini per median_price
library(ineq)

frequencies_median_price <- tabella_frequenze_median_price$Freq
gini_index_median_price <- ineq::Gini(frequencies_median_price)
cat("Indice di Gini per median_price:", gini_index_median_price, "\n")

# Calcoliamo l'indice di Gini per la variabile city

frequencies_city <- tabella_frequenze_city$Freq
gini_index_city <- ineq::Gini(frequencies_city)
cat("Indice di Gini per la variabile city:", gini_index_city, "\n")

#Studio delle probabilità
prob_citta_beaumont <- sum(texasDS$city == "Beaumont") / nrow(texasDS)
cat("Probabilità di scegliere una riga con la città di Beaumont:",
    prob_citta_beaumont, "\n")

prob_mese_luglio <- sum(texasDS$month == "Jul") / nrow(texasDS)
cat("Probabilità di scegliere una riga con il mese di Luglio:",
    prob_mese_luglio, "\n")

prob_dicembre_2012 <- sum(texasDS$month == "Dec" & texasDS$year == 2012) / nrow(texasDS)
cat("Probabilità di scegliere una riga con il mese di dicembre 2012:",
    prob_dicembre_2012, "\n")

#Creazionee della colonna prezzo medio
texasDS <- texasDS %>% 
  mutate(average_price = volume / sales)
str(texasDS)

#creazione della colonna efficacy
texasDS <- texasDS %>%
  mutate(efficacy = sales / listings)
str(texasDS)

#creazione dei summary specifici
generate_summary <- function(data, grouping_var, value_vars) {
  summary_df <- data %>%
    group_by({{ grouping_var }}) %>%
    summarise(across({{ value_vars }},
                     list(mean = ~mean(., na.rm = TRUE),
                     sd = ~sd(., na.rm = TRUE)), .names = "{col}_{fn}"))
  
  return(summary_df)
}

# Creiamo le funzione per i summary per mese, città e anno.
summary_month <- generate_summary(texasDS,
                                  month, c(listings, sales, average_price))

summary_city <- generate_summary(texasDS,
                                 city, c(listings, sales, average_price))

summary_year <- generate_summary(texasDS,
                                 year, c(listings, sales, average_price))
# Summary
print(summary_month)
print(summary_city)
print(summary_year)


#Funzione per creare i grafici sui summary interessati.
create_bar_chart <- function(data, x_var, y_var, title) {
  ggplot(data, aes(x = {{ x_var }}, y = {{ y_var }})) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = round({{ y_var }}, 2)), vjust = -0.5, size = 3) + 
    labs(title = title, x = as_label(quo(x_var)), y = as_label(quo(y_var)))
}

# grafici per month
bar_chart_month_listings <- create_bar_chart(summary_month, month, listings_mean, "Mean Listings per Month")
bar_chart_month_sales <- create_bar_chart(summary_month, month, sales_mean, "Mean Sales per Month")
bar_chart_month_average_price <- create_bar_chart(summary_month, month, average_price_mean, "Mean Average Price per Month")

# Grafici per city
bar_chart_city_listings <- create_bar_chart(summary_city, city, listings_mean, "Mean Listings per City")
bar_chart_city_sales <- create_bar_chart(summary_city, city, sales_mean, "Mean Sales per City")
bar_chart_city_average_price <- create_bar_chart(summary_city, city, average_price_mean, "Mean Average Price per City")

# Grafici per year
bar_chart_year_listings <- create_bar_chart(summary_year, year, listings_mean, "Mean Listings per Year")
bar_chart_year_sales <- create_bar_chart(summary_year, year, sales_mean, "Mean Sales per Year")
bar_chart_year_average_price <- create_bar_chart(summary_year, year, average_price_mean, "Mean Average Price per Year")

# Stamp dei grafici
print(bar_chart_month_listings)
print(bar_chart_month_sales)
print(bar_chart_month_average_price)
print(bar_chart_city_listings)
print(bar_chart_city_sales)
print(bar_chart_city_average_price)
print(bar_chart_year_listings)
print(bar_chart_year_sales)
print(bar_chart_year_average_price)

# Boxplot per confrontare la distribuzione del prezzo mediano delle case tra le varie città
ggplot(texasDS, aes(x = city, y = median_price, fill = city)) +
  geom_boxplot() +
  labs(title = "Distribuzione del Prezzo Mediano delle Case per Città",
       x = "Città",
       y = "Prezzo Mediano") +
  theme_minimal() +
  theme(legend.position="none") 

# Boxplot per confrontare la distribuzione del valore totale delle vendite tra le varie città e tra i vari anni
ggplot(texasDS, aes(x = city, y = volume, fill = as.factor(year))) +
  geom_boxplot() +
  labs(title = "Distribuzione del Valore Totale delle Vendite per Città e Anno",
       x = "Città",
       y = "Valore Totale delle Vendite",
       fill = "Anno") +
  theme_minimal()

# Grafico a barre normalizzato, totale delle vendite per città e anno.
texasDS |> 
  mutate(perc = sales / sum(sales), .by = c(year, month)) |> 
  ggplot(aes(x = month, y = sales, fill = city)) +
  geom_bar(stat = 'identity', position = 'fill') +
  facet_grid(rows = vars(year), scales = 'free_y') +
  labs(x = 'Mese', y = 'Percentuale del TOtale delle Vendite', fill = 'Città',
       title = 'Confronto normalizzato del Totale delle Vendite') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = 'top',
    strip.text.y = element_text(angle = 0)
  ) +
  scale_fill_brewer(palette = 'Set2') +
  geom_text(aes(label = scales::percent(perc, accuracy = .1)),
            position = position_fill(vjust = 0.5))

#linechart sul prezzo medio
ggplot(data = texasDS, aes(x = year, y = average_price, group = city, color = city)) +
  geom_line(size = 1) + 
  scale_y_log10() + 
  labs(title = "Andamento del prezzo medio degli immobili per città e anno",
       x = "Anno",
       y = "Prezzo medio") +
  theme_minimal() +
  theme(legend.position="top",  
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray"),
        plot.caption = element_text(size = 10, color = "gray"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = "Set2") 

library(ggplot2)
library(zoo)


# Supponendo che il tuo dataset abbia le colonne "year", "month", "average_price" e "city"
texasDS$date <- as.Date(paste(texasDS$year, as.integer(texasDS$month), "01", sep = "-"), format = "%Y-%m-%d")
head(texasDS)

ggplot(data = texasDS, aes(x = date, y = average_price, group = city, color = city)) +
  geom_line(size = 1) + 
  scale_y_log10() + 
  labs(title = "Andamento del prezzo medio degli immobili per città, anno e mese",
       x = "Data",
       y = "Prezzo medio") +
  theme_minimal() +
  theme(legend.position="top",  
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray"),
        plot.caption = element_text(size = 10, color = "gray"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = "Set2")

# Installa il pacchetto se non è già installato
# install.packages("scales")

# Carica il pacchetto scales
library(scales)

# Esempio di codice ggplot migliorato
ggplot(data = texasDS, aes(x = date, y = average_price, group = city, color = city)) +
  geom_line(size = 1) + 
  scale_y_log10(labels = scales::dollar_format(scale = 0.001, suffix = "k")) +  # Aggiunta di etichette in formato dollaro
  scale_x_date(labels = date_format("%b %Y"), date_breaks = "1 month") +  # Formato dei mesi sull'asse X
  labs(title = "Andamento del prezzo medio degli immobili per città, anno e mese",
       x = "Data",
       y = "Prezzo medio (log scale)") +
  theme_minimal() +
  theme(legend.position = "top",  
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray"),
        plot.caption = element_text(size = 10, color = "gray"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = "Set2")

# Esempio di codice ggplot migliorato
ggplot(data = texasDS, aes(x = date, y = average_price, group = city, color = city)) +
  geom_line(size = 1) + 
  scale_y_log10(labels = scales::dollar_format(scale = 0.001, suffix = "k")) +
  scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "1 month") +
  labs(title = "Andamento del prezzo medio degli immobili per città, anno e mese",
       x = "Data",
       y = "Prezzo medio (log scale)") +
  theme_minimal() +
  theme(legend.position = "top",  
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray"),
        plot.caption = element_text(size = 10, color = "gray"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_color_brewer(palette = "Set2")

# Esempio di codice ggplot migliorato
ggplot(data = texasDS, aes(x = date, y = average_price, group = city, color = city)) +
  geom_line(size = 1) + 
  scale_y_log10(labels = scales::dollar_format(scale = 0.001, suffix = "k")) +
  scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "1 month") +
  labs(title = "Andamento del prezzo medio degli immobili per città, anno e mese",
       x = "Data",
       y = "Prezzo medio (log scale)") +
  theme_minimal() +
  theme(legend.position = "top",  
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray"),
        plot.caption = element_text(size = 10, color = "gray"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +  # Regola l'angolo e l'allineamento etichette X
  scale_color_brewer(palette = "Set2")

# Esempio di codice ggplot migliorato
ggplot(data = texasDS, aes(x = date, y = average_price, group = city, color = city)) +
  geom_line(size = 1.5, alpha = 0.7) +  # Aumenta lo spessore delle linee e leggera trasparenza
  scale_y_log10(labels = scales::dollar_format(scale = 0.001, suffix = "k")) +
  scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "1 month") +
  labs(title = "Andamento del prezzo medio degli immobili per città, anno e mese",
       x = "Data",
       y = "Prezzo medio (log scale)") +
  theme_minimal() +
  theme(legend.position = "top",  
        plot.title = element_text(size = 18, face = "bold"),  # Aumenta la dimensione del titolo
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        panel.grid.major = element_line(color = "lightgray", size = 0.2),  # Aggiungi linee di griglia più chiare
        panel.grid.minor = element_blank(),  # Rimuovi linee di griglia minori
        axis.line.x = element_line(color = "black", size = 1)) +  # Aumenta la dimensione delle linee dell'asse X
  scale_color_brewer(palette = "Set2") +
  theme(legend.background = element_rect(fill = "white"))  # Sfondo della legenda bianco per maggiore leggibilità


head(texasDS)

library(ggplot2)
library(scales)

# Somma un piccolo valore costante ai valori di average_price per evitare log(0)
texasDS$average_price <- texasDS$average_price + 0.001

# Grafico ggplot con le modifiche
ggplot(data = texasDS, aes(x = date, y = average_price, group = city, color = city)) +
  geom_line(size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = as.Date(paste(2010:2022, "-01-01", sep = "-")), 
             linetype = "dashed", color = "gray", size = 1.5) +
  scale_y_log10(labels = scales::dollar_format(scale = 0.001, suffix = "k")) +
  scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "1 month") +
  labs(title = "Andamento del prezzo medio degli immobili per città, anno e mese",
       x = "Data",
       y = "Prezzo medio (log scale)") +
  theme_minimal() +
  theme(legend.position = "top",  
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "gray", hjust = 0.5),
        plot.caption = element_text(size = 10, color = "gray"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        panel.grid.major = element_line(color = "lightgray", size = 0.2),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = 1)) +
  scale_color_brewer(palette = "Set2") +
  theme(legend.background = element_rect(fill = "white"))

