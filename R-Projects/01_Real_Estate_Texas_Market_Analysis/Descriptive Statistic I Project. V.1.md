---
title: "Descriptive Statistic Project"
author: "Francesco Innocenti"
date: "`r format(Sys.Date(), '%d/%m/%Y')`"
output:
  html_document:
    self_contained: false
editor_options: 
  markdown: 
    wrap: 72
---

## **ANALISI DEL MERCATO IMMOBILIARE DEL TEXAS**

### *1. ANALISI DELLE VARIABILI*

```{r}

library(knitr)
library(dplyr)

#Load the data into R (CSV file)
data<-read.csv("Real Estate Texas.csv") 

#Exploring the Dataset: structure and variable types
#First rows of the Dataset
knitr::kable(head(data), caption = "First rows of the Dataset")
#Variables Names
kable(data.frame(Variables=names(data)), caption = "Variables Names")

#Dataset structure in tabular form
str_df <- data.frame(
  Variable = names(data),
  Class = sapply(data, class)
)
kable(str_df, caption = "Dataset Structure")

#Descriptive statistics
kable(summary(data), caption = "Descriptive statistics")
```

**Analisi Variabili del Dataset**

(Tipo Variabile \| Descrizione \| Tipo Analisi )

-   **City** : Qualitativa Nominale \| indica le città a cui si
    riferiscono i dati \| confronto vendite/volume tra città o aree
    geografiche.

-   **Year** : Quantitativa Continua (in questo contesto trattata come
    qualitativa ordinale) \| Anno di osservazione \| analisi di trend.

-   **Month** : Qualitativa nominale (ciclica) \| indica Mese di
    osservazione \| analisi di stagionalità.

-   **Sales** : Quantitativa Discreta \| numero totale di case vendute.
    \| Analisi Descrittiva della domanda immobiliare (media,
    dispersione, distribuzione)

-   **Volume** : Quantitativa continua \| Valore totale delle vendite
    (in milioni di dollari). \| Analisi descrittiva del giro d’affari
    del mercato (media, dispersione, distribuzione)

-   **Median Price** : Quantitativa Continua \| prezzo mediano di
    vendita (in dollari) \| Analisi Descrittiva sul livello dei prezzi
    delle case

-   **Listings** : Quantitativa Discreta \| numero totale di annunci
    attivi \| Analisi dell’offerta immobiliare (media, dispersione,
    distribuzione).

-   **months_inventory** : Quantitativa Continua \| Tempo (in mesi)
    stimato per vendere tutte le inserzioni correnti. \| Indicatore
    dell’equilibrio domanda/offerta.

**Tipo Analisi Possibili :**

1.  City -\> Confronto tra gruppi

2.  Year / Month -\> Trend e Stagionalità

3.  Variabili Quantitative -\> Distribuzioni, Variabilità e Correlazione

### *2. INDICI DI VARIABILITA', POSIZIONE E FORMA*

-   Per **Variabili Quantitative** ( *`sales`*, *`volume`*,
    `median_price`, `listings` , `month_inventory` )

    -\> Calcolo indici di :

    -   **Posizione** : Media, Mediana, Minimo , Massimo , Quartili

    -   **Variabilità** : Varianza , Deviazione Standard , Range

    -   **Forma** : Asimmetria , Curtosi

-   Per **variabili quantitative/temporali** (`city`, `year` , `month`)

    -\> Calcolo Distribuzione di frequenza

```{r}

library(knitr)
library(moments)

#Define the quantitative variables
var_quant <- c("sales","volume","median_price","listings","months_inventory")

#Function to calculate the requested indices
calc_stas<- function(x){
  c(
    Media = mean(x, na.rm = TRUE),
    Mediana = median(x, na.rm = TRUE),
    Minimo = min(x, na.rm = TRUE),
    Massimo = max(x, na.rm = TRUE),
    Quartile_1 = quantile(x, 0.25, na.rm = TRUE),
    Quartile_3 = quantile(x, 0.75, na.rm = TRUE),
    Varianza = var(x, na.rm = TRUE),
    Dev_Std = sd(x, na.rm = TRUE),
    Range = diff(range(x, na.rm = TRUE)),
    IQR = IQR(x, na.rm = TRUE),
    Asimmetria = skewness(x, na.rm = TRUE),
    Curtosi = kurtosis(x, na.rm = TRUE)
  )
}
#Apply the function to the qualitative variables
stats_df <- sapply(data[var_quant], calc_stas)%>%
  round(2) %>%
  as.data.frame()
stats_df<- tibble::rownames_to_column(stats_df,var= "Measure")

#Final table
kable(stats_df, caption = "Indices by size (rows) and variable (columns)")
```

#### Commento sulle Struttura delle Variabili :

1.  **Sales** :

    Distribuzione Asimmetrica : *Media (192.3) e Mediana (175.3) non
    coincidono*. Asimmetria Positiva (*Asim_Index*: 0.72) con pochi
    picchi molto alti e infatti sono più frequenti i valori bassi
    (quindi con una coda verso Dx). La curtosi è sotto 3 (*Kurtosis* :
    2.69) , sottolineando che la distribuzione è più piatta di una
    distribuzione normale. La variabilità è leggermente alta (*sd:
    79.7*) e Range e IQR sono molto diversi tra loro e quindi potrebbe
    suggerire presenza di outlier.

2.  **Volume** :

    Distribuzione Asimmetrica : *Media (31.01) e Mediana (27.062) non
    coincidono*. Asimmetria Positiva (*Asim_Index*: 0.84,) con pochi
    picchi molto alti e infatti sono più frequenti i valori bassi
    (quindi con una coda verso Dx). La curtosi è sopra 3 (*Kurtosis* :
    3.17) , indicando che le code sono un po più pesanti e quindi
    leggermente leptocurtiche. La variabilità è moderata (*sd: 16.7*) ma
    la differenza marcata tra Range e IQR potrebbe suggerire anche in
    questo caso presenza di outlier.

3.  **Median Price** :

    Distribuzione quasi simmetrica : Media e Mediana quasi coincidono,
    *AVG: 132 665,42, Mediana: 134 500* ), con lieve asimmetria negativa
    (*Asim_Index*: -0.36) . La curtosi è sotto 3 , sottolineando che la
    distribuzione è più piatta di una distribuzione normale (*Kurtosis*
    : 2.38). Tuttavia La variabilità è molto alta (*sd : 22662.15*)
    rispetto alla media, suggerendo un’alta eterogeneità dei prezzi.

4.  **Listings** :

    Distribuzione leggermente asimmetrica : Media e Mediana quasi
    coincidono (*AVG: 1738.02, Mediana: 1618.5*). Asimmetria Positiva
    (*Asim_Index*: 0.65). La curtosi è molto sotto 3 (*Kurtosis* :
    2.21), sottolineando che la distribuzione è più piatta di una
    distribuzione normale (distribuzione platicurtica) . La dispersione
    è elevata come mostrato dalla deviazione standard molto ampia (*sd:*
    752.7*).* Inoltre l'asimmetria e la curtosi indicano che la
    distribuzione è leggermente spostata verso valori alti.

5.  **Month Inventory** :

    Distribuzione sostanzialmente simmetrica : Media e Mediana quasi
    coincidono , con bassa variabilità (*sd : 2.3*) e una bassa
    asimmetria (*Asim_Index*: 0.04, *Kurtosis* : 2.82). La sua
    distribuzione si avvicina a quella di una distribuzione normale.

### *3. IDENTIFICAZIONE DELLE VARIABILI CON MAGGIORE VARIABILITA' E ASIMMETRIA*

Coefficienti da impiegare per confrontare "Variabilità" & "Asimmetria" :

-   **Variabilità** : utilizzo il coefficiente di variazione ( CV = sd /
    mean ), che essendo un valore percentuale permette il confronto tra
    scale diverse.

-   **Asimmetria** : utilizzo la funzione "skewness" e verifico la
    variabile con valore più alto (in questo caso distribuzione più
    asimmetrica).

```{r}
library(moments)
var<- c("sales","volume", "median_price", "listings", "months_inventory") 
#function
ds<- function(x){
  return(sd(x))
}
cv <- function(x){
  return(sd(x, na.rm = TRUE)/mean(x, na.rm= TRUE)*100)
  }
sk <- function(x){
  return(skewness(x))
}
#apply to the variables
DV<-sapply(data[var],ds)
CV<- sapply(data[var], cv)
SK<- sapply(data[var], sk)

#combine into table
results <- data.frame(
  Variable = var,
  Deviazione_Std = round(DV, 2),
  Coeff_Var = round(CV, 2),
  Skewness = round(SK, 2)
)
#sorting
return_CV<- results[order(-results$Coeff_Var),]
return_SK<- results[order(-results$Skewness),]

# Tabellar Output
kable(results, caption ="Standard deviation, Coefficient of variation, Skewness")
kable(return_CV, caption = "Variables sorted by coefficient of variation (in descending order)")
kable(return_SK, caption = "Variables sorted by skewness (in descending order)")
```

#### Risultato :

1.  **Variabilità ( Deviazione Standard & Coefficiente di Variazione CV
    )**

    -   La Variabile con la maggiore deviazione standard è
        "median_price" (22662) , ma ha diversa unità di misura rispetto
        alle altre variabili e questo influisce sulla sua scala di
        grandezza.

    -   Per un confronto più equo , utilizzo il CV (coefficiente di
        variazione): la variabile con una maggiore variabilità relativa
        risulta " volume " (CV = 53,7%). Questo indica che nel caso
        della variabile "volume" i dati si disperdono del 53,7% rispetto
        alla media.

2.  **Asimmetria ( Skewness )**

    -   La variabile che risulta più asimmetrica è " volume " , con
        indice di skewness pari a 0,88 (Asimmetria Positiva e quindi
        coda verso destra).

        Un'asimmetria positiva significa che la distribuzione è
        caratterizzata da molti valori bassi e pochi alti che trascinano
        la coda a destra.

### *4. CREAZIONE DI CLASSI PER VARIABILE QUANTITATIVA*

Per svolgere questa analisi ho usato come variabile " sales " (variabile
quantitativa discreta).

-   **Costruzione delle Classi**

    1.  Ho calcolato l'ampiezza di ogni classe alla larghezza della
        variabile per determinare il range e i n seguito ho suddiviso il
        range in 10 intervalli di uguale ampiezza.

    2.  Calcolo Frequenza assoluta, relativa e cumulata.

    3.  Creazione del Grafico della distribuzione :

        Ho rappresentato le frequenze assolute con un grafico a barre,
        ruotando le etichette per migliorarne la leggibilità.

```{r}

library(dplyr)
library(knitr)

#Choosen Variable
var<- data$sales
#Classe Number (es. 10) and width
n_classi<-10
ampiezza <- ceiling((max(var) - min(var)) / n_classi)

#Create intervals
breaks<- seq(from = floor(min(var)),
             to = ceiling(max(var)+ampiezza),
             by= ampiezza
             ) 
#Classes Labels
etichette<- paste(head(breaks,-1), breaks[-1], sep = " - ")
#frequency Table
freq_table<- data.frame(
  class= cut(var, breaks = breaks, labels = etichette, right = FALSE),
  stringsAsFactors = FALSE
) %>%
  group_by(class) %>%
  summarise(
    Absolute_frequency = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    Relative_frequency = Absolute_frequency / sum(Absolute_frequency),
    Cumulative_frequency = cumsum(Relative_frequency)
  ) %>%
  mutate(across(c(Relative_frequency, Cumulative_frequency), ~ round(.x, 2)))
#Tabular output
kable(freq_table, caption= "Frequency distribution for the variable sales")

```

-   **Grafico della Distribuzione**

```{r}
library(ggplot2)
ggplot(freq_table, aes(x = class, y = Absolute_frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequency Distribution - Sales",
       x = "Sales Classes",
       y = "Absolute Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

-   **Indice di Gini**

Formula :

-   $G = 1-Σ fi^2$

    dove $fi$ sono le frequenze relative per classe.

    -   G = 0 -\> Distribuzione concentrata in poche classi (Bassa
        Eterogeneità)

    -   G = 1 -\> Distribuzione più uniforme (alta eterogeneità)

```{r}

#Absolute Frequency
ni<- freq_table$Absolute_frequency
#Relative Frequency
fi<- ni/length(data$sales)
fi2<- fi^2
j<- length(ni)

# Gini Index
gini <- 1 - sum(fi2)

# Normalized Gini Index
gini_norm <- gini / ((j - 1) / j)

# Tabular Output
gini_df <- data.frame(
  Indice_Gini = round(gini, 3),
  Indice_Gini_Normalizzato = round(gini_norm, 3)
)

kable(gini_df, caption = "Gini Index per variable 'sales'")
```

Dato che G = 0,86 , la distribuzione del sales è abbastanza eterogenea ,
quindi non tutte le vendite sono concentrate nella stessa classe.

#### Risultato :

-   G = 0,86 ( Gini normalizzata = 0,956) , la distribuzione del sales è
    abbastanza eterogenea , quindi non tutte le vendite sono concentrate
    nella stessa fascia e si distribuiscono in modo diversificato.

-   Dal grafico a barre si può osservare che la maggioranza delle sales
    cadono tra 114 e 184 unità, ma sono presenti anche classi con
    frequenze minori , che contribuiscono all'elevato valore di
    eterogeneità.

### 5. CALCOLO DELLE PROBABILITA'

**Richiesta** : *"Qual è la probabilità che, presa una riga a caso di
questo dataset, essa riporti la città “Beaumont”? E la probabilità che
riporti il mese di Luglio? E la probabilità che riporti il mese di
dicembre 2012?"*

Per risolvere tutte e 3 le richieste utilizzo il metodo di calcolo della
**Probabilità Classica** :

$P(evento) = n° Casi favorevoli/ n° Tot casi Possibili$

dove :

-   **casi favorevoli** = osservazioni che soddisfano la condizione
    dell'evento

-   **casi possibili** = numero totale di osservazioni nel dataset

#### **1. Probabilità che la città sia "Beaumont"**

```{r}
library(knitr)
n_tot <- nrow(data)

#Probability city = Beaumont
n_beaumont <- sum(data$city == "Beaumont")
p_beaumont <- n_beaumont / n_tot
p_beaumont 
```

-   P (città = " Beaumont " ) = 25%

#### **2. Probabilità che il mese sia "Luglio"**

```{r}
#Probability July month
n_july <- sum(data$month == 7)
p_july <- n_july / n_tot
p_july
```

-   P (mese = " Luglio" ) = 8,3%

#### **3. Probabilità che data sia "Dicembre 2012"**

```{r}
#Probability December 2012
n_dec_2012 <- sum(data$month == 12 & data$year == 2012)
p_dec_2012 <- n_dec_2012 / n_tot
p_dec_2012
```

-   P ( data = Dicembre 2012) = 1,67%

**➽ RESUME**

```{r}
# Resume Table
prob_df <- data.frame(
  Event = c("City = Beaumont", "Month = July", "Data = December 2012"),
  Favorable_cases = c(n_beaumont, n_july, n_dec_2012),
  Possible_cases = n_tot,
  Probability = round(c(p_beaumont, p_july, p_dec_2012), 4)
)

kable(prob_df, caption = "Probabilities calculated using the classical definition")
```

-   P(City = Beaumont) = 25% → indica la probabilità che un’osservazione
    scelta a caso appartenga alla città di Beaumont.

-   P(Month = July) = 8,3% → indica la probabilità che un’osservazione
    sia relativa al mese di luglio.

-   P(December 2012)= 1,67% → indica la probabilità che un’osservazione
    sia relativa a dicembre 2012.

### 6. CREAZIONE DI NUOVE VARIABILI

#### 1. Prezzo Medio degli Immobili

Per calcolare il prezzo medio degli immobili ho effettutato il rapporto
tra " `volume` " ( valore totale delle vendite , espressa in milioni ) e
" `sales` " (numero di immobili venduti)

```{r}
library(knitr)
#Average Real Estate Price
data$avg_price<- data$volume *10^6/data$sales
#Extracted var AVG Real Estate Price
kable(
  head(data[, c("city", "year", "month", "sales", "volume", "listings", "avg_price")]),
  caption = "AVG Price - extract dataset"
)
```

#### 2. Efficacia degli annunci di vendita

Per calcolare l'efficacia degli annunci di vendita , ho effettuato il
rapporto tra " `sales` " ( vendite effettive ) e " `listings` " ( numero
di annunci ). Questo nuovo indice permette di misurare l'efficacia degli
annunci di vendita.

```{r}
#Ads Efficency 
data$efficency<- data$sales/data$listings
data$efficency<-round(data$efficency,2)
#Extracted var Ads Efficency
kable(
  head(data[, c("city", "year", "month", "sales", "volume", "listings","avg_price", "efficency")]),
  caption = "AVG Price + Ads Efficency - extract dataset"
)
```

Il risultato sarà compreso tra 0 & 1 :

-   valori vicini a 1 -\> alta Efficacia : Quasi tutti gli annunci hanno
    generato una vendita

-   valori vicini a 0 -\> Bassa Efficacia : Molti annunci non si
    trasformano in vendita

#### Analisi Variabili :

Analizzo nel dettaglio come le 2 variabili si distribuiscono e
concentrano la maggior parte degli immobili per AVG Price & Ads
efficiency.

#### - AVG Price

```{r}
#Summary Table "AVG Price"
tabella_summary <- data.frame(
  Statistic = names(summary(data$avg_price)),
  Value = as.numeric(summary(data$avg_price))
)
kable(tabella_summary, caption = "Descriptive statistics of avg_price")

#Histogram x AVG_Price
library(ggplot2)
ggplot(data)+
  geom_histogram(aes(x= avg_price),binwidth = 10000 ,fill= "lightblue",color= "black")+
  geom_vline(xintercept = 132939, color= "red",lty="dashed", lwd=2)+
  geom_vline(xintercept = 173915, color= "red",lty= "dashed", lwd=2)+
  labs(title = "Distribution of Real Estate's AVG price", x="AVG Price [$]", y="Frequency")+
  theme_classic()
  

#Line Chart x Avg-Price
data$period <- as.Date(paste(data$year, data$month, "01", sep = "-"))
data$period <- as.Date(paste(data$year, data$month, "01", sep = "-"))
ggplot(data)+geom_line(aes(x = period, y = avg_price, color = city, group = city)) +
  geom_point(aes(x = period, y = avg_price, color = city, group = city),size = 1) +
  labs(title = "AVG Price for city in time",
       x = "Period",
       y = "AVG Price ($)") +
  scale_x_date(date_labels = "%Y-%m")+theme_minimal()+
  theme(legend.position = "bottom")

```

-   **Range** : I prezzi medi oscillano tra 97.000 \$ e i 213.000 \$.

-   **Quartili** : il 1° Quartile ( 132.939 \$) e il 3° quartile
    (173.915 \$) racchiudono il 50% centrale delle osservazioni.

-   **Mediana** : Pari a 156.588 \$ ) è molto vicina alla Media (
    154.320 \$ ) , suggerendo una distribuzione abbastanza simmetrica.

-   **Concentrazione** : La maggior parte degli immobili ha un prezzo
    medio di vendita tra 130.000 \$ - 175.000 \$ , con alcuni valori
    estremi ( 97.000\$ - 21000 \$ ).

-   **Trend Temporale** : dal 2010 al 2011, Il prezzo medio mantiene un
    andamento costante per tutte le città , mentre dal 2012 si denota
    una tendenza alla crescita, sopratutto per città come Bryan-Collage
    Station.

#### - Ads efficiency:

```{r}
data_efficiency_sort<- data[order(data$efficency),]
kable(
  head(data_efficiency_sort[, c("city", "year", "month", "sales", "volume", "listings", "avg_price", "efficency")]),
  caption = "Ads efficency sorted - dataset Extract"
)
#Summary Table "AVG Price"
tabella_summary <- data.frame(
  Statistic = names(summary(data$efficency)),
  Value = as.numeric(summary(data$efficency))
)
kable(tabella_summary, caption = "Descriptive statistics of ADS efficency")

#Line Chart x Avg-Price
ggplot(data)+geom_line(aes(x = period, y = efficency, color = city, group = city))+
  geom_point(aes(x = period, y = efficency, color = city, group = city),size = 1) +
  labs(title = "Ads efficency for city in time",
       x = "Period",
       y = "Ads efficency") +
  scale_x_date(date_labels = "%Y-%m")+theme_minimal()+
  theme(legend.position = "bottom")
```

-   **Range** : Il tasso di conversione tra annuncio pubblicitario e
    vendita immobile varia tra 5% e 39% -\> range non elevato , con
    qualche outlier verso l'alto che si stacca dalla maggior parte della
    distribuzione.

-   **Quartili** : il 1° Quartile ( 8,9 %) e il 3° quartile (13,49% )
    racchiudono il 50% centrale delle efficienze.

-   **Mediana** : Pari a 10,96 % è leggermente superiore alla Media (
    11,87 % ) e questo suggerisce una leggera asimmetria verso destra
    dovuto a pochi annunci molto efficienti (+ valori bassi nella
    distribuzione).

-   **Concentrazione** : La maggior parte degli annunci ha un tasso di
    conversione tra il 9% e il 13 % , con pochi casi oltre il 20%.

-   **Trend Temporale** : Dal 2010 - 2011 l'efficienza resta bassa (5 %
    -10%) ; dal 2012 si denota un trend crescente , con gran parte delle
    città che arriva tra il 10% e il 15%. Dal 2013 si registra un trend
    crescente raggiungendo picchi notevoli (Le città che raggiunge più
    rapidamente questi picchi è Bryan-College Station).

#### Commento Finale :

-   **AVG Price** : I prezzi medi degli immobili si attesta intorno ai
    150-170k \$ con un distribuzione abbastanza simmetrica/ equilibrata

-   **Ads Efficency** : il tasso di conversione pubblicità / vendita
    degli immobili si concentra sul 10 % con pochi casi che innalzano la
    media. Dal 2012 mostra un trend crescente nel tempo, suggerendo un
    miglioramento delle capacità di convertire gli annunci di vendite.

### 7. ANALISI CONDIZIONATA

*Usa il pacchetto `dplyr` o il linguaggio base di R per effettuare
analisi statistiche condizionate per città, anno e mese. Genera
dei **summary** (media, deviazione standard) e rappresenta graficamente
i risultati.*

Inizialmente generiamo dei summary (media e deviazione standard) per
effettuare analisi statistiche condizionate per città , anno, mese.

```{r}
library(dplyr)
#Mean & standard deviation for City & Year/Month 
summary_city_year<- data %>%
  group_by(city,year) %>%
  summarise(
    mean_sales = mean(sales, na.rm= TRUE),
    sd_sales = sd(sales, na.rm = TRUE),
    mean_volume = mean(volume, na.rm = TRUE),
    sd_volume = sd(volume, na.rm= TRUE),
    mean_listings = mean(listings, na.rm= TRUE),
    sd_listings= sd(listings, na.rm = TRUE),
    mean_avg_price = mean(avg_price, na.rm = TRUE),
    sd_avg_price = sd(avg_price, na.rm = TRUE),
    mean_efficency = mean(efficency, na.rm = TRUE ),
    sd_efficency = sd(efficency, na.rm = TRUE),
    .groups= "drop"
  )

kable(head(summary_city_year), caption = "Average and Standard Deviation per city and year")
```

Ora posso utilizzare questo summarise per le seguenti variabili :
`sales`, `volume`, `avg_price`, `listings`, `efficency` per costruire
dei grafici per visualizzare analisi statistiche condizionate per città
, anno , mese.

#### Visualizzazione Grafica

-   **Variables for Years / City**

```{r}

#Graphics per Year

library(scales)
library(ggplot2)

#Barplot x Sales
ggplot(summary_city_year, aes(x= year, y= mean_sales, fill=city))+
  geom_bar(stat= "identity", position= "dodge")+
  geom_errorbar(aes(ymin=mean_sales-sd_sales, ymax = mean_sales+sd_sales), position = position_dodge(width = 0.9))+geom_text(aes(label = round(mean_sales, 1)),position = position_dodge(width = 0.9), size=2.5, vjust= -0.4, hjust= 0.5) +
  labs(title= "Avg Sales Trend per city", x= "Year", y= "Avg Sales")+theme_gray()+
  theme(legend.position = "bottom")

#Barplot x Volume
ggplot(summary_city_year, aes(x= year, y= mean_volume, fill=city))+
  geom_bar(stat= "identity", position="dodge")+
  geom_errorbar(aes(ymin=mean_volume-sd_volume, ymax = mean_volume+sd_volume), position = position_dodge(width = 0.9))+
  geom_text(aes(label = round(mean_volume, 1)),position = position_dodge(width = 0.9), size=2.5, vjust= -0.4, hjust= 0.5) +
  labs(title= "Avg Volume Trend per city", x= "Year", y= "Avg Volume [$]")+theme_gray()+theme(legend.position = "bottom")

#Barplot x Listings
ggplot(summary_city_year, aes(x= year, y= mean_listings, fill=city))+
  geom_bar(stat= "identity", position="dodge")+
  geom_errorbar(aes(ymin=mean_listings- sd_listings, ymax = mean_listings+sd_listings), position = position_dodge(width = 0.9))+geom_text(aes(label = round(mean_listings, 1)),position = position_dodge(width = 0.9), size=2.5, vjust= -0.4, hjust= 0.5)+
  labs(title= "Listings Trend per city", x= "Year", y= "N° Listings")+theme_grey()+theme_gray()+theme(legend.position = "bottom")

#Barplot x AVG Price
ggplot(summary_city_year, aes(x= year, y= mean_avg_price, fill=city))+
  geom_bar(stat= "identity", position="dodge")+
  geom_errorbar(aes(ymin=mean_avg_price- sd_avg_price, ymax = mean_avg_price+sd_avg_price), position = position_dodge(width = 0.9))+ geom_text(aes(label = round(mean_avg_price, 1)),position = position_dodge(width = 0.9), size=2.5, vjust= -0.4, hjust= 0.5)+ labs(title= "Avg Price Trend per city", x= "Year", y= "Avg Price [$]")+theme_gray()+theme_gray()+theme(legend.position = "bottom")

#Barplot x Ads Efficency
ggplot(summary_city_year, aes(x= year, y= mean_efficency, fill=city))+
  geom_bar(stat= "identity", position= "dodge")+
  geom_errorbar(aes(ymin=mean_efficency- sd_efficency, ymax = mean_efficency+sd_efficency), position = position_dodge(width = 0.9))+ geom_text(aes(label = round(mean_efficency, 3)),position = position_dodge(width = 0.9), size=2.5, vjust= -0.4, hjust= 0.5)+ labs(title= "Ads Efficency Trend per city", x= " Year", y= "Ads Efficency")+theme_grey()+theme_gray()+theme(legend.position = "bottom")
```

-   **Variables for Months / City**

```{r}
library(dplyr)
#Add Var "Period" : Month +Year
summary_city_month <- data %>%
  group_by(city,year, month) %>%
  summarise(
    mean_sales = mean(sales, na.rm= TRUE),
    sd_sales = sd(sales, na.rm = TRUE),
    mean_volume = mean(volume, na.rm = TRUE),
    sd_volume = sd(volume, na.rm= TRUE),
    mean_listings = mean(listings, na.rm= TRUE),
    sd_listings= sd(listings, na.rm = TRUE),
    mean_avg_price = mean(avg_price, na.rm = TRUE),
    sd_avg_price = sd(avg_price, na.rm = TRUE),
    mean_efficency = mean(efficency, na.rm = TRUE ),
    sd_efficency = sd(efficency, na.rm = TRUE),
    .groups = "drop"
  )%>%
  mutate(period = as.Date(paste(year, month, "01", sep = "-")))

#Barplot x Sales
ggplot(summary_city_month, aes(x = period, y = mean_sales, color = city, group = city)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_sales - sd_sales, ymax = mean_sales + sd_sales), width = 10) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
  labs(title = "Avg Sales Trend per city (2010–2014)",
       x = "Period", y = "Avg Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#Barplot x Volume
ggplot(summary_city_month, aes(x = period, y = mean_volume, color = city, group = city)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_volume - sd_volume, ymax = mean_volume + sd_volume), width = 10) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
  labs(title = "Avg Volume Trend per city (2010–2014)",
       x = "Period", y = "Avg Volume [$]") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#Barplot x Listings
ggplot(summary_city_month, aes(x = period, y = mean_listings, color = city, group = city)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_listings - sd_listings, ymax = mean_listings + sd_listings), width = 10) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
  labs(title = "Listings Trend per city (2010–2014)",
       x = "Period", y = "N° Listings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#Barplot x AVG Price
ggplot(summary_city_month, aes(x = period, y = mean_avg_price, color = city, group = city)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_avg_price - sd_avg_price, ymax = mean_avg_price + sd_avg_price), width = 10) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
  labs(title = "Avg Price Trend per city (2010–2014)",
       x = "Period", y = "Avg Price [$]") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#Barplot x Ads Efficency
ggplot(summary_city_month, aes(x = period, y = mean_efficency, color = city, group = city)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_efficency - sd_efficency, ymax = mean_efficency + sd_efficency), width = 10) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
  labs(title = "Ads Efficency Trend per city (2010–2014)",
       x = "Period", y = "Ads Efficency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

```

#### Analisi Grafica per City :

-   **Beaumont** : Ha una crescita moderata in termini di vendite e
    ricavi dalle vendite, con picchi solo nel 2013 e nel 2014. Questo
    può essere dovuto a uno dei più bassi tassi di conversione
    annuncio - vendita degli immobili. Beaumont si dimostra tra le città
    peggiori in termini di vendite; un aumento di annunci pubblicitari
    porterebbe portare un drastico incremento del fatturato.

-   **Bryan-College Station** : si dimostra la città con la crescita
    migliore in termini di vendita, ricavi, prezzi medi. Per ora
    dimostra un volume di vendita e un fatturato peggiore rispetto a
    Tyler , probabilmente dovuti a bassi investimenti in annunci
    pubblicitari. Tuttavia dimostra sempre un alto tasso di conversione
    e ciò potrebbe suggerire un maggiore investimento in advertising per
    consentire una crescita sostanziale delle vendite.

-   **Tyler** : presenta un crescita solida in termini di vendite e
    ricavi, ma ciò è dovuto a maggior investimenti in annunci
    pubblicitari e un minor prezzo medio degli immobili venduti rispetto
    a Bryan-College Station. La sua efficienza pubblicitaria è buona ma
    non eccellente. Per Tyler è preferibile ottimizzare il rapporto tra
    pubblicità e prezzi medi contenuti per aumentare i ricavi.

-   **Wichita Falls** : mostra le performance più basse in termini di
    vendite e ricavi di vendita , nonostante ha tassi di conversione più
    alti rispetto ad altre città come Beaumont e Tyler. Quindi l'unico
    modo per risollevare le vendite in questa città sembra essere un
    aumento significativo degli investimenti pubblicitari.

**Sintesi :** Bryan-College Station mostra la crescita più marcata sia
nelle vendite che nei prezzi medi , con alta efficienza pubblicitaria.
Tyler mantiene vendite solide grazie a investimenti pubblicitari ma con
prezzi medi più bassi. Beaumont e Wichita Falls restano indietro: la
prima per bassi tassi di conversione, la seconda per volumi di vendita
ridotti nonostante una buona efficienza. In entrambi i casi, maggiori
investimenti in advertising potrebbero migliorare le performance.

### 8. CREAZIONE DI VISUALIZZAZIONI CON GGPLOT2

#### 1. Boxplot - Prezzo Mediano per Città

```{r}
#Boxplot Avg Price x city
ggplot(data, aes(x = factor(year), y = median_price, fill = city)) +
  geom_boxplot() +
  labs(title = "Median Price Boxplot per City and Year",
       x = "Year", y = "Median Price [$]") +
  theme_minimal() +
  theme(legend.position = "bottom")
```

**Insights :**

-   **Beaumont** : Mediana intermedia (= 130k \$) , posizionato nella
    fascia intermedia rispetto alle altre città. La variabilità è
    moderata, con outlier isolato che indica la presenza di pochi
    immobili di valore molto superiore alla media. Questo suggerisce un
    mercato tendenzialmente omogeneo, ma con qualche episodio di vendita
    di immobili di lusso che però non incide sulla distribuzione
    complessiva.

-   **Bryan-College Station** : Mediana più alta (= 157k \$) e una
    variabilità contenuta. La stabilità dei valori, unita alla
    collocazione nella fascia alta, riflette un mercato caratterizzato
    da domanda costante per immobili di qualità e da un’offerta che si
    mantiene su livelli di prezzo sostenuti. Potrebbe indicare un’area
    con forte attrattività residenziale e da acquirenti con un buon
    reddito.

-   **Tyler** : Mediana intermedia (= 141k \$) ma con un IQR più ampio
    rispetto a Beaumont e Bryan‑College Station. Questo segnala una
    maggiore dispersione dei prezzi, con immobili il cui prezzo medio
    spazia sia nella fascia media sia in quella più economica. Tale
    configurazione può riflettere un mercato eterogeneo, in cui
    coesistono quartieri con valori più accessibili e zone con immobili
    di livello superiore.

-   **Wichita Falls** : Mediana nettamente più bassa (102,7k \$) e IQR
    ridotto , a indicare un mercato con immobili a basso prezzo e valori
    fortemente concentrati attorno alla mediana. La scarsa variabilità
    suggerisce un’offerta piuttosto uniforme, con poche eccezioni verso
    l’alto, tipica di mercati meno dinamici.

#### 2. Grafici a barre - Vendite totali per Mese e Città

```{r}
#BarChart Sales x month & city
library(dplyr)

sales_monthly <- data %>%
  group_by(year, month, city) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE), .groups = "drop")

ggplot(sales_monthly, aes(x = factor(month), y = total_sales, fill = city)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year, ncol = 2) +
  labs(title = "Sales Trend per city and month",
       x = "Month", y = "Total Sales") +
  theme_minimal() +
  theme(legend.position = "bottom")

```

**Insights :**

-   **Andamento generale** : Le vendite mostrano una tendenza
    complessivamente stabile nella maggior parte delle città analizzate,
    con oscillazioni fisiologiche legate alla stagionalità. L’eccezione
    più evidente è Bryan‑College Station,che evidenzia una crescita
    progressiva e visibile nel tempo, segnale di un mercato in
    espansione e di una domanda in aumento.

-   **Pattern stagionale** : In tutte le città si osservano picchi
    ricorrenti nei mesi estivi, in particolare tra marzo e settembre,
    con massimi spesso concentrati in giugno‑luglio. Al contrario, i
    mesi invernali registrano un calo marcato delle vendite. Questo
    andamento conferma che la stagionalità è un fattore fondamentale per
    la pianificazione di strategie commerciali e di marketing, ad
    esempio concentrando campagne promozionali e attività di vendita nei
    periodi di maggiore domanda.

#### 3. Line Chart - Andamento storico vendite per Città

```{r}
data <- data %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

sales_monthly <- data %>%
  group_by(date, city) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE), .groups = "drop")

ggplot(sales_monthly, aes(x = date, y = total_sales, color = city)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(title = "Storic Sales Trend by City",
       x = "Period", y = "Total Sales") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "4 months") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
```

**Insights :**

Tramite il Line Chart si evidenzia l’evoluzione storica delle vendite
immobiliari nelle diverse città, mettendo in evidenza le tendenze di
lungo periodo.

-   **Tyler**: Si conferma come la città con i volumi di vendita più
    elevati in tutto l’arco temporale analizzato, mostrando una crescita
    costante e sostenuta dal 2010 al 2015.

-   **Bryan-Collage Station** : presenta un trend in crescita marcata
    soprattutto tra il 2012 e il 2014, segnale di un’espansione
    significativa della domanda in quegli anni. La curva suggerisce un
    mercato in consolidamento, capace di attrarre acquirenti in modo
    crescente.

-   **Beaumont** : mostra un andamento stabile senza variazioni
    rilevanti per gran parte del periodo, con una lieve crescita negli
    ultimi due anni. Questo comportamento può indicare un mercato
    maturo, con incrementi graduali nel tempo.

-   **Wichita Falls** è l’unica città a evidenziare un calo delle
    vendite negli ultimi due anni, dopo una fase iniziale più stabile.
    Questo potrebbe riflettere un rallentamento della domanda o altri
    fattori che hanno inciso negativamente sul mercato.

Nel complesso tutte le città , tranne Wichita Falls, mostrano un
crescita positiva nel periodo analizzato , sintomo di una fase di
espansione del mercato immobiliare, probabilmente sostenuta da una
domanda residenziale in aumento e da buone condizioni economiche.

### 9. CONCLUSIONI

#### 1. Andamento Generale del Mercato Texano :

Nel periodo 2010 - 2014, il mercato immobiliare presenta alcune
caratteristiche distintive :

-   **Crescita costante** : Si osserva una crescita costante sia in
    termini di vendite che di importo generato dalle vendite , seppur
    con dinamiche decisamente diverse da città a città.

-   **Stagionalità Marcata :** Le vendite di immobili si concentrano tra
    marzo e settembre, mentre nei mesi invernali si registra un
    rallentamento significativo.

-   **Segmentazione Netta** : Alcune città si attestano su prezzi
    elevati e stabili, altre privilegiavano i volumi , con prezzi medi
    più bassi ma un maggiore numero di vendite.

#### 2. Risultati per Città :

-   **Bryan-College Station**

    -   [Volumi di Vendita]{.underline} : Vendite e Ricavi crescono in
        modo rilevante soprattutto dal 2012 in poi.

    -   [Prezzo Medio degli Immobili]{.underline} : Prezzo medio è il
        più alto (175k \$) tra le città analizzate , confermando il
        posizionamento nel segmento dei immobili di fascia alta.

    -   [Tasso di conversione tra annunci e vendite]{.underline} : è tra
        i più alti tra le città analizzate con un picco del 23,6% nel

        2014. 

    In termini operativi , conviene incrementare l'investimento
    pubblicitario per sfruttare al massimo l'efficienza ed aumentare
    ulteriormente i volumi.

-   **Tyler**

    -   [Volumi di Vendita]{.underline} : Leader indiscusso in termini
        di vendite e ricavi in tutta la serie storica.

    -   [Prezzo Medio degli Immobili]{.underline} : Il prezzo medio si
        colloca intorno a 141k \$ ma con alta variabilità , segno di un
        mercato eterogeneo.

    -   [Tasso di conversione tra annunci e vendite]{.underline} :
        l'efficienza pubblicitaria è buona con una media del 10% , ma
        inferiore dei top performer.

    La strategia dovrebbe integrare campagne pubblicitarie mirate con
    una revisione del pricing, al fine di massimizzare i ricavi per
    unità venduta.

-   **Beaumont**

    -   [Volumi di Vendita]{.underline} : andamento stabile per vendite
        e ricavi , con alcuni picchi nel biennio 2013 - 2014.

    -   [Prezzo Medio degli Immobili]{.underline} : Il prezzo medio si
        colloca intorno a 131k \$ , con variabilità contenuta.

    -   [Tasso di conversione tra annunci e vendite]{.underline} :
        l'efficienza pubblicitaria è tra i più bassi.

    La priorità è aumentare il numero di annunci e ottimizzare le
    campagne per migliorare i tassi di conversione.

-   **Wichita Falls**

    -   [Volumi di Vendita]{.underline} : Vendite e Ricavi sono
        contenuti e mostrano una tendenza verso il basso.

    -   [Prezzo Medio degli Immobili]{.underline} : Prezzo medio basso
        (circa 102k \$) e una bassa variabilità, segno di un mercato
        omogeneo.

    -   [Tasso di conversione tra annunci e vendite]{.underline} :
        l'efficienza pubblicitaria, rapportata alla dimensione del
        mercato, è discreta.

    Un ampliamento dell'offerta e un potenziamento della visibilità
    potrebbero stimolare la domanda.

#### 3. Evidenze statistiche chiave

-   **Variabilità** : Il coefficiente di variazione è massimo per
    *volume* e *listings ,* indicando differenze sostanziali tra mercati
    per dimensione e valore delle vendite.

-   **Asimmetria** : Positiva su vendite e ricavi riflette la presenza
    di code destre, con pochi casi di valori molto elevati che
    influenzano la media.

-   **Distribuzione** : L'analisi tramite il boxplot conferma la netta
    separazione nei livelli di prezzo tra città; inoltre la
    concentrazione in specifiche fasce è evidente; l'indice di Gini
    evidenzia maggiore eterogeneità nei mercati segmentati.

#### 4. Raccomandazione Strategiche

Sul piano operativo, è fondamentale pianificare le campagne nei mesi di
maggiore attività (primavera-estate) per ottimizzare l'impatto.

**Strategie specifiche per area :**

-   *Bryan-College Station* : aumentare i volumi mantenendo elevato il
    prezzo d'acquisto.

-   *Tyler* : lavorare sull'aumento del prezzo medio senza perdere
    competitività.

-   *Beaumont* : migliorare l'efficienza pubblicitaria per valorizzare
    il potenziale di crescita.

-   *Wichita Falls* : ampliare offerta e visibilità per stimolare la
    domanda.

#### **Sintesi Finale**

Le analisi statistiche confermano che le dinamiche locali devono guidare
le strategie operative: un approccio uniforme al mercato texano
rischierebbe di non valorizzare le specificità di ciascuna area.
L’integrazione di analisi descrittive, indici di variabilità e
distribuzione, e osservazioni temporali ha permesso di delineare un
quadro chiaro delle opportunità e delle criticità per ogni città.
