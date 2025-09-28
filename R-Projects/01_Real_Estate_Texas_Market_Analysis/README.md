# **Real Estate Market Analysis – Texas (2010–2014)**

## 📌 **Descrizione**
Questo progetto fa parte del percorso del Master in Data Science & AI e ha l’obiettivo di analizzare il mercato immobiliare texano nel periodo **2010–2014** attraverso tecniche di statistica descrittiva e visualizzazione dati in R.

---

## 📂 **Contenuto della cartella**
- **Descriptive Statistic I Project. V.1.Rmd** → file sorgente RMarkdown con codice, analisi e commenti.  
- **Descriptive Statistic I Project. V.1.html** → (opzionale) versione knit del progetto, pronta per la consultazione.  
- **realestate_texas.csv** → dataset utilizzato per l’analisi.  

---

## 🎯 **Obiettivi del progetto**
- Esplorare e descrivere le variabili del dataset.  
- Calcolare indici di posizione, variabilità e forma.  
- Identificare le variabili con maggiore variabilità e asimmetria.  
- Creare distribuzioni di frequenza e calcolare l’indice di Gini.  
- Stimare probabilità di eventi specifici.  
- Creare nuove variabili derivate (*avg_price*, *ads_efficiency*).  
- Effettuare analisi condizionate per città, anno e mese.  
- Visualizzare i dati con **ggplot2** (boxplot, barplot, line chart).  
- Trarre conclusioni operative e strategiche per ciascuna città.

---

## 📊 **Principali evidenze**
- **Stagionalità marcata**: picchi di vendite tra marzo e settembre.  
- **Segmentazione netta**: Bryan‑College Station si posiziona nella fascia alta, Tyler domina per volumi, Beaumont è stabile ma poco efficiente, Wichita Falls mostra calo e prezzi bassi.  
- **Variabilità**: massima per *volume* e *listings*.  
- **Asimmetria positiva**: vendite e ricavi influenzati da pochi valori molto elevati.  
- **Indice di Gini (sales)**: 0,86 → distribuzione eterogenea.  
- **Nuove variabili**:  
  - *avg_price* ~150–170k $, distribuzione simmetrica.  
  - *ads_efficiency* ~10%, in crescita dal 2012.  

---

## 🛠️ **Tecnologie utilizzate**
- **R** (dplyr, ggplot2, knitr, moments)  
- **RMarkdown** per la documentazione e l’esecuzione del codice  
- **GitHub** per la condivisione del progetto  

---

## 🌐 **Versione online**
Il progetto è disponibile anche su **RPubs** al seguente link:  
👉 [Visualizza su RPubs](https://rpubs.com/Francesco2311/1346538)

---

## 📌 **Conclusioni**
Le analisi confermano che le **dinamiche locali** devono guidare le strategie operative:  
- **Bryan‑College Station** → aumentare i volumi mantenendo prezzi alti.  
- **Tyler** → ottimizzare il pricing per aumentare i ricavi unitari.  
- **Beaumont** → migliorare l’efficienza pubblicitaria.  
- **Wichita Falls** → ampliare offerta e visibilità per stimolare la domanda.  

