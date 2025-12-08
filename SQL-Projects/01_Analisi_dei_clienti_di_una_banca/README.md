# 🏦 Analisi dei Clienti di una Banca – Feature Engineering con SQL
📚 Progetto del Master in Data Science, Machine Learning & AI (ProfessionAI)

## 📌 Descrizione
Questo progetto ha l’obiettivo di costruire una **feature table denormalizzata** per l’addestramento di modelli di Machine Learning supervisionato.  
L’analisi è stata svolta su un database bancario contenente informazioni su **clienti**, **conti** e **transazioni**, utilizzando tecniche SQL di aggregazione, join e creazione di KPI.

## 📂 Contenuto della cartella
- `Progetto_Finale_SQL.sql` → file principale con la soluzione completa.  
- `db_bancario.sql` → database fornito per il progetto.  
- `README.md` → documento descrittivo del progetto.

## 🎯 Obiettivi del progetto
- Calcolare feature anagrafiche, contabili e transazionali per ogni cliente.
- Realizzare una tabella finale utile per modelli ML come:
  - 🔁 Churn Prediction  
  - 💳 Propensione all’acquisto  
  - 🛡️ Valutazione del rischio  
  - 🚨 Rilevazione anomalie  
- Sviluppare **due versioni distinte** della query:
  1. 🥇 *SELECT unica* con join e aggregazioni dirette  
  2. 🧪 *Temporary Tables* per maggiore modularità

## 📊 Indicatori calcolati

### 👤 Indicatori demografici
- Età del cliente

### 💸 Indicatori sulle transazioni
- Numero transazioni in entrata / uscita  
- Importo totale in entrata / uscita  

### 🏦 Indicatori sui conti
- Numero totale di conti posseduti  
- Numero di conti per ciascuna tipologia (`tipo 0`, `1`, `2`, `3`)

### 🔍 Indicatori transazionali per tipologia di conto
Per ogni tipo di conto (0–3):
- Transazioni IN  
- Transazioni OUT  
- Importi IN  
- Importi OUT  

## 📈 Principali evidenze
- Il database permette di generare **feature granulari** utili a diversi modelli predittivi.  
- La tabella finale integra **dati demografici, operativi e comportamentali**.  
- Le due versioni (SELECT unica e Temporary Tables) generano risultati **identici**.  
- La soluzione è ottimizzata per **leggibilità, modularità e performance**.

## 🛠️ Tecnologie utilizzate
- SQL (JOIN, funzioni su date, aggregazioni, CASE WHEN, temporary tables)  
- GitHub per documentazione e versionamento  

## ✅ Conclusioni
Il progetto dimostra la capacità di:
- lavorare su un database relazionale complesso,  
- sviluppare una pipeline completa di feature engineering in SQL,  
- progettare KPI utili a modelli di Machine Learning,  
- produrre codice chiaro, commentato e performante.

Questo lavoro rappresenta una solida base per i moduli successivi del Master:
- 🔹 Big Data (Spark / PySpark)  
- 🔹 Modelli di classificazione  
- 🔹 Data Engineering  
- 🔹 Feature Engineering avanzato  
