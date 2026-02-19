
# 📚 Wikipedia Text Classification with PySpark

## 🚀 Project Overview

This project implements a **scalable text classification system** using **Apache Spark (PySpark)** to analyze and categorize Wikipedia articles.

The goal is to:

* Perform large-scale text preprocessing
* Explore linguistic patterns across categories
* Build a Machine Learning pipeline for automatic classification
* Evaluate model performance using standard metrics

The project was developed in Google Colab using PySpark.

---

## 🧠 Business / Analytical Objective

Automatically classify Wikipedia articles into predefined categories based on textual content.

This type of system can be applied to:

* Content organization
* Knowledge base structuring
* Automatic tagging systems
* Text analytics pipelines

---

## 🛠️ Tech Stack

* **Python**
* **PySpark**
* **Spark MLlib**
* **NLP techniques (Tokenization, TF-IDF)**
* **Matplotlib**
* **WordCloud**
* **Google Colab**

---

## 🔎 Project Workflow

### 1️⃣ Data Ingestion & Cleaning

* Loaded multi-line textual dataset from remote source
* Managed escaped characters and structured text fields
* Removed null or empty records
* Combined summary and document body into a unified `text` column
* Cached dataset for performance optimization

---

### 2️⃣ Exploratory Data Analysis (EDA)

Performed descriptive analysis to understand dataset structure:

* Category distribution (balanced dataset)
* Text length analysis (characters and word count)
* Vocabulary size per category
* Most frequent terms after stopword removal
* WordCloud visualization per category

This phase provided insight into linguistic differences across categories.

---

### 3️⃣ Feature Engineering

Implemented NLP preprocessing pipeline:

* `RegexTokenizer`
* `StopWordsRemover`
* `CountVectorizer`
* `IDF (TF-IDF weighting)`

The use of TF-IDF reduces the impact of highly frequent generic words and improves model discrimination.

---

### 4️⃣ Machine Learning Model

Built a multi-class classification pipeline using:

* **Logistic Regression (Spark ML)**

Dataset split:

* 80% Training
* 20% Test

Evaluation metrics:

* Accuracy
* Precision
* Recall
* F1-score
* Confusion Matrix

---

## 📊 Results

The model achieved:

* **Accuracy ≈ 93%**
* Balanced Precision and Recall
* Limited misclassification between linguistically similar categories

The results demonstrate that TF-IDF combined with Logistic Regression performs effectively on structured textual datasets.

---

## 📁 Repository Structure

```
📁 wikipedia-text-classification-pyspark
├── README.md
└── Progetto_Big_Data_Analisi_di_Wikipedia.ipynb
```

---

## 💡 Skills Demonstrated

✔ Big Data processing with Spark
✔ NLP preprocessing techniques
✔ Feature engineering (TF-IDF)
✔ ML pipeline construction
✔ Multi-class classification
✔ Model evaluation and interpretation
✔ Data cleaning and optimization

---

## ⚙️ How to Run

1. Open the `.ipynb` file in Google Colab
2. Install PySpark if needed
3. Run cells sequentially

---

## 📌 Final Notes

This project showcases the application of distributed data processing and machine learning techniques for large-scale text analytics.

It represents a practical step toward scalable NLP systems using Spark.

