# 💳 **Credit Scoring Prediction (Machine Learning Project)**

## 🚀 **Project Overview**
This project implements a Machine Learning system designed to estimate the credit reliability of customers based on demographic, financial, and employment information.

The goal is to:

- Explore and clean the dataset  
- Handle missing values and categorical variables  
- Build and compare multiple classification models  
- Apply pruning to improve interpretability  
- Select a final model capable of providing clear, explainable decisions  

The project was developed in **Python** using **Scikit‑Learn**.

📒 *To view the notebook in its original `.ipynb` format, open it in Google Colab.*

---

## 🧠 **Business / Analytical Objective**
Predict whether a customer is **credit‑reliable** (`TARGET = 1`) or **not reliable** (`TARGET = 0`).

This type of predictive system can support:

- Credit risk assessment  
- Customer profiling  
- Decision support for loan approval  
- Reduction of manual evaluation time  
- More consistent and data‑driven decisions  

---

## 🛠️ **Tech Stack**
- Python  
- Pandas, NumPy  
- Matplotlib, Seaborn  
- Scikit‑Learn  
- Google Colab  

---

## 🔎 **Project Workflow**

### **1️⃣ Exploratory Data Analysis (EDA)**
- Inspected dataset structure  
- Identified numerical and categorical variables  
- Checked missing values and duplicates  
- Converted `DAYS_BIRTH` into age  
- Handled outliers in `DAYS_EMPLOYED`  
- Visualized distributions (age, income, children, target)

### **2️⃣ Preprocessing**
- Split into train/test sets with stratification  
- Built pipelines for numerical and categorical features  
- Numerical: median imputation + standardization  
- Categorical: most frequent imputation + One‑Hot Encoding  
- Combined everything using a `ColumnTransformer`

### **3️⃣ Machine Learning Models**
Three models were trained and evaluated:

- **Logistic Regression** (baseline)  
- **Decision Tree**  
- **Random Forest**

Metrics used:

- Accuracy  
- ROC‑AUC  
- Classification Report  
- Confusion Matrix  

### **4️⃣ Pruning & Interpretability**
A pruned Decision Tree was created using:

- `max_depth=10`  
- `min_samples_leaf=5`  
- `class_weight="balanced"`

A second “super interpretable” tree was also built with:

- `max_depth=3`  
- `min_samples_leaf=20`  
- `min_samples_split=40`

This model is ideal for explaining decisions to non‑technical stakeholders.

---

## 📊 **Results**

### **Model Performance Summary**

| Model | Train Accuracy | Test Accuracy | Overfitting | Interpretability |
|-------|---------------:|--------------:|-------------|------------------|
| Logistic Regression | 0.8238 | 0.8266 | Low | Medium |
| Decision Tree | 1.0000 | 0.9411 | High | High |
| Decision Tree (Pruned) | 0.9490 | 0.9480 | Low | High |
| Random Forest (20 trees) | 0.9490 | 0.9500 | Very Low | Medium |

### ⭐ **Final Model Selected**
**Decision Tree (Pruned)**  
Chosen because it provides:

- strong performance  
- reduced overfitting  
- clear and explainable decision rules  

---

## 📁 **Repository Structure**

```
📁 credit-scoring-ml
├── README.md
├── Credit_Scoring_ML.ipynb
├── src/
│   └── credit_scoring.py        (optional)
├── data/
│   └── credit_scoring.csv       (optional)
└── images/
    ├── decision_tree_pruned.png
    ├── confusion_matrix.png
    └── feature_importance.png   (optional)
```

---

## 💡 **Skills Demonstrated**
✔ Exploratory Data Analysis  
✔ Feature Engineering  
✔ Handling missing values  
✔ Categorical encoding  
✔ Standardization  
✔ Decision Tree modeling  
✔ Pruning and interpretability  
✔ Random Forest comparison  
✔ Model evaluation  
✔ Clean and structured ML workflow  

---

## ⚙️ **How to Run**
1. Open the `.ipynb` notebook in Google Colab  
2. Upload the dataset if needed  
3. Run all cells sequentially  

---

## 📌 **Final Notes**
This project demonstrates how Machine Learning can support credit‑risk evaluation by providing consistent, explainable predictions.  
The pruned Decision Tree offers a good balance between performance and interpretability, making it suitable for real‑world decision support systems.
