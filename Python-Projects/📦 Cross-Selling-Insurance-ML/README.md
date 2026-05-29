
# 📚 **Insurance Cross-Selling Prediction (Machine Learning Project)**

## 🚀 **Project Overview**
This project implements a predictive Machine Learning system designed to identify customers who are likely to accept a cross‑sell insurance offer.

The goal is to:

- Analyze customer characteristics and behavioral patterns  
- Handle class imbalance in the target variable  
- Build and compare multiple Logistic Regression–based models  
- Evaluate performance using standard classification metrics  

The project was developed in **Python** using **Scikit‑Learn** and **Imbalanced‑Learn**.

---

📒 To read the notebook in its original `.ipynb` format, open it in Google Colab :
[Cross_Selling_ML.ipynb](https://colab.research.google.com/drive/1zoaLAINw9iL3S9anP8gggjSUE56zz4yu?usp=sharing) *

---

## 🧠 **Business / Analytical Objective**
Predict whether an existing health‑insurance customer is likely to purchase an additional **vehicle insurance policy**.

This type of predictive system can support:

- Targeted marketing campaigns  
- Customer segmentation  
- Lead scoring  
- Cost reduction in outreach strategies  
- Increased conversion rates  

---

## 🛠️ **Tech Stack**
- Python  
- Pandas, NumPy  
- Matplotlib, Seaborn  
- Scikit‑Learn  
- Imbalanced‑Learn  
- Google Colab  

---

## 🔎 **Project Workflow**

### **1️⃣ Data Ingestion & Cleaning**
- Loaded dataset from remote source  
- Removed unused fields (e.g., customer ID)  
- Converted categorical variables into numerical format  
- Checked for missing values and corrected inconsistent entries  
- Standardized numerical features  

---

### **2️⃣ Exploratory Data Analysis (EDA)**
Performed a detailed analysis to understand customer behavior:

- Distribution of the target variable **Response** (strong class imbalance)  
- Correlation matrix to identify key predictive features  
- Boxplots and histograms to explore relationships between:  
  - Age  
  - Policy Sales Channel  
  - Vehicle Damage  
  - Previously Insured  
- Identification of variables with strong predictive power (e.g., Vehicle Damage, Previously Insured)

This phase highlighted important behavioral patterns relevant for cross‑selling.

---

### **3️⃣ Handling Class Imbalance**
Since the target variable is highly imbalanced, the following techniques were applied:

- **class_weight='balanced'**  
- **Random Oversampling**  
- **Random Undersampling**

Oversampling and undersampling were applied **only to the training set** to avoid data leakage.

---

### **4️⃣ Machine Learning Model**
Built multiple Logistic Regression models:

- Logistic Regression (baseline)  
- Logistic Regression + Oversampling  
- Logistic Regression + Undersampling  

Dataset split:

- 75% Training  
- 25% Test  

Evaluation metrics:

- Accuracy  
- Precision  
- Recall  
- AUC  
- Confusion Matrix  
- ROC Curve  

---

## 📊 **Results**
The models achieved:

- **AUC ≈ 0.82**  
- **Recall ≈ 0.98** for the positive class  
- Similar performance across all balancing strategies  
- Oversampling/undersampling improved training metrics but **not** test performance  

The most stable and interpretable model was:

### ⭐ **Logistic Regression with `class_weight='balanced'`**

---

## 📁 **Repository Structure**
```
📁 insurance-cross-selling-ml
├── README.md
├── Cross_Selling_ML.py 
├── data/
│   └── insurance_cross_sell.csv   (optional)
└── images/
    ├── roc_curve.png
    ├── confusion_matrix.png
    └── correlation_heatmap.png
```

---

## 💡 **Skills Demonstrated**
✔ Exploratory Data Analysis  
✔ Feature Engineering  
✔ Handling class imbalance  
✔ Logistic Regression modeling  
✔ Model evaluation & interpretation  
✔ Data visualization  
✔ Clean and structured ML workflow  

---

## ⚙️ **How to Run**
1. Open the `.py` file in Google Colab  
2. Install required libraries if needed  
3. Run all cells sequentially  

*📒 To read the notebook in its original `.ipynb` format, open it in Google Colab :
[Cross_Selling_ML.ipynb](https://colab.research.google.com/drive/1zoaLAINw9iL3S9anP8gggjSUE56zz4yu?usp=sharing) 

---

## 📌 **Final Notes**
This project demonstrates how Machine Learning can support **data‑driven marketing strategies** by identifying customers with high cross‑sell potential.

It represents a practical application of classification techniques and class‑imbalance handling in a real‑world business scenario.

