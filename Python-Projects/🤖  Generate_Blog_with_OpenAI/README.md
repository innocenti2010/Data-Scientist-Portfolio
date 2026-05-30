# 🧠 Generative AI Blog Generator  
### NLP Application using OpenAI API (Python CLI)

---

## 📌 Project Overview

This project implements a Generative AI-based blog paragraph generator using Python and the OpenAI API.

The system takes a user-defined topic as input and generates a structured paragraph using a GPT-based Large Language Model (LLM).  
The application runs via Command Line Interface (CLI) and supports iterative prompt-based interaction.

---

## 🎯 Project Purpose

This project was developed as a learning exercise to understand how:

- AI models can be integrated into Python applications  
- Prompt-based text generation works  
- API keys can be managed securely  
- Interactive CLI applications can be built  

---

## 📂 Repository Structure

├── blog_generator.py </br>
├── README.md </br>
├── requirements.txt </br>
├── .env.example </br>

---

## 🔐 Environment Setup

Before running the application, create a `.env` file in the root directory of the project and insert your OpenAI API Key:

API_KEY=your_openai_api_key_here

You can refer to the `.env.example` file included in the repository.

---

## ▶️ Installation & Usage

### 1️⃣ Clone the repository


git clone https://github.com/yourusername/Data-Scientist-Portfolio.git </br>
cd Data-Scientist-Portfolio/Python-Projects/02-Generate_Blog_with_OpenAI

---

### 2️⃣ Install the required dependencies

pip install -r requirements.txt

---

### 3️⃣ Run the application

python blog_generator.py


---

## ⚙️ How It Works

1. The user is asked whether they want to generate a paragraph  
2. If yes, the user inputs a topic  
3. The topic is sent as a prompt to the OpenAI model  
4. The model generates a paragraph related to the topic  
5. The generated text is displayed in the terminal  
6. The process can be repeated as many times as desired  

---

## 📚 Learning Source

Project developed as part of the [Codedex AI Python learning path](https://www.codedex.io/projects/generate-a-blog-with-openai).

