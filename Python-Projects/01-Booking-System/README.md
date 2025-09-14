🌐 **Multi-AI agent Travel agency** ✈️ <br>
Implementa un sistema multi-agent che simuli un'agenzia di viaggi. Il cliente può specificare la meta, il numero di notti, il budget totale, preferenze sulla tipologia di hotel (es. 4 stelle) e infine preferenze sulle attività da fare nella città di destinazione (visite ai musei, escursioni nella natura...). <br>
L'agenzia di viaggi è composta da due operatori: un HotelExpertAgent, specializzato nella ricerca e selezione dell'hotel più adatto alle esigenze dell'utente, e un TravelExperienceAgent, specializzato nel consigliare all'utente le attività da svolgere durante il suo soggiorno. Per semplicità le informazioni sugli hotel e quelle sulle escursioni sono contenute in due file separati Hotel.md e Experiences.md, accessibili rispettivamente dall'HotelExpertAgent e dall'TravelExperienceAgent

Puoi utilizzare un framework a scelta tra LangGraph, CrewAI e AutoGen.


📁 Folder Structure
-----

📁 01-Booking-System <br>
├── README.md <br>                
├── data/ <br>                 
│   ├── Hotel.md <br>
│   └── Experiences.md <br>
├── notebook/ <br>                
│   └── Booking_System.ipynb <br>
