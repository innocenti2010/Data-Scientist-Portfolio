-- 1. CREAZIONE TABELLE BASE NEL DATABASE 

/* Devo creare le tabelle che andranno a popolare il database : 
- Cliente = contiene informazioni personali sui clienti (id_cliente , nome , cognome, età).  
- Conto = contiene informazioni sui conti posseduti dai clienti.
- Tipo_conto = descrive le diverse tipologie di conti disponibili.
- Tipo_transazioni = contiene i tipi di transazione che possono avvenire sui conti.
- Transazioni = contiene i dettagli delle transazioni effettuate dai clienti sui vari conti.
*/

-- Tabella Clienti 
CREATE TABLE cliente (
id_cliente INTEGER,
  nome TEXT,
  cognome TEXT,
  data_nascita date
);

-- tabella Conto 
CREATE TABLE conto(
id_conto INTEGER,
id_cliente integer,
id_tipo_conto integer
);

-- Tabella Tipo_Conto
CREATE TABLE tipo_conto (
id_tipo_conto INTEGER,
  desc_tipo_conto TEXT
);

-- Tabella Tipo_Transazione
CREATE TABLE tipo_transazione (
id_tipo_transazione INTEGER,
  desc_tipo_trans TEXT,
  segno TEXT
);

-- Tabella Transazioni
CREATE TABLE transazioni (
data date,
  id_tipo_trans INTEGER,
  importo REAL,
  id_conto INTEGER
);

SELECT * FROM cliente; 
SELECT * FROM conto; 
SELECT * FROM tipo_transazione;
SELECT * FROM transazioni; 

-- 2. JOIN DELLE TABELLE 
/* 
Per costruire la tabella finale, sarà necessario eseguire una serie di join tra le tabelle disponibili nel database.
*/

#Per unire le varie tabelle tra loro uso la INNER JOIN e uso le colonne KEY per effettuare collegamenti tra le varie Tab.
SELECT * FROM transazioni tr
INNER JOIN tipo_transazione tp
	ON tp.id_tipo_transazione = tr.id_tipo_trans
INNER JOIN conto co 
	ON co.id_conto = tr.id_conto
INNER JOIN cliente cl
	ON cl.id_cliente= co.id_cliente;

-- 3. INDICATORI DI BASE 

# Calcolo come KPI di base = Età del cliente, facendo :
-- 1. Differenza di date tra la data attuale e la loro data di nascita (ottengo la loro età in giorni), usando funzione DATEDIFF;
-- 2. Divido la loro età per 365 (il numero di giorni in 1 anno);
-- 3. Ottengo solo la parte intera, usando la funzione FLOOR;

SELECT id_cliente, nome,cognome, 
floor(datediff(current_date(),data_nascita) /365) as eta
FROM banca.cliente; 

-- 4. INDICATORI SULLE TRANSAZIONI
-- 4.1 Numero di transazioni in uscita su tutti i conti 
	# Bisogna ricordarsi che gli indicatori sono calcolati per cliente, quindi non bisogna dimenticare di fare la JOIN con al tabella "cliente" 

SELECT cl.id_cliente, cl.nome, cl.cognome, 
count(tr.id_conto) AS num_transazioni_out FROM banca.transazioni tr 
INNER JOIN banca.tipo_transazione tp 
	ON tr.id_tipo_trans = tp.id_tipo_transazione
INNER JOIN banca.conto co
	ON tr.id_conto = co.id_conto
INNER JOIN banca.cliente cl
	ON cl.id_cliente = co.id_cliente
WHERE tp.segno= "-"
GROUP BY 1,2,3;

-- 4.2 Numero di transazioni in entrata su tutti i conti 
SELECT cl.id_cliente, cl.nome, cl.cognome, 
count(tr.id_conto) AS num_transazioni_in FROM banca.transazioni tr 
INNER JOIN banca.tipo_transazione tp 
	ON tr.id_tipo_trans = tp.id_tipo_transazione
INNER JOIN banca.conto co
	ON tr.id_conto = co.id_conto
INNER JOIN banca.cliente cl
	ON cl.id_cliente = co.id_cliente
WHERE tp.segno= "+"
GROUP BY  1,2,3;

-- 4.3 Importo totale transato in uscita su tutti i conti.
SELECT cl.id_cliente, cl.nome, cl.cognome, 
sum(tr.importo) AS importo_transazioni_out FROM banca.transazioni tr 
INNER JOIN banca.tipo_transazione tp 
	ON tr.id_tipo_trans = tp.id_tipo_transazione
INNER JOIN banca.conto co
	ON tr.id_conto = co.id_conto
INNER JOIN banca.cliente cl
	ON cl.id_cliente = co.id_cliente
WHERE tp.segno= "-"
GROUP BY 1,2,3;

-- 4.4 Importo totale transato in entrata su tutti i conti.
SELECT cl.id_cliente, cl.nome, cl.cognome, 
sum(tr.importo) AS importo_transazioni_in FROM banca.transazioni tr 
INNER JOIN banca.tipo_transazione tp 
	ON tr.id_tipo_trans = tp.id_tipo_transazione
INNER JOIN banca.conto co
	ON tr.id_conto = co.id_conto
INNER JOIN banca.cliente cl
	ON cl.id_cliente = co.id_cliente
WHERE tp.segno= "+"
GROUP BY 1,2,3;

#Inserisco tutti gli indicatori generati all'interno di un'unica tabella come "INDICATORI SULLE TRANSAZIONI" 

SELECT cl.id_cliente, cl.nome, cl.cognome, 
count(CASE WHEN tp.segno= "-" THEN tr.id_conto END) AS num_transazioni_out,
sum(CASE WHEN tp.segno= "-" THEN tr.importo else 0 END) AS importo_transazioni_out,
count(CASE WHEN tp.segno= "+" THEN tr.id_conto END) AS num_transazioni_in,
sum(CASE WHEN tp.segno= "+" THEN tr.importo ELSE 0 END) AS importo_transazioni_in
FROM banca.transazioni tr
INNER JOIN banca.tipo_transazione tp 
	ON tr.id_tipo_trans = tp.id_tipo_transazione
INNER JOIN banca.conto co
	ON tr.id_conto = co.id_conto
INNER JOIN banca.cliente cl
	ON cl.id_cliente = co.id_cliente
GROUP BY 1,2,3;

-- 5. INDICATORI SUI CONTI 
#Gli indicatori sarann sempre calcolati per cliente (id_cliente) 
-- 5.1 Numero Totale di conti posseduti 
SELECT cl.id_cliente, cl.nome, cl.cognome, 
count(co.id_conto) AS num_conti FROM banca.cliente cl
INNER JOIN banca.conto co
	ON co.id_cliente = cl.id_cliente
GROUP BY 1,2,3;

-- 5.2 di conti posseduti per tipologia
SELECT cl.id_cliente, cl.nome, cl.cognome, 
count(DISTINCT CASE WHEN co.id_tipo_conto = 0 THEN co.id_conto END) AS count_conto_0,
count(DISTINCT CASE WHEN co.id_tipo_conto = 1 THEN co.id_conto END) AS count_conto_1,
count(DISTINCT CASE WHEN co.id_tipo_conto = 2 THEN co.id_conto END) AS count_conto_2,
count(DISTINCT CASE WHEN co.id_tipo_conto = 3 THEN co.id_conto END) AS count_conto_3
FROM banca.conto co
INNER JOIN banca.cliente cl
ON cl.id_cliente= co.id_cliente
GROUP BY 1,2,3; 


-- 6. INDICATORI SULLE TRANSAZIONI PER TIPOLOGIA DI CONTO
-- 6.1 Numero di transazioni in uscita per tipologia di conto
SELECT cl.id_cliente, cl.nome, cl.cognome, 
count(CASE WHEN co.id_tipo_conto = 0 THEN tr.id_conto END) AS count_conto_0,
count(CASE WHEN co.id_tipo_conto = 1 THEN tr.id_conto END) AS count_conto_1,
count(CASE WHEN co.id_tipo_conto = 2 THEN tr.id_conto END) AS count_conto_2,
count(CASE WHEN co.id_tipo_conto = 3 THEN tr.id_conto END) AS count_conto_3
FROM banca.conto co
INNER JOIN banca.cliente cl
	ON cl.id_cliente= co.id_cliente
INNER JOIN banca.transazioni tr
	ON tr.id_conto = co.id_conto
INNER JOIN banca.tipo_transazione tp
	ON tp.id_tipo_transazione = tr.id_tipo_trans
WHERE tp.segno= "-"
GROUP BY 1,2,3;

-- 6.2 Numero di transazioni in entrata per tipologia di conto
SELECT cl.id_cliente, cl.nome, cl.cognome, 
count(CASE WHEN co.id_tipo_conto = 0 THEN tr.id_conto END) AS count_conto_0,
count(CASE WHEN co.id_tipo_conto = 1 THEN tr.id_conto end) AS count_conto_1,
count(CASE WHEN co.id_tipo_conto = 2 THEN tr.id_conto end) AS count_conto_2,
count(CASE WHEN co.id_tipo_conto = 3 THEN tr.id_conto end) AS count_conto_3
FROM banca.conto co
INNER JOIN banca.cliente cl
	ON cl.id_cliente= co.id_cliente
INNER JOIN banca.transazioni tr
	ON tr.id_conto = co.id_conto
INNER JOIN banca.tipo_transazione tp
	ON tp.id_tipo_transazione = tr.id_tipo_trans
WHERE tp.segno= "+"
GROUP BY 1,2,3;

-- 6.3 Importo transato in uscita per tipologia di conto
SELECT cl.id_cliente, cl.nome, cl.cognome, 
sum(CASE WHEN co.id_tipo_conto = 0 THEN tr.importo ELSE 0 END) AS importo_conto_0,
sum(CASE WHEN co.id_tipo_conto = 1 THEN tr.importo ELSE 0 END) AS importo_conto_1,
sum(CASE WHEN co.id_tipo_conto = 2 THEN tr.importo ELSE 0 END) AS importo_conto_2,
sum(CASE WHEN co.id_tipo_conto = 3 THEN tr.importo ELSE 0 END) AS importo_conto_3
FROM banca.conto co
INNER JOIN banca.cliente cl
	ON cl.id_cliente= co.id_cliente
INNER JOIN banca.transazioni tr
	ON tr.id_conto = co.id_conto
INNER JOIN banca.tipo_transazione tp
	ON tp.id_tipo_transazione = tr.id_tipo_trans
WHERE tp.segno= "-"
GROUP BY 1,2,3;

# Se voglio l'importo complessivo in uscita e non per singolo utente
select 
sum(CASE WHEN co.id_tipo_conto = 0 THEN tr.importo ELSE 0 END) AS importo_conto_0,
sum(CASE WHEN co.id_tipo_conto = 1 THEN tr.importo ELSE 0 END) AS importo_conto_1,
sum(CASE WHEN co.id_tipo_conto = 2 THEN tr.importo ELSE 0 END) AS importo_conto_2,
sum(CASE WHEN co.id_tipo_conto = 3 THEN tr.importo ELSE 0 END) AS importo_conto_3
FROM banca.conto co
INNER JOIN banca.cliente cl
	ON cl.id_cliente= co.id_cliente
INNER JOIN banca.transazioni tr
	ON tr.id_conto = co.id_conto
INNER JOIN banca.tipo_transazione tp
	ON tp.id_tipo_transazione = tr.id_tipo_trans
WHERE tp.segno= "-";

-- 6.3 Importo transato in entrata per tipologia di conto
SELECT cl.id_cliente, cl.nome, cl.cognome, 
sum(CASE WHEN co.id_tipo_conto = 0 THEN tr.importo ELSE 0 END) AS importo_conto_0,
sum(CASE WHEN co.id_tipo_conto = 1 THEN tr.importo ELSE 0 END) AS importo_conto_1,
sum(CASE WHEN co.id_tipo_conto = 2 THEN tr.importo ELSE 0 END) AS importo_conto_2,
sum(CASE WHEN co.id_tipo_conto = 3 THEN tr.importo ELSE 0 END) AS importo_conto_3
FROM banca.conto co
INNER JOIN banca.cliente cl
	ON cl.id_cliente= co.id_cliente
INNER JOIN banca.transazioni tr
	ON tr.id_conto = co.id_conto
INNER JOIN banca.tipo_transazione tp
	ON tp.id_tipo_transazione = tr.id_tipo_trans
WHERE tp.segno= "+"
GROUP BY 1,2,3;

# Se voglio l'importo complessivo in entrata e non per singolo utente
select 
sum(CASE WHEN co.id_tipo_conto = 0 THEN tr.importo ELSE 0 END) AS importo_conto_0,
sum(CASE WHEN co.id_tipo_conto = 1 THEN tr.importo ELSE 0 END) AS importo_conto_1,
sum(CASE WHEN co.id_tipo_conto = 2 THEN tr.importo ELSE 0 END) AS importo_conto_2,
sum(CASE WHEN co.id_tipo_conto = 3 THEN tr.importo ELSE 0 END) AS importo_conto_3
FROM banca.conto co
INNER JOIN banca.cliente cl
	ON cl.id_cliente= co.id_cliente
INNER JOIN banca.transazioni tr
	ON tr.id_conto = co.id_conto
INNER JOIN banca.tipo_transazione tp
	ON tp.id_tipo_transazione = tr.id_tipo_trans
WHERE tp.segno= "+";

-- 7. TABELLA FINALE
# Il nostro obiettivo è creare una tabella di feature per il training di modelli di machine learning,
# arricchendo i dati dei clienti con vari indicatori calcolati a partire dalle loro transazioni e dai conti posseduti.
# La tabella finale sarà riferita all'ID cliente e conterrà informazioni sia di tipo qualitativo che quantitativo.

# La tabella finale può essere realizzata in 2 modalità: 
# 1) Si utilizza un'unica query andando in join ma estraendo direttamente i dati sulla select;
# 2) Si crea delle tabelle temporanee che poi utilizziamo per andare in join

-- 7.1 SELECT UNICA (= unica query andando in join ma estraendo direttamente i dati sulla select) 

select cl.id_cliente, cl.nome, cl.cognome,
-- 1.Età
floor(datediff(current_date(),cl.data_nascita) /365) as etA,
-- 2. Indicatori sulle transazioni
count(CASE WHEN tp.segno = "-" THEN tr.id_conto END) AS num_transazioni_out,
count(CASE WHEN tp.segno = "+" THEN tr.id_conto END) AS num_transazioni_in,
sum(CASE WHEN tp.segno = "-" THEN tr.importo ELSE 0 END) AS importo_transazioni_out,
sum(CASE WHEN tp.segno = "+" THEN tr.importo ELSE 0 END) AS importo_transazioni_in,

-- 3. Indicatori sui conti
count(DISTINCT co.id_conto) AS num_conti,
count(DISTINCT CASE WHEN co.id_tipo_conto = 0 THEN co.id_conto END) AS count_conto_0,
count(DISTINCT CASE WHEN co.id_tipo_conto = 1 THEN co.id_conto END) AS count_conto_1,
count(DISTINCT CASE WHEN co.id_tipo_conto = 2 THEN co.id_conto END) AS count_conto_2,
count(DISTINCT CASE WHEN co.id_tipo_conto = 3 THEN co.id_conto END) AS count_conto_3,

-- 4. Indicatori sulle transazioni per tipologia di conto
count(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 0 THEN tr.id_conto END) AS num_transazioni_out_0,
count(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 1 THEN tr.id_conto END) AS num_transazioni_out_1,
count(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 2 THEN tr.id_conto END) AS num_transazioni_out_2,
count(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 3 THEN tr.id_conto END) AS num_transazioni_out_3,

count(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 0 THEN tr.id_conto END) AS num_transazioni_in_0,
count(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 1 THEN tr.id_conto END) AS num_transazioni_in_1,
count(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 2 THEN tr.id_conto END) AS num_transazioni_in_2,
count(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 3 THEN tr.id_conto END) AS num_transazioni_in_3,

sum(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 0 THEN tr.importo ELSE 0 END) AS importo_transazioni_out_0,
sum(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 1 THEN tr.importo ELSE 0 END) AS importo_transazioni_out_1,
sum(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 2 THEN tr.importo ELSE 0 END) AS importo_transazioni_out_2,
sum(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 3 THEN tr.importo ELSE 0 END) AS importo_transazioni_out_3,

sum(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 0 THEN tr.importo ELSE 0 END) AS importo_transazioni_in_0,
sum(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 1 THEN tr.importo ELSE 0 END) AS importo_transazioni_in_1,
sum(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 2 THEN tr.importo ELSE 0 END) AS importo_transazioni_in_2,
sum(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 3 THEN tr.importo ELSE 0 END) AS importo_transazioni_in_3

FROM banca.cliente cl
LEFT JOIN banca.conto co 
	ON co.id_cliente = cl.id_cliente
LEFT JOIN banca.transazioni tr 
	ON tr.id_conto= co.id_conto
LEFT JOIN banca.tipo_transazione tp
	ON tp.id_tipo_transazione= tr.id_tipo_trans
GROUP BY 1,2,3,4;

-- 7.2 [VERSIONE FACOLTATIVA] TEMPORARY TABLE (= crea delle tabelle temporanee da utilizzare per andare in join) 
# Vado a creare tabelle temporanee per : età , KPI per transazioni totali, KPI per conti , KPI per tipologia di conto

-- Tab. Temporanea: Età cliente
CREATE TEMPORARY TABLE tmp_eta AS 
SELECT id_cliente, 
floor(datediff(current_date(),data_nascita) /365) as eta
FROM banca.cliente; 

-- Tab. Temporanea: KPI per transazioni totali
CREATE TEMPORARY TABLE tmp_transazioni_tot AS 
SELECT cl.id_cliente, cl.nome, cl.cognome, 
	count(CASE WHEN tp.segno = "-" THEN tr.id_conto END) AS num_transazioni_out,
	count(CASE WHEN tp.segno = "+" THEN tr.id_conto END) AS num_transazioni_in,
	sum(CASE WHEN tp.segno = "-" THEN tr.importo ELSE 0 END) AS importo_transazioni_out,
	sum(CASE WHEN tp.segno = "+" THEN tr.importo ELSE 0 END) AS importo_transazioni_in
FROM banca.cliente cl
LEFT JOIN banca.conto co
	ON co.id_cliente=cl.id_cliente
LEFT JOIN banca.transazioni tr
 	ON tr.id_conto= co.id_conto
LEFT JOIN banca.tipo_transazione tp
 	ON tp.id_tipo_transazione= tr.id_tipo_trans
GROUP BY 1,2,3; 

-- Tab. Temporanea: KPI per conti 
CREATE TEMPORARY TABLE tmp_conti AS 
SELECT cl.id_cliente, cl.nome, cl.cognome, 
count(DISTINCT co.id_conto) AS num_conti,
count(DISTINCT CASE WHEN co.id_tipo_conto = 0 THEN co.id_conto END) AS count_conto_0,
count(DISTINCT CASE WHEN co.id_tipo_conto = 1 THEN co.id_conto END) AS count_conto_1,
count(DISTINCT CASE WHEN co.id_tipo_conto = 2 THEN co.id_conto END) AS count_conto_2,
count(DISTINCT CASE WHEN co.id_tipo_conto = 3 THEN co.id_conto END) AS count_conto_3
FROM banca.cliente cl
LEFT JOIN banca.conto co
	ON co.id_cliente=cl.id_cliente
GROUP BY 1,2,3;

-- Tab. Temporanea: KPI per tipologia di conto
CREATE TEMPORARY TABLE tmp_tipo_conti AS 
SELECT cl.id_cliente, cl.nome, cl.cognome, 
count(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 0 THEN tr.id_conto END) AS num_transazioni_out_0,
count(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 1 THEN tr.id_conto END) AS num_transazioni_out_1,
count(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 2 THEN tr.id_conto END) AS num_transazioni_out_2,
count(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 3 THEN tr.id_conto END) AS num_transazioni_out_3,

count(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 0 THEN tr.id_conto END) AS num_transazioni_in_0,
count(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 1 THEN tr.id_conto END) AS num_transazioni_in_1,
count(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 2 THEN tr.id_conto END) AS num_transazioni_in_2,
count(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 3 THEN tr.id_conto END) AS num_transazioni_in_3,

sum(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 0 THEN tr.importo ELSE 0 END) AS importo_transazioni_out_0,
sum(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 1 THEN tr.importo ELSE 0 END) AS importo_transazioni_out_1,
sum(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 2 THEN tr.importo ELSE 0 END) AS importo_transazioni_out_2,
sum(CASE WHEN tp.segno = "-" AND co.id_tipo_conto= 3 THEN tr.importo ELSE 0 END) AS importo_transazioni_out_3,

sum(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 0 THEN tr.importo ELSE 0 END) AS importo_transazioni_in_0,
sum(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 1 THEN tr.importo ELSE 0 END) AS importo_transazioni_in_1,
sum(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 2 THEN tr.importo ELSE 0 END) AS importo_transazioni_in_2,
sum(CASE WHEN tp.segno = "+" AND co.id_tipo_conto= 3 THEN tr.importo ELSE 0 END) AS importo_transazioni_in_3
FROM banca.cliente cl
LEFT JOIN banca.conto co
	ON co.id_cliente=cl.id_cliente
LEFT JOIN banca.transazioni tr
 	ON tr.id_conto= co.id_conto
LEFT JOIN banca.tipo_transazione tp
 	ON tp.id_tipo_transazione= tr.id_tipo_trans
GROUP BY 1,2,3; 

SELECT cl.id_cliente,cl.nome, cl.cognome,
te.eta, 
ttt.num_transazioni_out,
ttt.num_transazioni_in,
ttt.importo_transazioni_out,
ttt.importo_transazioni_in,
tc.num_conti,
tc.count_conto_0,
tc.count_conto_1,
tc.count_conto_2,
tc.count_conto_3,
ttc.num_transazioni_out_0,
ttc.num_transazioni_out_1,
ttc.num_transazioni_out_2,
ttc.num_transazioni_out_3,
ttc.num_transazioni_in_0,
ttc.num_transazioni_in_1,
ttc.num_transazioni_in_2,
ttc.num_transazioni_in_3,

ttc.importo_transazioni_out_0,
ttc.importo_transazioni_out_1,
ttc.importo_transazioni_out_2,
ttc.importo_transazioni_out_3,
ttc.importo_transazioni_in_0,
ttc.importo_transazioni_in_1,
ttc.importo_transazioni_in_2,
ttc.importo_transazioni_in_3

FROM banca.cliente cl
LEFT JOIN tmp_eta te
	ON te.id_cliente=cl.id_cliente
LEFT JOIN tmp_transazioni_tot ttt
 	ON ttt.id_cliente= cl.id_cliente
LEFT JOIN tmp_conti tc
 	ON tc.id_cliente= cl.id_cliente
LEFT JOIN tmp_tipo_conti ttc
 	ON ttc.id_cliente= cl.id_cliente;


