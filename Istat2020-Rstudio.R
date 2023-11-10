# Analizzeremo il dataset ISTAT2020_red
# Il dataset “ISTAT2020_red” è un dataset che contiene i dati statistici relativi alla popolazione, alla società, all’istruzione, al lavoro, all’economia, all’ambiente e al territorio dell’Italia per l’anno 2020. Il dataset è stato prodotto dall’Istituto Nazionale di Statistica (Istat), che è l’ente pubblico che si occupa di raccogliere, elaborare e diffondere i dati ufficiali sulla realtà italiana.


# Analizzeremo prima il dataset compiendo su di esso analisi di statistica descrittiva, per poi andare più in profondità usando parametri di regressione a seconda di come i dati si presenteranno.


# Statistica descrittiva --------------------------------------------------

# Iniziamo estrapolando i nomi delle tabelle, ci sarà utile per capire quali sono i parametri che dobbiamo analizzare
names(ISTAT2020_red)
length(names(ISTAT2020_red))
#  Vi sono 38 variabili, ciò indica un dataset molto voluminoso e completo per lo scopo che si prefigge, cioè di analizzare i vari settori della società italiana


# Poi vediamo se nelle tabelle ci sono valori anomali, tipo NA, che dobbiamo elidere
# Iniziamo con la prima variabile che rappresenta il numero dei componenti del nucleo familiare
ISTAT2020_red$c_Ncmp_fatto
summary(ISTAT2020_red$c_Ncmp_fatto)
table(ISTAT2020_red$c_Ncmp_fatto)
# Abbiamo innanzitutto stampato i valori della colonna, poi abbiamo verificato che non ci fossero valori anomali attraverso la funzione "summary()", che ci ha anche dato interessanti informazioni sul numero dei componenti del nucleo familiare in Italia nel 2020, la cui media è 2.281, quindi concludiamo che le famiglie italiane sono in media di 2 individui (con un grado di approssimazione accettabile di 1 cifra dopo la virgola)


# Adesso controlliamo il sesso del primo componente del nucleo familiare
ISTAT2020_red$sesso_1
summary(ISTAT2020_red$sesso_1)
table(ISTAT2020_red$sesso_1)
# Neanche qui vi sono fortunatamente valori anomali come si evince da summary(). Gli 1 della lista rappresentano i maschi, i 2 le femmine. Vi sono più maschi che femmine, i maschi sono 16354 e le femmine 9314.


# Controlliamo anche la variabile relativa al paese di nascita del primo componente del nucleo familiare
ISTAT2020_red$pnasc_1
summary(ISTAT2020_red$pnasc_1)
table(ISTAT2020_red$pnasc_1)
# Nessun valore anomalo neanche qui. I valori 1 indicano che il primo componente del nucleo familiare è nato in Italia ed il 2 in Albania. Sono 23550 gli individui con il primo componente nato in Italia e 2118 con il primo nato in Albania.


# Adesso quella relativa alla classe di età del primo componente del nucleo familiare
ISTAT2020_red$c_etacalc_1
summary(ISTAT2020_red$c_etacalc_1)
table(ISTAT2020_red$c_etacalc_1)
hist(ISTAT2020_red$c_etacalc_1, main = "Classe di età del primo componente familiare", xlab = "Classe di età", ylab = "Frequenza", breaks = c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), labels = c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+", "Sconosciuta", "Non dichiarata"))
# Nessun valore anomalo neanche qui. Il valore 4 si ripete 130 volte, il che significa che ci sono 130 nuclei familiari in cui il primo componente ha un’età compresa tra 0 e 14 anni. Il valore più frequente è il 15, che si ripete 4871 volte, il che significa che ci sono 4871 nuclei familiari in cui l’età del primo componente non è calcolabile. Questo potrebbe dipendere da diversi motivi, come la mancanza di informazioni sulla data di nascita o la presenza di errori nei dati. Per avere una visione più chiara della distribuzione dei dati, si potrebbe usare la funzione hist() per creare un istogramma della variabile, che mostra la frequenza dei dati per ogni classe di età.


# Adesso relativa alla cittadinanza del primo componente del nucleo familiare
ISTAT2020_red$cittad_1
summary(ISTAT2020_red$cittad_1)
table(ISTAT2020_red$cittad_1)
# Nessun valore anomalo. I valori 1 indicano che il primo componente ha cittadinanza italiana ed i valori 2 che il primo componente ha cittadinanza straniera. In questo caso, sono di più coloro che hanno cittadinanza italiana che sono 24379 rispetto a coloro che, come primo componente del nucleo familiare, hanno cittadinanza straniera, ne contiano 1289.


# Adesso stampiamo i valori che riguardano il paese di nascita del padre del primo componente del nucleo familiare
ISTAT2020_red$pnasc_padre_1
summary(ISTAT2020_red$pnasc_padre_1)
table(ISTAT2020_red$pnasc_padre_1)
# Nessun NA, o valore anomalo, questo significa che nessuno degli interessati ha voluto non dichiarare il paese di nascita del padre, o che non ci sono stati errori di compilazione del dataset. Gli 1 rappresentano il paese di nascita Italia e sono la maggioranza, cioè 23815; i 2 rappresentano chi ha genitore nato in Albania e sono 1832, infine Marocco che sono solo 21. 


# Possiamo creare un grafico a mosaico che metta in relazione il numero dei componenti del nucleo familiare con il paese di nascita del primo componente
library(mosaic)
mosaicplot(~c_Ncmp_fatto + pnasc_1, data = ISTAT2020_red, shade = TRUE, legend = TRUE)
# Dal grafico si evince che la maggioranza delle famiglie italiane sono composte da una o due persone (8043 e 8281 rispettivamente) e che la maggioranza di esse hanno il primo componente nato in Italia (23550). Si può anche notare che le famiglie con il primo componente nato all'estero (2118) tendono ad avere un numero maggiore di figli rispetto alle famiglie con il primo componente nato in Italia. Per esempio le famiglie con 6 componenti sono quasi tutte con il primo componente nato al'estero.
# Notiamo a destra valori come il p-value ed i residui di Pearson, un p-value con la tonalità rossa forte, indica una forte correlazione tra le due variabili in gioco, invece una forte tonalità blu indica il contrario. Sembra esserci una correlazione significativa tra una famiglia con il primo componente nato all'estero e l'avere una famiglia con due figli.


# Analizziamo adesso la classe di reddito annuo netto in euro del primo componente del nucleo familiare
ISTAT2020_red$c_redd_1
summary(ISTAT2020_red$c_redd_1)
table(ISTAT2020_red$c_redd_1)
# Nessun valore anomalo. Ci sono 9431 persone analizzate che hanno un reddito inferiore a 10.000, 2988 che hanno un reddito tra 10.000 a 14.999, 10.003 persone che hanno un reddito compreso tra 15.000 a 19.999, 1718 che hanno reddito tra 20.000 a 24.999, 593 che hanno reddito compreso tra 25.000 a 29.999 euro, 864 persone tra 30.000 e 34.999 euro e 71 persone con reddito tra 35.000 euro in sopra.
# Caricare la libreria ggplot2
library(ggplot2)
# Trasformare sesso_1 in fattore
ISTAT2020_red$sesso_1 <- factor(ISTAT2020_red$sesso_1, levels = c(1, 2), labels = c("Maschio", "Femmina"))
# Trasformare c_redd_1 in fattore
ISTAT2020_red$c_redd_1 <- factor(ISTAT2020_red$c_redd_1, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("<=10k", "10-15k", "15-20k", "20-25k", "25-30k", "30-35k", ">35k"))
# Creiamo un grafico a violino
ggplot(ISTAT2020_red, aes(x = sesso_1, y = c_redd_1, fill = sesso_1, group = factor(sesso_1))) +
  geom_violin() +
  scale_x_discrete(labels = c("Maschio", "Femmina")) +
  scale_y_discrete(labels = c("<=10k", "10-15k", "15-20k", "20-25k", "25-30k", "30-35k", ">35k")) +
  scale_fill_discrete(labels = c("Maschio", "Femmina")) +
  labs(x = "Sesso del primo componente", y = "Classe di reddito (in euro)", fill = "Sesso") +
  theme_minimal()
# Abbiamo usato questo grafico perché ci permette di vedere quanto il sesso del primo componente del nucleo familiare ha affinità con la classe di reddito (in euro); dal grafico evinciamo che i le famiglie che hanno un maschio come primo componente si trovano con molta più frequenza nella classe di reddito intervallata tra 15-20k, così come le femmine; ma i maschi che godono di questo trattamento sono in misura superiore; ma è anche interessante notare che le famiglie che hanno come primo componente una femmina arrivano a classi di reddto più elevate rispetto ai maschi, si veda la zona 30-35k.



# Prendiamo adesso in esame il tipo di contratto di lavoro del primo componente del nucleo familiare
ISTAT2020_red$contratto_1
summary(ISTAT2020_red$contratto_1)
table(ISTAT2020_red$contratto_1)
# Qui vi sono molti valori anomali, gli NA in questo caso ci mostrano che la maggioranza degli individui è senza contratto di lavoro, invece i numeri "1" degli interessati godono di un contratto di lavoro a tempo indeterminato e sono 9356 e coloro che hanno un contratto di lavoro a tempo determinato sono 155. 


# Prendiamo in esame i poveri
ISTAT2020_red$poveri
summary(ISTAT2020_red$poveri)
table(ISTAT2020_red$poveri)
# Ci sono 23474 0, che sono le famiglie non povere, e gli 1, che sono le famiglie povere e sono 2194, quindi di meno.

# Adesso le regioni di residenza
ISTAT2020_red$rgn
summary(ISTAT2020_red$rgn)
table(ISTAT2020_red$rgn)
# Si può osservare che la regione con il maggior numero di famiglie nel dataset è la Lombardia (codice 3), con 3568 famiglie, seguita dal Lazio (codice 12), con 2259 famiglie, e dalla Campania (codice 15), con 1785 famiglie. La regione con il minor numero di famiglie nel dataset è il Molise (codice 14), con 617 famiglie, seguita dalla Basilicata (codice 17), con 588 famiglie, e dalla Valle d’Aosta (codice 2), con 488 famiglie. Non ci sono valori mancanti (<NA>) nella variabile rgn.

# Convertire le variabili contratto_1 e rgn in fattori
ISTAT2020_red$contratto_1 <- factor(ISTAT2020_red$contratto_1, levels = c(1, 2, 3), labels = c("Tempo indeterminato", "Tempo determinato", "Disoccupato"))
ISTAT2020_red$rgn <- factor(ISTAT2020_red$rgn, levels = c(1:20), labels = c("Piemonte", "Valle d'Aosta", "Lombardia", "Trentino-Alto Adige", "Veneto", "Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna", "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia", "Sardegna"))
# Creare una tabella di contingenza tra il tipo di contratto e la regione
tab <- table(ISTAT2020_red$contratto_1, ISTAT2020_red$rgn)
# Convertire la tabella in un data frame
df <- as.data.frame(tab)
# Rinominare le colonne del data frame
colnames(df) <- c("Contratto", "Regione", "Frequenza")
# Creare un grafico a calore con il tipo di contratto sull'asse x e la regione sull'asse y


# Trasformo le variabili categoriche in numeriche
df$Contratto <- as.numeric(factor(df$Contratto, levels = c("Tempo indeterminato", "Tempo determinato", "Disoccupato")))
df$Regione <- as.numeric(factor(df$Regione, levels = c("Piemonte", "Valle d'Aosta", "Lombardia", "Trentino-Alto Adige", "Veneto", "Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna", "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia", "Sardegna")))

ggplot(df, aes(x = Contratto, y = Regione, fill = Frequenza)) +
  geom_raster() +
  scale_fill_gradientn(colours = c("white", "yellow", "orange", "red"), trans = "log10") +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("Tempo indeterminato", "Tempo determinato", "Disoccupato")) +
  scale_y_continuous(breaks = c(1:20), labels = c("Piemonte", "Valle d'Aosta", "Lombardia", "Trentino-Alto Adige", "Veneto", "Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna", "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia", "Sardegna")) +
  labs(x = "Tipo di contratto del primo componente", y = "Regione di residenza") +
  theme_minimal()


# Descrizione teorica: Vedendo questi grafici e questi dati notiamo che in Italia vi sono meno famiglie povere di quelle non-povere (quindi anche di medio reddito), che i livelli di disoccupazione sono, almeno secondo un punto di vista statistico, allarmanti più della mancanza di contratti a tempo indeterminato. Ora possiamo verificare se possiamo fare la regressione ad esempio tra i poveri ed il primo componente del nucleo familiare, le due variabili sembrano correlate, ma per dimostrarlo facciamo il test chi-quadro. Poi eventualmente adotteremo una regressione logit.



# Regressione -------------------------------------------------------------
# Creare una tabella di contingenza tra le variabili poveri e cittad_1
tab <- table(ISTAT2020_red$poveri, ISTAT2020_red$cittad_1)

# Eseguire il test chi-quadro
chisq.test(tab)
# Il test chi-quadro ha dato un valore test elevatissimo ed un p-value bassissimo rispetto al valore di confidenza 0.05, dunque concludiamo che c'è relazione tra le due variabili e che possiamo adottare la regressione logit per stimare la probabilità che una famiglia sia povera in funzione della cittadinanza del primo componente.

# Creare un modello di regressione logistica con la variabile poveri come dipendente e la variabile cittad_1 come indipendente
logit <- glm(poveri ~ cittad_1, data = ISTAT2020_red, family = binomial())
# Visualizzare il riassunto del modello
summary(logit)
# Dal modello estrapoliamo le seguenti informazioni: poiché i valori dei quartili sembrano la maggioranza di essi prossimi allo zero, concludiamo che i valori predetti dal modello ed i valori osservati combaciano in maniera molto alta, quindi il modello è idoneo; nella parte dei coefficienti notiamo che la stima di "cittad_1" è 1.32994, il che significa che passare da cittadino italiano a straniero ha un impatto di povertà di circa 1.3 logit in probabilità, i p-value dimostrano che la variabile indipendente ha effetto significativo su quella dipendente. Poi vi è un parametro di dispersione della famiglia binomiale che è stato assunto uguale ad 1, il che significa che il modello è adeguato. La devianza residua è inferiore alla devianza nulla, dunque i dati si adattano bene al modello. L'AIC è basso, dunque il modello è preferibile. Infine l'algoritmo di Fisher ci dice il numero di iterazioni usate per trovare i coefficienti, in questo caso 5 indica che l'algoritmo ha funzionato bene.


# Creare un grafico con la variabile cittad_1 sull'asse x e la variabile poveri sull'asse y
ggplot(ISTAT2020_red, aes(x = cittad_1, y = poveri)) +
  # Aggiungere i punti con il colore in base alla variabile poveri
  geom_point(aes(color = factor(poveri))) +
  # Aggiungere la curva di regressione logistica con l'intervallo di confidenza al 95%
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  # Aggiungere le etichette degli assi e del titolo
  labs(x = "Cittadinanza del primo componente", y = "Probabilità di essere poveri", title = "Regressione logistica tra cittadinanza e povertà") +
  # Aggiungere la legenda per il colore
  scale_color_discrete(name = "Condizione di povertà", labels = c("Non povero", "Povero")) +
  # Aggiungere il tema minimalista
  theme_minimal()
# Dal grafico notiamo che la probabilità di essere povero sono maggiori se si è di cittadinanza straniera, inoltre la nuvola attorno alla linea indica che la probabilità è incerta contando un intervallo di confidenza del 95%.



# Adesso controlliamo se possiamo fare una regressione multinomiale tra le variabili "regioni" e "pnasc_1", cioè primo componente del nucleo familiare
tab2 <- table(ISTAT2020_red$rgn, ISTAT2020_red$pnasc_1)
chisq.test(tab2)
# Anche in questo caso, il valore del test è elevato ed il p-value basso, quindi possiamo adottare la regressione multinomiale


library(nnet)
# Trasformare le variabili categoriche in fattori
ISTAT2020_red$rgn <- factor(ISTAT2020_red$rgn)
ISTAT2020_red$pnasc_1 <- factor(ISTAT2020_red$pnasc_1)
# Il modello ha usato il Piemonte come livello di riferimento per la variabile risposta, e l’Italia come livello di riferimento per la variabile esplicativa. Da questi risultati, si può interpretare i coefficienti di regressione come i logit delle probabilità relative di appartenere a una determinata regione rispetto al Piemonte, a parità del paese di nascita del primo componente. Per esempio, il coefficiente di pnasc_12 per la Valle d’Aosta è pari a -0.54582256, il che significa che la probabilità relativa di appartenere alla Valle d’Aosta rispetto al Piemonte è minore di 0.54582256 volte per i soggetti nati in un paese diverso dall’Italia rispetto a quelli nati in Italia, a parità delle altre variabili esplicative.



# Aggiustare il modello di regressione multinomiale
mnm <- multinom(rgn ~ pnasc_1, data = ISTAT2020_red)
# Visualizzare i risultati
summary(mnm)
# Caricare il pacchetto effects
library(effects)
# Tracciare l'effetto di pnasc_1
plot(effect("pnasc_1", mnm))
# La curva che aumenta da sinistra verso destra in alcune regioni indica la curva di densità che aumenta mostra la distribuzione dei dati per ogni livello della variabile pnasc_1. La curva di densità è una funzione che approssima la probabilità che un dato assuma un certo valore. Più la curva è alta in un punto, più è probabile che il dato sia vicino a quel valore.


