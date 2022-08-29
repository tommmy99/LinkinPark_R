library("wesanderson")
library("ggrepel")          # Install ggrepel package
library("tidyverse")
library("gt")
library("knitr")
library("dplyr")
library("linkinpark",warn.conflicts = FALSE)
library("ggplot2")
tinytex::install_tinytex()
#' Audio Features of Linkin Park Songs
#'
#' @md
#' @description A collection of spotify audio features of Linkin Park Albums
#'
#' @format A tibble of 6577 observations of 39 variables
#'
#' See the Spotify API documentation for audio features.  https://developer.spotify.com/documentation/web-api/
#' @export
"audio_features"
audio_features
#album_images
#track_name,
#album_name,
#album_release_date,
#danceability,  #danzabilità  (informazioni ricavate da spotify)
                #------------------------------>Definita tramite delle caratteristiche della canzone come (beat strength), tempo, stabilità, ...
                #                               Il valore tornato  determina la facilità con cui una persona può danzare ad una canzone durante il suo corso
#energy,        #energia
                #------------------------------>L'energia sonora è il movimento dell'energia attraverso una sostanza in onde
                #
#speechiness,   #discorso
                #------------------------------>Parole e Frasi delle canzoni
                #
#acousticness,  #acustica
                #------------------------------>Misura da 0 a 1 di quanto una canzone sia acustica
                #
#valence        #valenza
                #------------------------------>Positivezza derivante da una canzone (misura da 0 a 1)
                #
#loudness       #


#Cercare di studiare come variano le precedenti caratteristiche delle canzoni negli anni

#Provare a vedere se conviene veramente usare geom_point o meglio una bar/...

#Distribuzione nel corso degli anni
ggplot(data = audio_features) +
  geom_bar(mapping = aes(x = album_release_year)) #Aggiungerci il colore a tutti

t2020 = audio_features %>%
  filter(album_release_year == "2020") %>%
  select(album_release_date,album_name,artist_name)

ggplot(data = t2020) +
  geom_point(mapping = aes(x = album_release_date, y = album_name, fill = artist_name)) +
  scale_x_discrete(guide = guide_axis(angle = 90))

ggplot(data = audio_features) +
  geom_bar(mapping = aes(x = speechiness))

ggplot(data = audio_features) +
  geom_bar(mapping = aes(x = danceability))
           
#XK non funziona la bar??
ggplot(data = audio_features) +
  geom_point(mapping = aes(x = acousticness, y = album_release_year), alpha = 0.2)

#ggplot(data = audio_features) +
#  geom_bar(mapping = aes(x = acousticness))

ggplot(data = audio_features) +
  geom_bar(mapping = aes(x = valence))

ggplot(data = audio_features) +
  geom_bar(mapping = aes(x = loudness))

ggplot(data = audio_features) +     
  geom_bar(mapping = aes(x = instrumentalness))

ggplot(data = audio_features) +     
  geom_bar(mapping = aes(x = liveness))

#Scegliere se ultimi 3/4 grafici metterli come point o lasciare bar --> pensare a qualcosa per parlare della questione

####Utilizzare questo dataset per confrontare i risultati del riia con i risultati del billboard (ovvero quali sono i migliori e confronti)####

#----------------------------------------------------------------------------------------------------------------------------------------------
#' Recording Industry Association of America Awards For Linkin Park
#'
#' @md
#' @description A table that includes Awards given to Linkin Park by the
#' Recording Industry Association of America (RIAA). Awards are given out
#' by the RIAA based on number of copies sold, with albums that sell more
#' than 500,000 are certified Gold, and albums that reach 1,000,000 certified
#' sales are certified Platinum. A Diamond Award is also given for
#' 10,000,000 sales. For certification purposes, each sale is a unit measured
#' by one of the following:
#' + Sale of digital or physical album
#' + 10 individual track downloads
#' + 1500 steamed audio or video from the album
#'
#' All data was scraped from the RIAA Gold/Platinum search.
#'
#' @format A tibble with 88 observations of 6 variables.
#' \describe{
#'   \item{album_title}{the name of the album}
#'   \item{release_date}{date of album release}
#'   \item{format}{the type of release c("album", "single")}
#'   \item{certification}{certification type c("gold", "platinum")}
#'   \item{certification_date}{date of certification}
#'   \item{plat_modifier}{the multiplier for platinum i.e. 2x platinum or 11x platinum}
#' }
"riaa_lp"

#----------------------------------------------------------------------------------------------------------------------------------------------
#Quando sono stati pubblicati i vari album - le varie canzoni?
#Ho voluto evidenziare il fatto di quali di questi siano album e di quali siano canzoni
#Qui sono state definite le date di quando sono state certificate le canzoni, certificazione ovvero.... 
#dal grafico si riesce a vedere più o meno 

#Quali sono le canzoni che posseggono una certificazione?
ggplot(data = riaa_lp) +
  geom_point(mapping = aes(x = release_date ,y = album_title, color = format))

ggplot(data = riaa_lp) +
  geom_point(mapping = aes(x = release_date ,y = album_title, color = certification))

#Qual è l'andamento delle varie certificazioni? Ovvero --> Da quando vengono rilasciate a quando sono certificate quanto tempo è passato
#Modificabile tramite un mutate, ovvero
r = riaa_lp %>%
  mutate(tempo = as.numeric(certification_date - release_date,units = "days")) #numero in giorni

ggplot(data = r,mapping = aes(x = tempo)) +
  geom_bar() 

#delayedAssign("riaa_lp", local({
#  if (requireNamespace("tibble", quietly = TRUE)) {
#    tibble::as_tibble(linkinpark:::riaa_lp)
#  } else {
#    linkinpark:::riaa_lp
#  }
#}))

#IDEALMENTE DOVREBBE RESTITUIRE SOLO GLI ELEMENTI PLATINATI CON IL LORO MODIFICATORE APPOSITO
#Tra tutti gli album platinati come varia tra tutti il modificatore?

riaa = riaa_lp %>%
  filter(certification == "Platinum")%>%
  select(release_date,album_title,plat_modifier)

ggplot(data = riaa) +
  geom_point(mapping = aes(x = release_date ,y = album_title, color = plat_modifier)) +
  scale_color_gradient(low = "lightgrey", high = "purple") 

 #Interessante per vedere quando i primi album hanno ricevuto la loro certificazione


# I migliori album Rispetto alla classifica Billboard -> ovvero gli album migliori degli stati uniti 
#-----------------------------------------------------------------------------------------------------------------------------------
#' @format A tibble with 10 observations of 4 variables
#' + `album_title`: Nome dell'album
#' + `peak_position`: Posizione di picco raggiunta dall'album
#' + `date_peaked`: Data in cui l'album ha raggiunto il picco (Format YYYY-MM-DD)
#' + `weeks_on_chart`: numero di settimane in cui è entrato in classifica.
#' + `chart_name`: Name of chart
#' @export
"billboard_albums"

#-----------------------------------------------------------------------------------------------------------------------------------

#PUNTO 2.1))))
ggplot(data = billboard_albums) +
  geom_point(mapping = aes(x = date_peaked, y = album_title)) # --> interessa solo quali album sono entrate in classifica e quando

ggplot(data = billboard_albums) + 
  geom_bar(mapping = aes(x = weeks_on_chart, fill = album_title)) #--> interessa quali album entrate in classifica (viste in precedenza) quanto ci 
# hanno messo in termini di settimane per entrare nella classifica

ggplot(data = billboard_albums) + 
  geom_bar(mapping = aes(x = peak_position, fill = album_title))  #Dopo aver visto tutte le info apposite si vuole vedere quale è stata la posizione
                                                                #Finale raggiunta (più alta)

ggplot(data = billboard_albums,mapping = aes(x = weeks_on_chart,y = peak_position)) + 
  geom_point() +
  geom_smooth()

#interessante da vedere (nel peak_position in particolare si possono vedere risultati vicini allo 0 ma questo stanno a significare risultati buoni)
#come ...

#Grafico per raffigurare la situazione in base a quante settimane gli album abbiano impiegato per entrare nella classifica
#-----------------------------------------------------------------------------------------------------------------------------------

#' Peak Position of Linkin Park Songs on Billboard
#'
#' @md
#' @description A data set containing peak positions of charted Linkin Park Songs
#' from the Billboard Hard Rock streaming songs Chart. This data was scraped from billboard.com
#'
#' @format A tibble with 10 observations of 4 variables
#' + `song_title`: Nome della canzone
#' + `peak_position`: La più alta posizione in classifica raggiunta
#' + `date_peaked`: Data in cui la canzone ha raggiunto il picco (Format YYYY-MM-DD)
#' + `weeks_on_chart`: Numero di settimante in cui la canzone è entrata in classifica.
#' @export
"billboard_songs"
#--------------------------------------------------------------------------------------------------------------------------------------

#PUNTO 2.2)))) -->Domande valgono anche per la 2.1<--
#BILLBOARD RISPETTO A: NOME CANZONE, QUANDO SONO ENTRATE IN CLASSIFICA E QUANTE SETTIMANE CI HANNO MESSO PER ENTRARE IN CLASSIFICA
#NOTA LE CANZONI PIU CONOSCIUTE (OVVERO IN THE END,...) CI HANNO MESSO MENO TEMPO ...

#Quando sono entrate in classifica tali canzoni? --> ###possibile visualizzazione tramite smooth()###
ggplot(data = billboard_songs) +
  geom_point(mapping = aes(x = date_peaked, y = trackname)) # --> interessa solo quali canzoni sono entrate in classifica e quando

#Qual'è la vetta più alta che hanno raggiunto? Quali sono i casi peggiori/migliori?
ggplot(data = billboard_songs) +
  geom_point(mapping = aes(x = peak_position, y = trackname))

ggplot(data = billboard_songs) + 
  geom_bar(mapping = aes(x = weeks_on_chart, fill=trackname)) #--> interessa quali canzoni entrate in classifica (viste in precedenza) quanto ci 
                                                              #    hanno messo in termini di settimane per entrare nella classifica

#Quanto tempo ci hanno messo da quando sono entrate fino alla loro posizione di punta? Qual'è l'andamento?
ggplot(data = billboard_songs,mapping = aes(x = weeks_on_chart,y = peak_position)) + 
  geom_point() +
  geom_smooth()

#contrapposizioni tra entrambe le parti ovvero confronti tra album e canzoni


