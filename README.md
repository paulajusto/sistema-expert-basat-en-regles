# Sistema Expert Basat en Regles — Generador de Menús Personalitzats

Aquest projecte implementa un sistema expert desenvolupat en CLIPS capaç de generar menús personalitzats per a esdeveniments (bodes, batejos, comunions, congressos). El sistema construeix automàticament tres menús complets (econòmic, mitjà i car) a partir de les preferències i restriccions proporcionades per l’usuari.

## Funcionalitats principals

* Generació automàtica de menús segons:

  * Tipus d’esdeveniment
  * Pressupost
  * Estil gastronòmic
  * Temporada
  * Complexitat
  * Ingredients prohibits
  * Dietes i al·lèrgies
* Suport per a menús infantils
* Recomanació de vins o maridatge
* Opcions específiques per a bodes
* Ontologia construïda amb Protégé
* Base de dades de plats estructurada
* Script Python per a la conversió CSV → CLP

## Estructura del projecte

```
/ontologia.clp        Definició de classes i instàncies derivades de Protégé
/database.clp         Base de dades de plats en format CLIPS
/csv2clips.py         Script de conversió de CSV a .clp
/executable.clp       Fitxer principal que integra tots els mòduls del sistema
```

### Mòduls principals

* SHARED: definicions globals i plantilles
* PREGUNTES: interacció amb l’usuari
* ABSTRACCIO: derivació de coneixement
* ASSOCIACIO: selecció de l’arquetip
* ONTOBRIDGE: connexió amb la ontologia
* REFINAMENT: construcció de menús i càlcul de preus
* IMPORTADOR: importació i conversió de dades
* MAIN: control del flux d’execució

## Execució

Per executar el sistema correctament, cal carregar els fitxers en el següent ordre:

1. Obrir CLIPS.
2. Carregar l’ontologia:

   ```
   (load "ontologia.clp")
   ```
3. Carregar la base de dades:

   ```
   (load "database.clp")
   ```
4. Carregar el motor principal:

   ```
   (load "executable.clp")
   ```
5. Inicialitzar i executar el sistema:

   ```
   (reset)
   (run)
   ```

## Jocs de prova

El projecte inclou deu jocs de prova que validen el sistema en escenaris diversos:

* Diferents estils i temporades
* Menús infantils
* Gestió d’al·lèrgies i dietes
* Ingredients prohibits
* Casos específics com bodes o menús regionals

## Possibles millores

* Ampliació de la base d’ingredients per plat
* Integració completa d’una base de dades de begudes
* Suport per a més al·lèrgies
* Optimització del filtratge i la selecció de plats

## Autors

Paula Justo
Albert Roca
Júlia Pedrol

Grau en Intel·ligència Artificial — FIB (UPC)
