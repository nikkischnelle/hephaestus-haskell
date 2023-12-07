- [Funktionalität](#funktionalität)
  - [Konfiguration](#konfiguration)
    - [Optionen](#optionen)
  - [Dark/Light Mode](#darklight-mode)
  - [File Browser](#file-browser)
  - [Markdown Viewer](#markdown-viewer)
- [Technical Details](#technical-details)
  - [Configuration](#configuration)
  - [Embedded Files](#embedded-files)
  - [Markdown Viewer](#markdown-viewer-1)
  - [File Viewer](#file-viewer)
  - [Trash Bin](#trash-bin)
  - [API](#api)

# Funktionalität
## Konfiguration
Insofern keine `config.yaml` in dem Ordner liegt, von dem das Projekt gestartet wird, wird folgendes ausgegeben:  

```
Reading config...
Error reading config file: config.yaml: withBinaryFile: does not exist (No such file or directory)
Using default config and storing it to config.new.yaml; please review and move to config.yaml.
```

Wie die Warning bereits aussagt wird nach dem Versuch die Konfiguration der App zu lesen die Standardkonfiguration in `config.yaml.new` abgelegt. Diese kann bearbeteitet werden und danach in `config.yaml` umbenannt werden. Bei einem Neustart der App wird diese dann gelesen.

### Optionen
`logToStdOut: True` führt zum loggen der Anfragen an den Server in die Konsole  
`logToStdOut: False` dazu dass nur andere Log-Einträge in der Konsole angezeigt werden.  

`logPath: hephaestus.log` fürt zum loggen der Anfragen an den Server in die Datei `hephaestus.log`.  
`logPath: ""` führt dazu, dass in keine Datei gelogged wird.  

`port: 3000` konfiguriert den Port auf dem der Server gestartet wird.  
Standardmäßig hierfür ist `3000`, wobei die App dann unter [localhost:3000](http://localhost:3000/) erreichbar ist.

## Dark/Light Mode
Es gibt auf jeder Seite oben rechts einen Button, der zwischen Light- und Dark-Mode hin und her wechselt. 

> Dieses Feature ist mit JavaScript und CSS realisiert. Haskell wird hier nur benutzt um den Button im HTML einzubauen.

![Light Mode Toggle GIF](/files/testFiles/raw files/lightmodeswitching.gif)

## File Browser
Der File-Browser links neben diesem Text zeigt Standardmäßig nur die "oberste Schicht" der Dateien an. Es sind drei Arten von Dateien zu unterscheiden:
- Markdown-Dateien
    - haben keine Dateiendung
    - haben ein herkömmliches Datei-Symbol
- andere Dateien
    - haben Dateiendnungen
    - haben ein Pictogramm für Bild-Dateien als Symbol
- Ordner
    - haben ein Plus (oder Minus) neben sich
    - haben ein Ordner-Symbol

Durch Anklicken eines Ordners, wird dieser aus- bzw. eingeklappt. Es werden dann Dateien und Unterordner, die in diesem Order liegen, angezeigt. Welche Ordner ausgeklappt sind werden im Browserspeicher gespeichert, damit diese bei Neuladen der Seite persisten.
> Das Ausklappen der Ordner und die Speicherung der Ausklappzustände wird mit JavaScript realisiert. Das FileBrowser an sich wird jedoch mit Haskell erstellt.

Durch Anklicken einer Datei wird diese geöffnet. Auf der neuen Seite wird sowohl die Datei, als auch weiterhin der File-Browser angezeigt.

## Markdown Viewer
Der Markdown Viewer nutzt das Github Markdown-Flavor mit zusätzlichen Extensions:

- Subscript
- Superscript
- Defintion Lists
- Footnotes  

Es liegt eine `markdown/sample.md` vor, in der einige der Features ausprobiert werden.  
Der Viewer hat außerdem Syntax-Highlighting für Code-Blöcke. Dies kann in `markdown/syntax-highlighting.md` getestet werden.

> Das CSS der Seite ist so angepasst, dass das Drucken bzw. Konvertieren von Markdown-Dateien in PDFs über die Seite möglich ist. FileBrowser und Light/Dark-Mode Toggle werden beim Drucken versteckt.

# Technical Details
## Configuration
The Configuration YAMl is parsed using the yaml library.

## Embedded Files
Files are embedded inside the binary using the file-embed library:
- CSS and Javascript for the frontend
- Default Files for testing the viewer, including this one. They are put in place if the storage directory is empty.

## Markdown Viewer
Der Markdown Viewer ist mit Pandoc realisiert. Wird eine Markdown-Datei über `localhost:{port}/view/(dateiName)` angefragt, wird die Markdown-Datei in `{storageDir}/files/(dateiName)` eingelesen, mit Pandoc wird HTML generiert und dieses wird dann in die Skelett-Seite[^first] eingefügt.

[^first]: **Skelett-Seite** nenne ich den HTML, der für den FileBrowser, den Light/Dark-Mode Toggle, das Styling und das einbetten vom Javascript zuständig ist. 

## File Viewer
Wird eine Datei aus dem File-Browser geöffnet, die keine Markdown-Datei ist, gibt es drei Möglichkeiten wie diese angezeigt wird. Welche genutzt wird hängt vom Datei Mime-Type ab: 

- Text-Dateien werden in einem simplen `<pre>` angezeigt
- Image-Dateien werden in einem simplen `<image>` angezeigt
- Alle anderen Dateien (wie PDFs) werden ganz einfach durch den Browser angezeigt[^second]

[^second]: Die Dateien können ohne HTML wrapper vom Backend angefragt werden. Für das Anzeigen von Dateien nutze ich dies und embedde in die Skelett-Seite ein iframe, das den Link zu der rohen Datei enthält. Somit kümmert sich der Browser um die Darstellung der Datei für mich.

## Trash Bin
Es gibt eine simple implementierung von einem Papierkorb. Werden Dateien über die API gelöscht, werden sie zuerst in `{storageDir}/trash` verschoben, aus dem sie dann per API-Requests vollends gelöscht oder auch wieder hergestellt werden können.

## API
Zusätzlich zu den Endpunkten, die für die Anzeige von Dateien im Browser genutzt werden, gibt es noch mehr Endpunkte, die aktuell vom Frontend teilweise noch ungenutzt sind.

|Methode | Endpunkt | Funktionalität |
|---|---|---|
| get | /view/(fileName) | HTML-Seiten, die zur Anzeige genutzt werden |
| get | /files/(fileName) | simples serven der Files, auch als Downloadendpunkt nutzbar |
| post | /files/(directoryName) | Uploadendpunkt, der das Hochladen von Dateien in den angegebenen Ordner erlaubt
| delete | /files/(fileName) | Verschieben der Dateien von `{storageDir}/files/(fileName)` in `{storageDir}/trash/(fileName)`.
| post | /trash/(fileName) | Restored die Datei vom Trash-Bin (macht `delete` auf `/files/(fileName)` rückgänging).[^third] 
| delete | /trash/(fileName) | Löscht die Datei im Trash-Bin vom Dateisystem |
| get | /webresources/(resourceName) | CSS und Javascript, die in der Binary embedded sind, also für das Frontend wichtig sind, werden geserved. 


[^third]: Wird fehlschlagen, falls neue Datei an ursprünglichem Ort liegt.