# Building
Das Projekt hat für ein Feature magic als Dependency, die den mime-type von Dateien feststellen kann. Da Windows keine Mime-types kennt, ist das Projekt nur auf Unix-Systemen unterstützt.
Damit das Projekt läuft, muss libmagic installiert sein. Bei den meisten Linux-Distributionen stellt das Paket `file` diese bereit (oder installiert diese als dependeny von `file`).

Ist `file` installiert, kann mit `cabal build` eine binary gebaut werden.  
Dies funktioniert auch in WSL; getestet mit Ubuntu 22.04 mit `sudo apt install file`.  
Der Build-Prozess dauert aufgrund der Größe von Pandoc ziemlich lang.

Ich empfehle die binary zu verschieben und über die Konsole in einem leeren Ordner auszuführen.
Im Ordner aus dem die binary ausgeführt wird werden nämlich Dateien und Ordner erstellt.

## Getestete Versionen
Ich habe mit folgenden Versionen das builden getestet:
> Scotty funktioniert nicht auf `latest`, da `latest` die Version 2.1 von Text shipped und Scotty diese aktuell auf < 2.1 beschränkt.

|GHC|Cabal|HLS|
|--|--|--|
|9.4.7 (base-4.17.2.0)|3.6.2.0-p1|2.4.0.0|
