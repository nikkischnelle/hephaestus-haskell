# First use
`cabal run`

Insofern keine `config.yaml` in dem Ordner liegt, von dem das Projekt gestartet wird, wird folgendes ausgegeben:  

```
Reading config...
Error reading config file: config.yaml: withBinaryFile: does not exist (No such file or directory)
Using default config and storing it to config.new.yaml; please review and move to config.yaml.
```

Wie die Warning bereits aussagt wird nach dem Versuch die Konfiguration der App zu lesen die Standardkonfiguration in `config.yaml.new` abgelegt. Diese kann bearbeteitet werden und danach in `config.yaml` umbenannt werden. Bei einem Neustart der App wird diese dann gelesen.

## Config Options
Die Applikation kann Anfragen an den Server loggen. Dies kann in die Konsole über stdout passieren, wenn in der Konfiguration  
`logToStdOut` auf `true` gesetzt wurde.  
Die App kann aber auch in eine Datei loggen (was standardmäßig in `hephaestus.log` geschieht). Um diesen Log zu deaktivieren, kann `logPath: ""` eingetragen werden. Möchte mann dass die Logdatei an einen anderen Ort oder mit anderem Namen geschrieben wird, kann man diesen hier ebenfalls eintragen.


Letztendlich kann noch der Port geändert werden auf dem die App bereit steht. Der Standard hierfür ist `3000`, wobei die App dann unter [localhost:3000](http://localhost:3000/) erreichbar ist.
unter `port` kann aber auch jeder andere Port (der vom User genutzt werden darf) eingetragen werden.