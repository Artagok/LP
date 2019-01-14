# LP
Llenguatges de Programació

### Modificació posterior de l'enunciat (merdes típiques de la FIB)

Degut a que las coordenades de localització que apareixen en l'arxiu XML de la pràctica no poden ser fàcilment usades, considereu la següent modificació de l'enunciat:

A més d'usar el XML de la pàgina d'esdeveniments "mensuals" a la ciutat de Barcelona:

http://w10.bcn.es/APPS/asiasiacache/peticioXmlAsia?id=103

usarem el XML de la pàgina d'esdeveniments "diaris" a la ciutat de Barcelona (on s'inclouen coordenades de latitud i longitud):

http://w10.bcn.es/APPS/asiasiacache/peticioXmlAsia?id=199

Si en la crida al script hi ha paràmetre `date` llavors farem el que demana la pràctica sobre l'arxiu d'esdeveniments mensuals (http://w10.bcn.es/APPS/asiasiacache/peticioXmlAsia?id=103), però no farem la part de les estacions de bicing. És a dir, només mostrarem les dades de les activitats trobades.

Si en la crida no hi ha paràmetre `date` llavors farem el que demana la pràctica però sobre l'arxiu d'esdeveniments diaris (http://w10.bcn.es/APPS/asiasiacache/peticioXmlAsia?id=199) incloent la part de les estacions de bicing i assumint que tots els esdeveniments que apareixen són del dia en curs (és a dir, sense comprovar dates).

Si hi ha paràmetre `date` llavors podeu assumir que no ens donaran paràmetre `distance`.
