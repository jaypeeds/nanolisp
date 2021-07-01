# NANOLISP RETRO-COMPUTING

## Motivation

Dans les années 80, bien avant que cette tendance ne devienne possible économiquement pour le plus grand nombre
certains passionnés ont entrevu les développemet possibles d'une robotique personnelle.
C'est ainsi qu'est née la revue "Micros et Robots" dédiée à cette thématique. Sa vie fut brève, seulement 16 numéros
ont été publiés. En 1985, une série de trois articles sur le langage LISP sous la plume de JM Husson, commence au numéro
15. Quinze, seize, mais où est passé le troisième volet ? Cette revue était le rejeton d'une institution datant des 
années 30, et c'est dans la revue mère, Radio-Plan, que le troisième volet est paru. On retrouve assez facilement des 
scans de qualité suffisante de ces revues, sur le site "abandonware" ou sur "web.archive.org".
Le code est en Pascal, langage mainstream à cette époque, grâce à Apple et le système UCSD sur Apple II, puis plus tard
MacPascal ou TurboPascal sur PC.
Ma motivation est multiple avec une grande curiosité pour commencer, et l'envie de comprendre, explorer, tester les 
limites, peut-être les repousser.
LISP est avec FORTRAN l'un des plus anciens langages de programmation, datant du milieu du XXe siècle. Aujourd'hui, 
il renaît en permanence, réinventé sur chaque écosystème productif, sur la JVM avec Clojure, sur la machine BEAM 
d'Erlang avec LFE ou Clojerl, et même sur les micro-contrôleurs avec uLisp.com.   

## La démarche
Je me souviens avoir dactylographié tout le code lors de sa parution. Aujourd'hui, j'ai tenté une méthode plus efficace,
qui a plus ou moins réussi. Les listings ont été extraits par copie d'écran en séparant les colonnes publiées 
côte-à-côte, puis transformés en PDF et déposés sur un Google drive. Le service Google permet de faire de l'OCR sur de 
tels documents si on les ouvre en tant que Google Doc.
L'IDE de choix pour le Pascal est Lazarus. Sur Mac, c'est assez compliqué à faire fonctionner, comme tous les outils 
hors Apple. Malgré les indications trouvées, malgré le certificat de signature du code, je n'ai pas réussi à passer la 
sécurité qui assure que le débugger gdb ou lldb s'utilse de manière sûre. Je me suis donc tourné vers Linux pour avancer
plus vite.
Si on considère le fait que la plupart des implémentations de Lisp ont été écrites en Assembleurs divers, Pascal, c'est 
presque de la triche ! Il existe des versions plus récentes, écrites en C, mais elles ont souvent une perspective 
moderne et avec la connaissance actuelle des langages de programmation (provenant de C++/Java). La démarche est 
différente lorsqu'on s'attache à suivre le chemin historique : Les S-Exp et leurs sept opérateurs quote, atom, eq, car,
cdr, cons et cond.  

## Le bilan
Il a fallu faire des ajustements, l'interpréteur lit un fichier source conventionnel SOURCE.NLSP. On peut choisir le nom
que l'on veut puis faire un lien symbolique avant éxécution. La plupart des fonctions, notamment la définition de 
fonction, sont opérationnelles.
Les conventions de nommage devrait être plus strictes: Les primitives sont nommées F suivi de la primitive : FCAR, FCDR
mais pour être exploitables, ces opérateurs devraient tous avoir un résultat de type S-Exp, ou SGRAPHE dans ce code.
FNULL ne respecte pas cette convention.

## Et ensuite ? 
- Implémenter l'arithmétique entière. .
- Permettre des commentaires pour documenter le code. 
La méthode consiste à ajouter la syntaxe nouvelle et à analyser le crash sous débugger et modifier le code en fonction.
- Porter en C simple.
