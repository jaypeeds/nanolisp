# NANOLISP RETRO-COMPUTING

## Motivation

Dans les années 80, bien avant que cette tendance ne devienne possible économiquement pour le plus grand nombre,
certains passionnés ont entrevu les développements possibles d'une robotique personnelle.
C'est ainsi qu'est née la revue "Micros et Robots" dédiée à cette thématique. Sa vie fut brève, seulement 16 numéros
ont été publiés. En 1985, une série de trois articles sur le langage LISP sous la plume de JM Husson, commence au numéro 15. Quinze, seize, mais où est passé le troisième volet ? Cette revue était le rejeton d'une institution datant des
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
tels documents, si on les ouvre en tant que Google Doc.
L'IDE de choix pour le Pascal, de nos jours, est Lazarus. Sur Mac, j'ai essayé, mais c'est assez compliqué à faire fonctionner,
comme tous les outils en dehors de ceux d'Apple. Malgré les indications trouvées, malgré le certificat de signature du code,
je n'ai pas réussi à passer la  sécurité qui assure que le débugger gdb ou lldb soit utilisé de manière sûre. Je me suis
donc tourné vers Linux pour avancer plus rapidement.
Si on considère le fait que la plupart des implémentations de Lisp ont été écrites en Assembleurs divers, Pascal, c'est
presque de la triche ! Il existe des versions plus récentes, écrites en C, mais elles ont souvent une perspective
moderne et avec la connaissance actuelle des langages de programmation (provenant de C++/Java). La démarche est
différente lorsqu'on s'attache à suivre le chemin historique : Les S-Exp et leurs sept opérateurs quote, atom, eq, car,
cdr, cons et cond.  

## Le bilan de la restauration
Il a fallu faire des ajustements, l'interpréteur lit un fichier source conventionnel SOURCE.NLSP. On peut choisir le nom
que l'on veut puis faire un lien symbolique avant éxécution. La plupart des fonctions, notamment la définition de
fonction, sont opérationnelles.
Les conventions de nommage devraient être plus strictes: Les primitives sont nommées F suivi de la primitive : FCAR, FCDR
mais pour être exploitables, ces opérateurs devraient tous avoir un résultat de type S-Exp, ou SGRAPHE dans ce code.
FNULL ne respecte pas cette convention.
Petite note sur la lecture clavier, il faut 2 return pour conclure une saisie, à cause du test EOF qui réclame cette saisie supplémentaire.

## Et ensuite ?
- Implémenter l'arithmétique entière. Fait (en réel aussi) !
- Permettre la lecture à la console. Fait (la lecture de fichier aussi) !
- Fonctions mathématiques et trigo en degrés. Voir Exemples/test-maths.nlsp. Fait !
- Boucles. Possibles grâce à EVAL. Voir Exemples/test-boucle.nlsp
- Liste de propriétés (property list). Voir Exemples/test-prop.nslp.
- Porter en C simple. A faire !
- Permettre des commentaires pour documenter le code. A faire !
- Gestion plus grâcieuse des erreurs. A faire !

## Petit guide du langage
Cette version du langage est sensible à la casse, les mots-clés doivent être écrits en majuscules.
- Pas de notion de variables, ni de types, mais notion de symboles nommés, et de deux formes de valeurs:
 - Atome : valeur isolée,
   - Texte sans blanc précédé d'une seule apostrophe ou quote. En mémoire, une liste est utilisée car un atome nommé QUOTE est inséré devant le texte sans blanc, puis enlevé automatique lors de l'impression.
   - Nombre entier ou réel
   - Référence à un autre symbole
   - () est une valeur spéciale, ni atome, ni liste. On l'appelle NIL. C'est un élément neutre dans toute opération de composition. C'est aussi la valeur booléenne Faux.
   - T est la valeur booléenne Vrai.
 - Liste : plusieurs valeurs entre parenthèses.


- Tout code LISP est aussi une liste entre parenthèses. C'est la notion dite d'homoiconocité du langage.

- Création d'un symbole: (SETQ *nom* *valeur*)
  - (SETQ Ville 'Paris) L'apostrophe bloque l'évaluation, Paris est une valeur, et pas une référence à un autre symbole.
  - (SETQ LesNeveuxDeDonald '(Riri Fifi Loulou))
- Valeur d'un symbole
  - (Ville) répondra 'Paris.

- Exécution conditionnelle: (COND ((test) actions) ((test) actions)...)
  - Toutes les sous-listes dont le test est vrai sont évaluées, la valeur de retour est celle de la dernière sous-liste éxécutée. Un test est une S-EXP qui s'évalue à une liste vide pour Faux ou non-vide pour Vrai.

- Si on a affaire à un atome, alors on peut s'intéresser à son nom et à sa valeur.

- Si on a affaire à une liste, alors on peut s'intéresser à son premier élément, toujours un atome, nommé historiquement le CAR, et au reste de la liste, le CDR, qui est toujours une liste. Parfois, une fonction est écrite pour argument atome. Si on l'utilise avec une liste, il faut penser à en extraire le CAR, puis le CAR du CDR, etc..

- Construire une liste : (CONS *atome* *liste*) le constructeur de liste CONS est rarement nécessaire, les listes sont implicites grâce aux parenthèses. Il ne faut jamais ajouter de parenthèses en pensant priorité dans l'évaluation. La plupart du temps l'écriture la plus simple est la seule correcte, et c'est parfois une difficulté pour les débutants (dont je suis).

- Création d'une nouvelle fonction :
  - (DE *fonction*(*un seul argument atome ou liste*) (*Instructions*)) ou...
  - (DE *fonction*(*arg1* *arg2*...) (*Instructions*))

L'interprète crèe une liste LIFO pour "empiler" les valeurs des arguments pour permettre la récursion. C'est la fonction PAIRLIS du code Pascal.

Les nombres n'existent pas comme tels. A la création d'un atome, un test de "numéricité" est appliqué à son nom, et si le nom peut se traduire en valeur numérique, son nom est aussi sa valeur, le symbole est dit auto-évalué. En natif, 0, 1 font partie du "dictionnaire" initial de symboles. Les autres nombres sont créés à la demande, comme des symboles ordinaires, sauf qu'ils sont auto-evalués. Les chaînes de caractères ou toutes valeur "littérale" sont aussi auto-évaluées.

Le dossier Exemples contient les snippets ayant servi pendant le debug et l'élaboration des extensions.

## Comment l'interprète fonctionne
Le modèle mémoire : La mémoire est structurée comme une liste chaînée du langage hôte (Linked List), d'un type de base qui est la S-EXP simplement constituée d'une cellule "info" et d'un pointeur "suivant".
OBLIST --> [Info/Suivant] --> [Info/Suivant] --> null du langage hôte (NIL en Pascal, 0 en C). Le lien "Suivant" n'est manipulable que par le langage hôte.
"Info" contient un pointeur vers une cellule de mémoire de "donnée" qui est "typée":
[Type/Union de types occupant la même taille mémoire]
- Soit [ATOME/Nom/Valeur], "Nom" est une chaîne de caractères du langage hôte, "Valeur" un pointeur vers une autre S-EXP.
- Soit [LISTE/CAR/CDR], CAR et CDR sont tous les deux des pointeurs vers d'autres S-EXP.

Pour implémenter les propriétés, j'ai ajouté un pointeur vers une liste "privée" de S-EXP, représentant les propriétés:
[Propriétés/Type/Deux champs du type]
- L'interprète lit un atome jusqu'à un séparateur blanc, tabulation, ou saut à la ligne, puis le qualifie:
 - Est-ce un atome auto-évalué ? Si oui le créer en connaissance de sa particularité.
 - Est-ce une forme spéciale ? DE SETQ...qui va modifier l'environnement et la traiter comme il convient.
 - Les fonctions sont aussi des listes: Le CAR pointe vers un atome portant le nom. Le CDR contient deux liste, celle des arguments, celle de la définition. Pour dénoter que cette dernière est "éxécutable", un atome nommé LAMBDA est inséré en tête de la liste de définition de la fonction.
- Sinon, le reste de la liste de la commande est lu à la console ou d'un fichier.
 - Par défaut, après toutes les autres possibilités, le CAR est envisagé comme étant un nom de fonction et le CDR comme sa liste d'arguments. Le nom est remplacé par la liste commençant par LAMBDA, le tout est alors passé à la fonction APPLY(EVAL(*nom de fonction*), *arguments*), ce qui se traduit comme APPLY(*LAMBDA définition*, *Arguments*).

Le résultat est imprimé, puis on recommence. C'est exactement la formule désormais classique d'un interprète:
  - Lire ou *Read*
  - Evaluer ou *Evaluate*
  - Imprimer le résultat ou *Print*
  - Boucler ou *Loop*...

  C'est un *REPL*.

## Comment étendre le langage
Pour assurer leur composabilité, dirait-on de nos jours, "monadique", toutes les fonctions exposables dans le langage doivent accepter des S-EXP en entrée et en sortie, le type Pascal SGRAPHE, elles sont nommées F-suivi du nom exposé: FCAR FCDR FCONS, etc.. Puis le nom exposé doit être ajouté dans la liste des tests, soit de EVAL, soit de APPLY. La fonction INIT permet d'enrichir le "dictionnaire" initial. La commande (OBLIST) permet de le lister.

## En guise de conclusion
Parfois, il faut envisager la création d'un petit langage en oubliant ce que l'on sait de nos jours de la construction des compilateurs, l'analyse lexicale, la grammaire, la construction de l'arbre syntaxique, comme si le travail de Chomsky n'était pas encore connu... toute une science que  les pionniers de l'informatique ne pouvaient connaître. En repartant des bases historiques, on redécouvre une forme de simplicité et son génie, qui permet (et personne ne s'en prive) de recréer ce parcours historique dans n'importe quel langage moderne.
