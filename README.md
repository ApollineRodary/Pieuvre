# Pieuvre by Théa et Apolline

**Pieuvre** est un assistant de preuve minimal inspiré par Rocq, implémenté dans le cadre du cours de Projet Fonctionnel de L3 à l'Ecole Normale Supérieure de Lyon. Cette implémentation est proposée par Thibault Hervier (également de l'ENS) et moi-même. Au fur et à mesure que l'utilisateur progresse dans la preuve en cours, Pieuvre construit un [lambda-terme typé](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence) correspondant à la preuve.

## Fonctionnalités

Le projet se compose de deux exécutables, `toplevel` (un REPL élémentaire qui lit, évalue et affiche des lambda-termes typés rentrés par l'utilisateur) et `pieuvre`, l'assistant de preuve lui-même. Par défaut, Pieuvre lit une propriété à prouver depuis l'entrée standard, puis une suite de directives ou de tactiques correspondant aux règles de dérivation de la logique intuitionniste. Alternativement, un fichier contenant une propriété à prouver ainsi que les tactiques nécessaires pour prouver cette propriété peut être donné en argument. Dans tous les cas, Pieuvre engendre un fichier preuve (`proof.8pus`) contenant les étapes de la preuve, et un fichier `proof.lam` comprenant le lambda-terme généré.

### Propriétés

Les propriétés énonçables en Pieuvre sont sans quantifieurs. On autorise des propositions (`A`, `B`, `A0`...), des implications, des disjonctions, des conjonctions et l'utilisation du faux (`False`) et du vrai (`I`). On a également la négation : `~A` est un raccourci pour `A -> False`.

On peut par exemple énoncer (et prouver) les formules suivantes :
```coq
A -> (A -> B) -> B
(A \/ B) /\ ~C -> (A /\ ~C) \/ (B /\ ~C)
```

### Tactiques

- `intro`, `intros` pour l'introduction de l'implication
- `exf` pour le ex-falso
- `assumption` pour l'axiome
- `apply` pour l'élimination de la flèche ou pour l'axiome
- `left`, `right` pour l'introduction du OU
- `split` pour l'introduction du ET
- `elim` pour l'élimination du OU ou du ET
- `exact` permet de renseigner le lambda-terme typé correspondant directement au sous-but courant
- `admit` permet d'admettre le sous-but courant sans preuve

### Directives

- `Qed` pour conclure la preuve lorsqu'il ne reste aucun but à prouver
- `Admitted` pour conclure la preuve lorsqu'il ne reste aucun but à prouver, et si l'un des buts a été admis
- `Print` pour afficher le lambda-terme typé (éventuellement lacunaire) correspondant à l'état actuel de la preuve

### Autres options

Pieuvre peut également être lancé avec d'autres options pour tester quelques fonctions implémentées portant sur le lambda-calcul plutôt que de lancer une session interactive de preuve. Les options sont les suivantes :

- `-alpha` : Vérifie si deux lambda-termes sont alpha-convertibles.
- `-reduce` : Réduit un lambda-terme jusqu'à tomber sur sa forme normale. Si celle-ci n'existe pas, le programme boucle infiniement. Le typage n'est pas vérifié.
- `typecheck` : Vérifie si un lambda-terme donné a bien le type donné.

## Utilisation

Pieuvre utilise [Dune 3.7](https://dune.build).
Pour construire et exécuter Pieuvre :
```sh
$ dune build
$ dune exec Pieuvre
```

Pour utiliser des arguments en lançant Pieuvre, utiliser :
```sh
$ dune exec -- Pieuvre [args]
```

Par exemple :
```sh
$ dune exec -- Pieuvre
$ dune exec -- Pieuvre mapreuve.8pus
$ dune exec -- Pieuvre -alpha
```

### Exemple de preuve

```coq
((A \/ B) /\ C) -> (A /\ C) \/ (B /\ C).
intro h.
elim h.
intros h1 c.
elim h1.
intro a.
left.
split.
assumption.
assumption.
intro b.
right.
split.
assumption.
assumption.
Qed.
```