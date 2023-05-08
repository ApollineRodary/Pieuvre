# Pieuvre by Thibault et Apolline

## Fonctionnalités

Pieuvre a les options suivantes :
- `-alpha` : Vérifie si deux lambda termes sont alpha convertibles.
- `-reduce` : Réduit un lambda terme jusqu'à tomber sur sa forme normale. Si celle-çi n'existe pas, le programme boucle infiniement. Le typage n'est pas vérifié.
- `typecheck` : Vérifie si un lambda terme donné a bien le type donné.
- Pas d'argument : Affiche le lambda terme donné (le parenthèsage peut être différent, mais le terme sera le même)

## Partage du travail

TBD

## Utilisation

Notre implémentation de Pieuvre utilise Dune 3.7. Pour construire et exécuter Pieuvre :
```sh
dune build
dune exec Pieuvre
```

Pour utiliser des arguments en lançant Pieuvre, utiliser :
```sh
dune exec -- Pieuvre [args]
```
**Exemple :**
```sh
dune exec -- Pieuvre -alpha
```
