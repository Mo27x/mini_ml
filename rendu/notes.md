> memo:
>
> - pas de code dans le pdf du rendu
> - il manque probablement des questions

---

## réponses aux questions de chaque partie du projet

### 3.1 Parseur simple : compréhension

> 1. Donnez l'automate LR0 associé à la grammaire ci-dessus (on considèrera
>    pour cette question que built_in, INT et ID sont un seul terminal (qu'on
>    notera T), car le phénomène à observer ne les concerne pas vraiment).
>    Vous pouvez évidemment vous servir de menhir pour vérifier que vous avez le
>    bon automate. La grammaire simplifiée est donc la suivante (il faut évidemment
>    ajouter le non-terminal initial technique expr′ pour avoir un résultat
>    similaire à menhir)

> 2. Où sont les conflits sur cet automate, si on le considère comme un automate
>    SLR ?

> 3. Donnez un exemple de séquence de tokens sur laquelle deux arbres de
>    dérivations sont possibles avec cet automate. Qu'en déduisez-vous sur cette
>    grammaire naturelle ?

> 4. Quel est le choix fait par le parseur implémenté dans Parser calc.mly
>    sur votre séquence de tokens ?

> 5. Quelles priorités peut-on ajouter à la grammaire ci-dessus pour retrouver
>    le comportement de Parser calc.mly (utilisez menhir pour vérifier que vous
>    avez la bonne réponse).

> 6. Si on ajoute toutes les fonctions built_in dans le parseur, qu'est-ce que
>    cela donne en terme du nombre de priorités à écrire.

> 7. A votre avis, pourquoi donc utilisons-nous plutôt des non-terminaux
>    distincts dans ce cas plutôt que des priorités ?

### 3.2 Syntaxe étendue

> Vous expliquerez dans le document de réponses pour chaque point la solution
> que vous avez adopté pour le traiter, ainsi que les éventuelles difficultés
> rencontrées (i.e., qu'est-ce qui ne fonctionne pas correctement). Si vous
> n'arrivez pas à traiter un des points, fournissez un exemple de programme
> étant correctement parsé avec notre version et pas avec la vôtre.

> Evidemment, nous n'avons listé que les priorités des opérateurs. Vous
> expliquerez comment elles se comparent aux priorités déjà présentes dans le
> parseur simple et pourquoi c'est cette solution qui est la bonne.

### 4.1 Typage naïf et génération des contraintes

> **[DONE?]**
> Implémentez la fonction type_of_built_in du fichier typer util.ml (attention à correctement attribuer les types universels génériques).

> **[DONE?]**
> Implémentez la fonction type_expr du fichier typer naive.ml.

> **[WIP]**
> Donnez 3 (ou plus) petits programmes mini-ml qui illustrent le fonctionnement
> de votre typeur naïf (toutes les sous-expressions doivent être représentées).
> Vous placerez ces programmes dans examples/answers, et expliquerez ce qu'ils
> illustrent (en commentaire de ces programmes, et dans votre rapport).

> Choisissez l'un de ces programmes, et illustrez le fonctionnement du typeur
> sur celui-ci sur papier. Dessinez l'arbre, et pour chaque nœud, donnez son
> type et dites quelles contraintes ce nœud introduit (évidemment, choisissez un
> exemples où il y a des contraintes).

> Si vous avez une différence entre votre typeur et le nôtre sur l'un de vos
> programmes ou l'un des nôtres, décrivez cette différence, et expliquez d'où
> vous pensez qu'elle vient.

### 4.2 Résolution des contraintes et polymorphisme faible

> Dans les exemples de la question précédente, lesquels sont typés correctement,
> lesquels ne le sont pas ? Expliquez pourquoi.

> Observez que l'exemple donné en début de chapitre n'est pas typé. Donnez les
> parties typées, et expliquez où se situe l'erreur.

> Proposez un autre programme qui n'est pas correctement typé alors que OCaml le
> type, et expliquez pourquoi. Vous le placerez dans examples/answers.

### 4.3 Typage fortement polymorphe

> Illustrez le fonctionnement du typage polymorphe en fournissant deux exemples
> supplémentaires qui typent différemment avec les deux algorithmes de typage.
> Choisissez-en un dont vous expliquerez soigneusement où se situe la différence
> (i.e., décrivez l'application des deux algorithmes à cet exemple). Vous les
> placerez dans examples/answers.

> Si vous avez des différence entre votre implémentation et le comportement de
> l'outil qui vous est fourni, décrivez-les, et dites d'où vous pensez qu'elles
> viennent.

### 5 Extensions

---

## «difficultés rencontrées s'il y en a»

---

## «Qui sont les auteurs du projet en détaillant qui a fait quoi».
