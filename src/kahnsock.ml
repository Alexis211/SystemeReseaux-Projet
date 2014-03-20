Random.self_init ()

type ident = (int * int * int * int)
let gen_ident () =
    Random.int 1000000000, Random.int 1000000000,
    Random.int 1000000000, Random.int 1000000000

module Sock : Kahn.S = struct

    (* L'idée :
        - L'ensemble des noeuds qui font du calcul est un arbre.
          Le premier noeud lancé est la racine de l'arbre ; tous les
          noeuds qui se connectent par la suite se connectent à un
          noeud déjà présent et sont donc son fils.
        - Les processus sont des fermetures de type unit -> unit,
          transmises par des canaux
        - Un noeud de calcul est un processus ocaml avec un seul
          thread. Le parallélisme est coopératif (penser à faire
          des binds assez souvent).
        - Les noeuds publient régulièrement leur load, ie le nombre
          de processus en attente et qui ne sont pas en train
          d'attendre des données depuis un canal. Si un noeud a un
          voisin dont le load est plus faible que le sien d'une
          quantité plus grande que 2, il délègue une tâche.
        - Le noeud racine délègue toutes ses tâches et sert uniquement
          pour les entrées-sorties
        - Comportement indéterminé lorsqu'un noeud se déconnecte
          (des processus peuvent disparaître, le réseau est cassé...)
        - Les canaux sont identifiés par le type ident décrit
          ci-dessus. Lorsque quelqu'un écrit sur un canal, tout le
          monde le sait. Lorsque quelqu'un lit sur un canal, tout le
          monde le sait. (on n'est pas capable de déterminer
          quel est le noeud propriétaire du processus devant lire
          le message) Les communications sont donc coûteuses.
    *)

    type 'a process = (unit -> 'a)

    type 'a in_port = ident
    type 'a out_port = ident

    let cin = (0, 0, 0, 0)
    let cout = (0, 0, 0, 1)

end
