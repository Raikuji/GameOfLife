object Main extends App {

    type Grille = List[(Int,Int)];

    /**
     * Methode permettant de transformer une liste de chaines en la structure de donnee Grille que l'on veut
     * @param l Liste de chaines que l'on souhaite transformer
     * @return la Grille
     */
    def chaineToGrille(l: List[String]): Grille = {
        @scala.annotation.tailrec
        def go1(l: List[String], g: Grille, row: Int): Grille = (l, row) match {
            case (l, r) if l.isEmpty => g
            case (head::tail, r) => go1(tail, go2(head.split("").toList, g, r, 0), r+1)
        }
        @scala.annotation.tailrec
        def go2(str: List[String], g: Grille, row: Int, column: Int): List[(Int, Int)] = (str, row, column) match {
            case (s, _, c) if s.isEmpty => g
            case (head::tail, r, c) if head.equals("X") => go2(tail, g :+ (r, c), r, c + 1)
            case (_::tail, r, c) => go2(tail, g, r, c + 1)
        }
        go1(l, Nil, 0)
    }

    assert(chaineToGrille(List(" XX", "  X", "XXX")) == List((0,1), (0,2), (1,2), (2,0), (2,1), (2,2)))

    /**
     * Methode permettant l'affichage de la Grille
     * @param g la Grille a afficher
     */
    def afficherGrille(g: Grille): Unit = {
        def min(x: (Int, Int), y: (Int, Int)): (Int, Int) = (x, y) match {
            case ((a, b), (c, d)) if(a >= c && b >= d) => (c, d)
            case ((a, b), (c, _)) if(a >= c) => (c, b)
            case ((a, b), (_, d)) if(b >= d) => (a, d)
            case ((a, b), (_, _)) => (a, b)
        }
        def max(x: (Int, Int), y: (Int, Int)): (Int, Int) = (x, y) match {
            case ((a, b), (c, d)) if(a <= c && b <= d) => (c, d)
            case ((a, b), (c, _)) if(a <= c) => (c, b)
            case ((a, b), (_, d)) if(b <= d) => (a, d)
            case ((a, b), (_, _)) => (a, b)
        }
        if(g != Nil) {
            val maxCol = g.reduceLeft(max)._2
            val minCol = g.reduceLeft(min)._2

            @scala.annotation.tailrec
            def go(g: Grille, index: (Int, Int)): Unit = (g, index) match {
                case (Nil, _) => print("\n")
                case (g, (a, b)) if b > maxCol => print("\n")
                    go(g, (a + 1, minCol))
                case (head :: tail, (a, b)) if head == (a, b) => print("X")
                    go(tail, (a, b + 1))
                case (g, (a, b)) => print(" ")
                    go(g, (a, b + 1))
            }

            go(g, g.reduceLeft(min))
        }
    }

    //afficherGrille(List((-1,1), (0,1), (1,2), (2,0), (2,1)))

    /**
     * Methode retournant 8 voisines d'une case
     * @param l ligne sur laquelle est situee la case
     * @param c colonne sur laquelle est situee la case
     * @return liste de toutes les voisines de la case
     */
    def voisines8(l: Int, c :Int): List[(Int, Int)] = List((l-1, c-1), (l-1, c), (l-1, c+1), (l, c-1), (l, c+1), (l+1, c-1), (l+1, c), (l+1, c+1))

    assert(voisines8(0, 0) == List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)))

    /**
     * Methode retournant le nombre de case viavante d'une case donnee
     * @param g la Grille a analyser
     * @param neighbors liste des voisines de la case
     * @param n nombre de voisines deja reperees
     * @return nombre de voisines reperees
     */
    @scala.annotation.tailrec
    def nNeighbors(g: Grille, neighbors: List[(Int, Int)], n: Int): Int = neighbors match {
        case Nil => n
        case head::tail if g contains head => nNeighbors(g, tail, n+1)
        case _::tail => nNeighbors(g, tail, n)
    }

    /**
     * Methode permettant de savoir quelle cellule survit d'une etape a une autre
     * @param g la grille des cases a analyser
     * @return la grille des cases survivantes
     */
    def survivantes(g: Grille): Grille = {
        def go(grid: Grille, newGrid: Grille): Grille = grid match {
            case Nil => newGrid
            case head::tail if 2 to 3 contains nNeighbors(g, voisines8(head._1, head._2), 0) => go(tail, newGrid :+ head)
            case _::tail => go(tail, newGrid)
        }
        go(g, Nil)
    }

    assert(survivantes(List((-1,1), (0,1), (1,2), (2,0), (2,1))) == List((0,1), (1, 2), (2, 1)))

    /**
     * Methode permettant de fusionner 2 listes, supprimer les doublons de la liste et trier pour l'affichage
     * @param g1 premiere grille a traiter
     * @param g2 seconde grille a traiter
     * @return la grille traitee
     */
    def reduce(g1: Grille, g2: Grille): Grille = {
        @scala.annotation.tailrec
        def go(g1: Grille, g2: Grille, newGrid: Grille): Grille = (g1, g2) match {
            case (Nil, Nil) => newGrid
            case (Nil, g) => newGrid ++ g
            case (g, Nil) => newGrid ++ g
            case ((a, b)::t1, (c, d)::t2) if a < c => go(t1, (c, d)::t2, newGrid :+ (a, b))
            case ((a, b)::t1, (c, d)::t2) if a > c => go((a, b)::t1, t2, newGrid :+ (c, d))
            case ((a, b)::t1, (c, d)::t2) if b < d => go(t1, (c, d)::t2, newGrid :+ (a, b))
            case ((a, b)::t1, (c, d)::t2) if b > d => go((a, b)::t1, t2, newGrid :+ (c, d))
            case (h1::t1, h2::t2) => go(t1, t2, newGrid :+ h1)
        }
        @scala.annotation.tailrec
        def go2(g: Grille, newGrid: Grille): Grille = g match {
            case Nil => newGrid.sorted
            case head::tail if !(newGrid contains head) => go2(tail, newGrid :+ head)
            case _::tail => go2(tail, newGrid)
        }
        if(g2 == Nil) go2(g1, Nil)
        else go2(go(g1, g2, Nil), Nil)
    }

    /**
     * Methode permettant de savoir quelle cellules sont susceptible venir a la vie au prochain tour
     * @param g la grille a analyser
     * @return la grille des candidates
     */
    def candidates(g: Grille): Grille = {
        @scala.annotation.tailrec
        def go(g: Grille, newGrid: Grille): Grille = g match {
            case Nil => newGrid
            case head::tail => go(tail, newGrid ++ voisines8(head._1, head._2))
        }
        reduce(go(g, Nil),Nil)
    }

    assert(candidates(List((0, 0))) == List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)))

    /**
     * Methode donnant la liste des cellules naissantes d'un tour a l'autre
     * @param g la grille a analyser
     * @return la grille des cellules naissantes
     */
    def naissances(g: Grille): Grille = {
        @scala.annotation.tailrec
        def go(c: Grille, newGrid: Grille): Grille = c match {
            case Nil => newGrid
            case head::tail if nNeighbors(g, voisines8(head._1, head._2), 0) == 3 => go(tail, newGrid :+ head)
            case head::tail => go(tail, newGrid)
        }
        go(candidates(g), Nil)
    }

    assert(naissances(List((-1,1), (0,1), (1,2), (2,0), (2,1))) == List((0, 2),(1, 0)))

    /**
     * Methode permattant le jeu de la vie, sans possiblite de changer les regles
     * @param init la grille initiale
     * @param n nombre d'iteration a faire tourner
     */
    def jeuDeLaVie(init:Grille, n:Int):Unit = {
        @scala.annotation.tailrec
        def go(g: Grille, step: Int): Unit = step match {
            case s if s < n => afficherGrille(g)
                println("\nEtape " + n + " : \n")
                                 go(reduce(survivantes(g), naissances(g)),s + 1)
            case _ => afficherGrille(g)
        }
        go(init, 0)
    }

    val config = List("XXX    ",
                      "  X XX ",
                      "     X ")

    //jeuDeLaVie(chaineToGrille(config), 8)

    /**
     * Methode retourant la liste des 4 voisines en contact d'une case
     * @param l ligne de la case
     * @param c colonne de la case
     * @return la liste des voisines de la case
     */
    def voisines4(l: Int, c: Int): List[(Int, Int)] = {
        List((l-1, c),(l, c-1), (l, c+1), (l+1, c))
    }

    assert(voisines4(0, 0) == List((-1, 0), (0, -1), (0, 1), (1, 0)))

    /**
     * Methode retournant si une cellule nait selon son nombre de voisine pour le jeu de la vie
     * @param nbVoisines nombre de voisine de la case
     * @return si la cellule nait ou pas
     */
    def naitJDLV(nbVoisines: Int): Boolean = nbVoisines == 3

    assert(naitJDLV(3))
    assert(!naitJDLV(2))

    /**
     * Methode retournant si une cellule meurt selon son nombre de voisine pour le jeu de la vie
     * @param nbVoisines nombre de voisine de la case
     * @return si la cellule nait ou pas
     */
    def meurtJDLV(nbVoisines: Int): Boolean = 2 to 3 contains nbVoisines

    assert(meurtJDLV(2) && meurtJDLV(3))
    assert(!meurtJDLV(1) && !meurtJDLV(4))

    /**
     * Methode retournant si une cellule nait selon son nombre de voisine pour l'algorithme de freuddkin
     * @param nbVoisines nombre de voisine de la case
     * @return si la cellule nait ou pas
     */
    def naitAF(nbVoisines: Int): Boolean = nbVoisines % 2 == 1

    assert(naitAF(3))
    assert(!naitAF(2))

    /**
     * Methode retournant si une cellule meurt selon son nombre de voisine pour l'algorithme de freudkin
     * @param nbVoisines nombre de voisine de la case
     * @return si la cellule nait ou pas
     */
    def meurtAF(nbVoisines: Int): Boolean = nbVoisines % 2 == 0

    assert(meurtAF(4))
    assert(!meurtAF(3))

    /**
     * Methode generale permettant de savoir quelle cellule survit d'une etape a une autre
     * @param g la grille des cases a analyser
     * @param f1 fonction permettant de savoir quelle type de voisinage prendre en compte
     * @param f2 fonction permettant de determiner quelles cellules survivront au prochain tour
     * @return la grille des cases survivantes
     */
    def survivantesG(g: Grille, f1: (Int, Int) => List[(Int, Int)], f2: Int => Boolean): Grille = {
        def go(grid: Grille, newGrid: Grille): Grille = grid match {
            case Nil => newGrid
            case head :: tail if f2(nNeighbors(g, f1(head._1, head._2), 0)) => go(tail, newGrid :+ head)
            case _ :: tail => go(tail, newGrid)
        }

        go(g, Nil)
    }

    assert(survivantesG(List((-1,1), (0,1), (1,2), (2,0), (2,1)), voisines8, meurtJDLV) == List((0,1), (1, 2), (2, 1)))

    /**
     * Methode generale permettant de savoir quelle cellules sont susceptible venir a la vie au prochain tour
     * @param g la grille a analyser
     * @param f fonction permettant de savoir quelle type de voisinage prendre en compte
     * @return la grille des candidates
     */
    def candidatesG(g: Grille, f: (Int, Int) => List[(Int, Int)]): Grille = {
        @scala.annotation.tailrec
        def go(g: Grille, newGrid: Grille): Grille = g match {
            case Nil => newGrid
            case head::tail => go(tail, newGrid ++ f(head._1, head._2))
        }
        reduce(go(g, Nil),Nil)
    }

    assert(candidatesG(List((0, 0)), voisines8) == List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)))

    /**
     * Methode generale donnant la liste des cellules naissantes d'un tour a l'autre
     * @param g la grille a analyser
     * @param f1 fonction permettant de savoir quelle type de voisinage prendre en compte
     * @param f2 fonction permettant de determiner quelles cellules naitront au prochain tour
     * @return la grille des cellules naissantes
     */
    def naissancesG(g: Grille, f1: (Int, Int) => List[(Int, Int)], f2: Int => Boolean): Grille = {
        @scala.annotation.tailrec
        def go(c: Grille, newGrid: Grille): Grille = c match {
            case Nil => newGrid
            case head::tail if f2(nNeighbors(g, f1(head._1, head._2), 0)) => go(tail, newGrid :+ head)
            case head::tail => go(tail, newGrid)
        }
        go(candidatesG(g, f1), Nil)
    }

    assert(naissancesG(List((-1,1), (0,1), (1,2), (2,0), (2,1)), voisines8, naitJDLV) == List((0, 2),(1, 0)))

    /**
     * Moteur de jeu pouvant prendre en compte differentes lois
     * @param init la grille d'initalisation
     * @param n le nombre d'iteration
     * @param f1 fonction permettant de savoir quelle type de voisinage prendre en compte
     * @param f2 fonction permettant de determiner quelles cellules survivront au prochain tour
     * @param f3 fonction permettant de determiner quelles cellules naitront au prochain tour
     */
    def moteur(init:Grille, n:Int, f1: (Int, Int) => List[(Int, Int)], f2: Int => Boolean, f3: Int => Boolean):Unit = {
        @scala.annotation.tailrec
        def go(g: Grille, step: Int): Unit = step match {
            case s if s < n => afficherGrille(g)
                               println("\nEtape " + (s + 1) + " : \n")
                               go(reduce(survivantesG(g, f1, f2), naissancesG(g, f1, f3)),s + 1)
            case _ => afficherGrille(g)
        }
        go(init, 0)
    }

    //moteur(chaineToGrille(List("X ", " X")), 20, voisines8, meurtJDLV, naitJDLV)

    /**
     * Methode permettant le lancement du jeu de la vie
     * @param init la grille initiale
     * @param n le nombre d'iteration
     */
    def gameOfLife(init: Grille, n: Int): Unit = {
        moteur(init, n, voisines8, meurtJDLV, naitJDLV)
    }

    /**
     * Methode permettant le lancement de l'automate de freudkin
     * @param init la grille initiale
     * @param n le nombre d'iteration
     */
    def freudkinAutomata(init: Grille, n: Int): Unit = {
        moteur(init, n, voisines4, meurtAF, naitAF)
    }

    /**
     * Methode retourant la liste des 4 voisines en diagonale d'une case
     * @param l ligne de la case
     * @param c colonne de la case
     * @return la liste des voisines de la case
     */
    def voisineDiago4(l: Int, c: Int): Grille = {
        List((l-1, c-1), (l-1, c+1), (l+1, c-1), (l+1, c+1))
    }

    /**
     * Methode permettant le lancement de la variante de l'automate de freudkin
     * @param init la grille initiale
     * @param n le nombre d'iteration
     */
    def variantFreudkinAutomata(init: Grille, n: Int): Unit = {
        moteur(init, n, voisineDiago4, meurtAF, naitAF)
    }

    //gameOfLife(chaineToGrille(List(" X X  X", "X X XX", " X  XX")), 50)

    /**
     * Methode qui permet de selectionner les parametres du jeu
     */
    def start(): Unit = {
        println("Quel type de jeu voulez vous lancer :")
        println("   - 1 : Jeu de la vie")
        println("   - 2 : Automate de Freudkin")
        println("   - 3 : Variant de l'automate de Freudkin")
        val jeu = scala.io.StdIn.readInt()
        println("Combien de lignes souhaitez vous pour l'initialisation ?")
        val lignes = scala.io.StdIn.readInt()
        println("Combien de colonnes souhaitez vous pour l'initialisation ?")
        val colonnes = scala.io.StdIn.readInt()
        def go(l: Int, g: List[String]): List[String] = l match {
            case l if l < lignes => println("Entrez la ligne " + l)
                                    val ligne = scala.io.StdIn.readLine()
                                    if(ligne.length != colonnes) go(l, g)
                                    else go(l+1, g :+ ligne)
            case l if l == lignes => println("Entrez la ligne " + l)
                                     val ligne = scala.io.StdIn.readLine()
                                     if(ligne.length != colonnes) go(l, g)
                                     else g :+ ligne
        }
        def launch(t: Int, n: Int) = t match {
            case 0 => println(chaineToGrille(go(1, Nil)))
            case 1 => gameOfLife(chaineToGrille(go(1, Nil)), n)
            case 2 => freudkinAutomata(chaineToGrille(go(1, Nil)), n)
            case 3 => variantFreudkinAutomata(chaineToGrille(go(1, Nil)), n)
        }
        println("Entrez le nombre d'iteration que vous souhaitez faire :")
        val iter = scala.io.StdIn.readInt()
        launch(jeu, iter)
    }
    start()
}
