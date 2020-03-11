object Main extends App {

    type Grille = List[(Int,Int)];

    def chaineToGrille(l: List[String]): Grille = {
        def go1(l: List[String], g: Grille, row: Int): Grille = (l, row) match {
            case (l, r) if l.isEmpty => g
            case (head::tail, r) => go1(tail, go2(head.split("").toList, g, r, 0), r+1)
        }
        def go2(str: List[String], g: Grille, row: Int, column: Int): List[(Int, Int)] = (str, row, column) match {
            case (s, _, c) if s.isEmpty => g
            case (head::tail, r, c) if head.equals("X") => go2(tail, g :+ (r, c), r, c + 1)
            case (_::tail, r, c) => go2(tail, g, r, c + 1)
        }
        go1(l, Nil, 0)
    }

    assert(chaineToGrille(List(" XX", "  X", "XXX")) == List((0,1), (0,2), (1,2), (2,0), (2,1), (2,2)))

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
        val maxCol = g.reduceLeft(max)._2
        val minCol = g.reduceLeft(min)._2
        def go(g:Grille, index: (Int, Int)): Unit = (g, index) match {
            case (Nil, _) => print("\n")
            case (g, (a, b)) if b > maxCol => print("\n")
                                              go(g, (a + 1, minCol))
            case (head::tail, (a, b)) if head == (a, b) => print("X")
                                                           go(tail, (a, b + 1))
            case (g, (a, b)) => print(" ")
                                go(g, (a, b + 1))
        }
        go(g, g.reduceLeft(min))
    }

    //afficherGrille(List((-1,1), (0,1), (1,2), (2,0), (2,1)))

    def voisines8(l: Int, c :Int): List[(Int, Int)] = List((l-1, c-1), (l-1, c), (l-1, c+1), (l, c-1), (l, c+1), (l+1, c-1), (l+1, c), (l+1, c+1))

    assert(voisines8(0, 0) == List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)))

    def nNeighbors(g: Grille, neighbors: List[(Int, Int)], n: Int): Int = neighbors match {
        case Nil => n
        case head::tail if g contains head => nNeighbors(g, tail, n+1)
        case _::tail => nNeighbors(g, tail, n)
    }

    def survivantes(g: Grille): Grille = {
        def go(grid: Grille, newGrid: Grille): Grille = grid match {
            case Nil => newGrid
            case head::tail if 2 to 3 contains nNeighbors(g, voisines8(head._1, head._2), 0) => go(tail, newGrid :+ head)
            case _::tail => go(tail, newGrid)
        }
        go(g, Nil)
    }

    assert(survivantes(List((-1,1), (0,1), (1,2), (2,0), (2,1))) == List((0,1), (1, 2), (2, 1)))

    def reduce(g1: Grille, g2: Grille): Grille = {
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
        def go2(g: Grille, newGrid: Grille): Grille = g match {
            case Nil => newGrid
            case head::tail if !(newGrid contains head) => go2(tail, newGrid :+ head)
            case _::tail => go2(tail, newGrid)
        }
        if(g2 == Nil) go2(g1, Nil)
        else go2(go(g1, g2, Nil), Nil)
    }

    def sort(g: Grille): Grille = {
        g.sorted
    }

    def candidates(g: Grille): Grille = {
        def go(g: Grille, newGrid: Grille): Grille = g match {
            case Nil => newGrid
            case head::tail => go(tail, newGrid ++ voisines8(head._1, head._2))
        }
        reduce(go(g, Nil),Nil)
    }

    assert(candidates(List((0, 0))) == List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)))

    def naissances(g: Grille): Grille = {
        def go(c: Grille, newGrid: Grille): Grille = c match {
            case Nil => newGrid
            case head::tail if nNeighbors(g, voisines8(head._1, head._2), 0) == 3 => go(tail, newGrid :+ head)
            case head::tail => go(tail, newGrid)
        }
        go(candidates(g), Nil)
    }

    assert(naissances(List((-1,1), (0,1), (1,2), (2,0), (2,1))) == List((0, 2),(1, 0)))

    def jeuDeLaVie(init:Grille, n:Int):Unit = {
        def go(g: Grille, step: Int): Unit = step match {
            case s if s < n => afficherGrille(g)
                                 println("----------------------------------")
                                 go(reduce(survivantes(g), naissances(g)),s + 1)
            case _ => afficherGrille(g)
        }
        go(init, 0)
    }

    val config = List("XXX    ",
                      "  X XX ",
                      "     X ")

    //jeuDeLaVie(chaineToGrille(config), 8)

    def voisines4(l: Int, c: Int): List[(Int, Int)] = {
        List((l-1, c),(l, c-1), (l, c+1), (l+1, c))
    }

    def naitJDLV(nbVoisines: Int): Boolean = nbVoisines == 3

    def meurtJDLV(nbVoisines: Int): Boolean = 2 to 3 contains nbVoisines

    def naitAF(nbVoisines: Int): Boolean = nbVoisines % 2 == 1

    def meurtAF(nbVoisines: Int): Boolean = nbVoisines % 2 == 0

    def survivantesG(g: Grille, f1: (Int, Int) => List[(Int, Int)], f2: Int => Boolean): Grille = {
        def go(grid: Grille, newGrid: Grille): Grille = grid match {
            case Nil => newGrid
            case head :: tail if f2(nNeighbors(g, f1(head._1, head._2), 0)) => go(tail, newGrid :+ head)
            case _ :: tail => go(tail, newGrid)
        }

        go(g, Nil)
    }

    assert(survivantesG(List((-1,1), (0,1), (1,2), (2,0), (2,1)), voisines8, meurtJDLV) == List((0,1), (1, 2), (2, 1)))

    def candidatesG(g: Grille, f: (Int, Int) => List[(Int, Int)]): Grille = {
        def go(g: Grille, newGrid: Grille): Grille = g match {
            case Nil => newGrid
            case head::tail => go(tail, newGrid ++ f(head._1, head._2))
        }
        sort(reduce(go(g, Nil),Nil))
    }

    assert(candidatesG(List((0, 0)), voisines8) == List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)))

    def naissancesG(g: Grille, f1: (Int, Int) => List[(Int, Int)], f2: Int => Boolean): Grille = {
        def go(c: Grille, newGrid: Grille): Grille = c match {
            case Nil => newGrid
            case head::tail if f2(nNeighbors(g, f1(head._1, head._2), 0)) => go(tail, newGrid :+ head)
            case head::tail => go(tail, newGrid)
        }
        go(candidatesG(g, f1), Nil)
    }

    assert(naissancesG(List((-1,1), (0,1), (1,2), (2,0), (2,1)), voisines8, naitJDLV) == List((0, 2),(1, 0)))

    def moteur(init:Grille, n:Int, f1: (Int, Int) => List[(Int, Int)], f2: Int => Boolean, f3: Int => Boolean):Unit = {
        def go(g: Grille, step: Int): Unit = step match {
            case s if s < n => afficherGrille(g)
                println("----------------------------------")
                go(sort(reduce(survivantesG(g, f1, f2), naissancesG(g, f1, f3))),s + 1)
            case _ => afficherGrille(g)
        }
        go(init, 0)
    }

    //moteur(chaineToGrille(List("XX")), 20, voisines4, meurtAF, naitAF)

    def gameOfLife(init: Grille, n: Int): Unit = {
        moteur(init, n, voisines8, meurtJDLV, naitJDLV)
    }

    def freudkinAutomata(init: Grille, n: Int): Unit = {
        moteur(init, n, voisines4, meurtAF, naitAF)
    }

    def voisineDiago4(x: Int, y: Int): Grille = {
        List((x-1, y-1), (x-1, y+1), (x+1, y-1), (x+1, y+1))
    }

    def variantFreudkinAutomata(init: Grille, n: Int) = {
        moteur(init, n, voisineDiago4, meurtAF, naitAF)
    }

    variantFreudkinAutomata(chaineToGrille(List("X, X")), 20)
}
