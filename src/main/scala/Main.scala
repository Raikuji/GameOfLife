object Main extends App {

    type Grille = List[(Int,Int)];

    def chaineToGrille(l: List[String]): Grille = {
        def go1(l: List[String], fl: List[(Int, Int)], row: Int): List[(Int, Int)] = {
            def go2(l: List[String], fl: List[(Int, Int)], row: Int, column: Int): List[(Int, Int)] = {
                if(column >= l.length) {
                    fl
                } else if(l(column).equals("X")) {
                    go2(l, fl :+ (row, column), row, column + 1)
                } else {
                    go2(l, fl, row, column + 1)
                }
            }
            if(row >= l.length) {
                fl
            } else {
                go1(l, go2(l(row).split("").toList, fl, row, 0), row+1)
            }
        }
        go1(l, List(), 0)
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

    def voisines8(l: Int, c :Int): List[(Int, Int)] = {
        List((l-1, c-1), (l-1, c), (l-1, c+1), (l, c-1), (l, c+1), (l+1, c-1), (l+1, c), (l+1, c+1))
    }

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
    jeuDeLaVie(chaineToGrille(config), 8)
}
