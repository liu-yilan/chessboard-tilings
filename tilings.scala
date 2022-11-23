import scala.io.Source

enum State: 
    case Free 
    case Pebble 

type Step = (State, Int, Int) 

type Shape = List[Step] 

def putDomino(steps: Shape): List[Shape] = 
    (steps match
        case Nil => Nil 
        case head :: rem_steps => 
            (head match 
                case (State.Free, v, h) => 
                    if((v >= 2) && (h >= 2)){
                        List(horizontal(v, h, rem_steps), vertical(v, h, rem_steps))
                    }
                    else if((v >= 2) && (h == 1)){
                        List(vertical(v, h, rem_steps)) ++ 
                        putDomino(rem_steps).map(cons(State.Pebble, v, h))
                    }
                    else if((v == 1) && (h >= 2)){
                        List(horizontal(v, h, rem_steps))
                    }
                    else{
                        putDomino(rem_steps).map(cons(State.Free, v, h))
                    }
                case (State.Pebble, v, h) => 
                    if((v >= 1) && (h >= 2)){
                        List(horizontal(v, h, rem_steps))
                    }
                    else{
                        putDomino(rem_steps).map(cons(State.Pebble, v, h))
                    }))

def horizontal(v: Int, h: Int, steps: Shape): Shape = 
    if((h > 2) || (steps.isEmpty)){
        (State.Free, v - 1, 2) :: (State.Free, 1, h - 2) :: steps
    }
    else{
        (steps match
            case (p, v_p, h_p) :: steps_p => 
                (State.Free, v - 1, 2) :: (p, v_p + 1, h_p) :: steps_p
            case Nil => throw new Exception("list of steps is empty"))
    }

def vertical(v: Int, h: Int, steps: Shape): Shape = 
    if((h > 1) || (steps.isEmpty)){
        (State.Free, v - 2, 1) :: (State.Free, 2, h - 1) :: steps
    }
    else{
        (steps match
            case (p, v_p, h_p) :: steps_p => 
                (State.Free, v - 2, 1) :: (p, v_p + 2, h_p) :: steps_p
            case Nil => throw new Exception("list of steps is empty"))
    }

def cons(step: Step)(steps: Shape): Shape =
    ((step, steps) match
        case ((p, v, h), (State.Free, 0, h_p) :: rem_steps) => 
            (p, v, h + h_p) :: rem_steps
        case ((p, v, h), listOfSteps) => (p, v, h) :: listOfSteps)

def tilings(m: Int, n: Int): Option[List[Shape]] =
    if((m % 2 == 1) && (n % 2 == 1)) {
        None
    }
    else{
        val d = (m * n) / 2
        Some(putnDominos(d)(List(List((State.Free, m, n)))))
    }

def putnDominos(d: Int)(shapes: List[Shape]): List[Shape] =
    if(d == 0){
        shapes
    }
    else{
        putnDominos(d - 1)(shapes.map(putDomino).flatten)
    }

@main def printResult(): Unit = {
    println("please input the width of your board as an integer:")
    val m = scala.io.StdIn.readInt()
    println("please input the height of your board as an integer:")
    val n = scala.io.StdIn.readInt()
    (tilings(m,n) match
        case None => println("the number of distinct tilings of a " + 
                     m + "x" + n + " board is 0.")
        case Some(listOfShapes) => println("the number of distinct tilings of a " + 
                     m + "x" + n + " board is " + listOfShapes.length + "."))
}