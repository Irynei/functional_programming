package main.tiny_language

object main {
  def main(args: Array[String]): Unit = {
    val environment = Map(
    "x" -> Number(5),
    "y" -> Number(3),
    "z" -> Bool(true)
    )
    val machine = new Machine(environment)
    //    (2 + 3) * (x + y) => 40
    machine.run(
      Prod(Sum(Number(2), Number(3)), Sum(Var("x"), Var("y")))
    )
    println()
//    (2 + 3) * z  => (error expected)
    machine.run(
      Prod(Sum(Number(2), Number(3)), Var("z"))
    )
    println()
//    (2 + 3) * 2 < 3 * (1 + 2) => false
    machine.run(
      Less(Prod(Sum(Number(2), Number(3)), Number(2)), Prod(Number(3), Sum(Number(1), Number(2))))
    )
    println()
//    2 < z => (error expected)
    machine.run(
      Less(Number(2), Var("z"))
    )
    println()

//  if (z) then (2 + 2) * 2  else 3  => 8
    machine.run(
      IfElse(Var("z"), Prod(Sum(Number(2), Number(2)), Number(2)), Number(3))
    )
    println()
//    assign superPuper = if (z) then (2 + 2) * 2  else 3  => (superPuper = 8)
    machine.run(
      Assign("superPuper", IfElse(Var("z"), Prod(Sum(Number(2), Number(2)), Number(2)), Number(3)))
    )
    println()
//    (2 + superPuper) * superPuper => 80
    machine.run(
      Prod(Sum(Number(2), Var("superPuper")), Var("superPuper"))
    )
    println()
//    if(2) then (superPuper = (2 + 2) * 2) else (nothing) => error expected
    machine.run(
      IfElseStatement(
        Number(2),
        Assign("superPuper", Prod(Sum(Number(2), Number(2)), Number(-2))),
        DoNothing()
    ))
    println()

    machine.run(
      IfElseStatement(
        IfElse(
          Less(Number(1), Number(2)),
          Less(Number(1), Prod(Number(23), Sum(Number(2), Number(23)))),
          Less(Number(1), Prod(Number(-3), Number(-2)))),
        Assign("superPuper", Number(-2)),
        Assign("superPuper", Number(2))
      ))
    println()

    machine.run(
      While(
        Less(Sum(Var("superPuper"), Number(2)), Number(4)),
        Assign("superPuper", Sum(Var("superPuper"), Number(1)))
      )
    )

    println()
    machine.run(
      Sequence(List[Statement](
        Assign("superPuper", Bool(true)),
        IfElseStatement(Var("superPuper"), Assign("superPuper", Number(1)), Assign("superPuper", Number(234))),
        Assign("superPuper", Number(234))
      ))
    )

  }
}
