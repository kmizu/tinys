package com.github.kmizu.tinys
import scala.collection.mutable
import com.github.kmizu.tinys.MExpr.*

def evaluateProgram(program: MProgram): Any = {
  val environment: mutable.Map[String, Any] = mutable.Map()
  program.functions.foreach { f =>
    environment(f.name) = f
  }
  var result: Any = null
  program.bodies.foreach { body =>
    result = evaluate(body, environment)
  }
  result
}

def evaluateMathExpr(e: MBinExpr, env: mutable.Map[String, Any]): Int = e.op match {
  case "+" => evaluate(e.lhs, env).asInstanceOf[Int] + evaluate(e.rhs, env).asInstanceOf[Int]
  case "-" => evaluate(e.lhs, env).asInstanceOf[Int] - evaluate(e.rhs, env).asInstanceOf[Int]
  case "*" => evaluate(e.lhs, env).asInstanceOf[Int] * evaluate(e.rhs, env).asInstanceOf[Int]
  case "/" => evaluate(e.lhs, env).asInstanceOf[Int] / evaluate(e.rhs, env).asInstanceOf[Int]
}

def evaluateCompExpr(e: MBinExpr, env: mutable.Map[String, Any]): Int = e.op match {
  case "<" => if (evaluate(e.lhs, env).asInstanceOf[Int] < evaluate(e.rhs, env).asInstanceOf[Int]) 1 else 0
  case ">" => if (evaluate(e.lhs, env).asInstanceOf[Int] > evaluate(e.rhs, env).asInstanceOf[Int]) 1 else 0
  case "<=" => if (evaluate(e.lhs, env).asInstanceOf[Int] <= evaluate(e.rhs, env).asInstanceOf[Int]) 1 else 0
  case ">=" => if (evaluate(e.lhs, env).asInstanceOf[Int] >= evaluate(e.rhs, env).asInstanceOf[Int]) 1 else 0
  case "==" => if (evaluate(e.lhs, env).asInstanceOf[Int] == evaluate(e.rhs, env).asInstanceOf[Int]) 1 else 0
  case "!=" => if (evaluate(e.lhs, env).asInstanceOf[Int] != evaluate(e.rhs, env).asInstanceOf[Int]) 1 else 0
}

def evaluate(e: MExpr, env: mutable.Map[String, Any]): Any = e match {
  case binExpr: MBinExpr => binExpr.op match {
    case "+" | "-" | "*" | "/" => evaluateMathExpr(binExpr, env)
    case "<" | ">" | "<=" | ">=" | "==" | "!=" => evaluateCompExpr(binExpr, env)
  }
  case MSeq(bodies) =>
    var result: Any = null
    bodies.foreach(body => result = evaluate(body, env))
    result
  case MIf(condition, thenClause, elseClause) =>
    val cond = evaluate(condition, env).asInstanceOf[Int]
    if (cond != 0) evaluate(thenClause, env) else evaluate(elseClause, env)
  case MWhile(condition, bodies) =>
    var cond = evaluate(condition, env).asInstanceOf[Int]
    while (cond != 0) {
      bodies.foreach(body => evaluate(body, env))
      cond = evaluate(condition, env).asInstanceOf[Int]
    }
    null
  case MAssignment(name, expression) =>
    val result = evaluate(expression, env)
    env(name) = result
    result
  case MIdent(name) => env(name)
  case MCall(name, args) =>
    val func = env(name).asInstanceOf[MFunc]
    val evaluatedArgs = args.map(arg => evaluate(arg, env))
    val newEnvironment = mutable.Map[String, Any](env.toSeq: _*)
    func.params.zip(evaluatedArgs).foreach { case (param, value) =>
      newEnvironment(param) = value
    }
    evaluate(func.body, newEnvironment)
  case MInt(value) => value
}

@main def main(): Unit = {
  val program = tProgram(
    List(
      tFunction("main", List(), tSeq(
        tAssign("x", tInt(1)),
        tAssign("y", tInt(2)),
        tCall("add", tId("x"), tId("y"))
      )),
      tFunction("add", List("a", "b"), tAdd(tId("a"), tId("b")))
    ),
    tCall("main")
  )
  println(evaluateProgram(program))
}