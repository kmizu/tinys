package com.github.kmizu.tinys

import MExpr.*

case class MProgram(functions: List[MFunc], bodies: List[MExpr])
case class MFunc(name: String, params: List[String], body: MExpr)
enum MExpr {
  case MBinExpr(op: String, lhs: MExpr, rhs: MExpr)
  case MIf(condition: MExpr, thenClause: MExpr, elseClause: MExpr)
  case MSeq(bodies: List[MExpr])
  case MWhile(condition: MExpr, bodies: List[MExpr])
  case MCall(name: String, args: List[MExpr])
  case MAssignment(name: String, expression: MExpr)
  case MInt(value: Int)
  case MIdent(name: String)
}

// Helper functions to create instances of each case
def tProgram(functions: List[MFunc], bodies: MExpr*): MProgram = 
  MProgram(functions, bodies.toList)

def tFunction(name: String, params: List[String], body: MExpr): MFunc = 
  MFunc(name, params, body)

def tAdd(a: MExpr, b: MExpr): MBinExpr = 
  MBinExpr("+", a, b)

def tSub(a: MExpr, b: MExpr): MBinExpr = 
  MBinExpr("-", a, b)

def tMul(a: MExpr, b: MExpr): MBinExpr = 
  MBinExpr("*", a, b)

def tDiv(a: MExpr, b: MExpr): MBinExpr = 
  MBinExpr("/", a, b)

def tLt(a: MExpr, b: MExpr): MBinExpr = 
  MBinExpr("<", a, b)

def tGt(a: MExpr, b: MExpr): MBinExpr = 
  MBinExpr(">", a, b)

def tLte(a: MExpr, b: MExpr): MBinExpr = 
  MBinExpr("<=", a, b)

def tGte(a: MExpr, b: MExpr): MBinExpr = 
  MBinExpr(">=", a, b)

def tEq(a: MExpr, b: MExpr): MBinExpr = 
  MBinExpr("==", a, b)

def tNe(a: MExpr, b: MExpr): MBinExpr = 
  MBinExpr("!=", a, b)

def tInt(value: Int): MInt = 
  MInt(value)

def tAssign(name: String, value: MExpr): MAssignment = 
  MAssignment(name, value)

def tId(name: String): MIdent = 
  MIdent(name)

def tSeq(expressions: MExpr*): MSeq = 
  MSeq(expressions.toList)

def tCall(name: String, args: MExpr*): MCall = 
  MCall(name, args.toList)

def tWhile(condition: MExpr, bodies: MExpr*): MWhile = 
  MWhile(condition, bodies.toList)

def tIf(condition: MExpr, thenClause: MExpr, elseClause: MExpr): MIf = 
  MIf(condition, thenClause, elseClause)