package com.moseph.sclcc

import com.moseph.sclcc.prolog._

trait Interface {
  def askAbout(s:Stuff) : Option[Subst]
}

trait MultipleInterface extends Interface {
  def interfaces : Seq[Interface]
  def askAbout(s:Stuff) = (interfaces view) map {_.askAbout(s)} find {_.isDefined} flatten
}

class NullInterface extends Interface {
  def askAbout(s:Stuff) : Option[Subst] = None
}

class ConsoleInterface extends Interface with LCCParsers {
  import InterpreterHelper._
  def askAbout(s:Stuff) : Option[Subst]= {
    println("?"*80+s"\n Tell me: how do you feel about: $s\nPlease write it as 'VAR -> Term'*\n")
    val vars = varsof(s) toList
    val subs = vars map {v=>(v,ask_var(v))} collect {case (v,Some(s))=>(v,s)}
    subs.size == vars.size match {
      case true => Some(Subst(subs :_*))
      case false => None
    }
  }
  
  def ask_var(v:String) : Option[Term] = {
    println(s"? $v:")
    val resp = readLine()
    parseAll(compound_term,resp) match {
      case Success(t,_) => Some(t)
      case NoSuccess(_,_) if resp == "" => println("Giving up"); None
      case NoSuccess( err, next ) => printError(err,next,resp); ask_var(v)
    }
  }
}