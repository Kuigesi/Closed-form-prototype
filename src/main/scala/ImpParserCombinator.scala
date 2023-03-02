package impparser
import structured.heap._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

sealed trait ImpToken

case class  IDENTIFIER(str: String)   extends ImpToken
case class  INTEGER(x: Int)           extends ImpToken
case class  BOOLEAN(x: Boolean)       extends ImpToken
case class  OPERATION(op: String)     extends ImpToken
case object ASSIGN                    extends ImpToken
case object NEW                       extends ImpToken
case object LPARENTHESIS              extends ImpToken
case object RPARENTHESIS              extends ImpToken
case object LBRACKET                  extends ImpToken
case object RBRACKET                  extends ImpToken
case object LBRACE                    extends ImpToken
case object RBRACE                    extends ImpToken
case object DEREF                     extends ImpToken
case object IF                        extends ImpToken
case object THEN                      extends ImpToken
case object ELSE                      extends ImpToken
case object WHILE                     extends ImpToken
case object DO                        extends ImpToken
case object SEMICOLON                 extends ImpToken
case object SKIP                      extends ImpToken
case object ABORT                     extends ImpToken
case object ASSERT                    extends ImpToken

trait ImpCompilationError
case class ImpLexerError(msg: String) extends ImpCompilationError

object ImpLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\n\r\f]+".r
  def identifier: Parser[IDENTIFIER] = {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }
  def integer: Parser[INTEGER] = {
    "[\\+-]?(0|([1-9][0-9]*))".r ^^ { x => INTEGER(x.toInt) }
  }
  def boolean: Parser[BOOLEAN] = {
    "true|false".r ^^ {x => BOOLEAN(x.toBoolean) }
  }
  def operation: Parser[OPERATION] = {
    "\\+|-|\\*|/|>=|>|<=|<|==|\\|\\||&&|!".r ^^ {op => OPERATION(op) }
  }
  def assign    = ":=" ^^ {_ => ASSIGN}
  def _new     = "new" ^^ {_ => NEW}
  def lp        = "(" ^^ {_ => LPARENTHESIS}
  def rp        = ")" ^^ {_ => RPARENTHESIS}
  def lb        = "[" ^^ {_ => LBRACKET}
  def rb        = "]" ^^ {_ => RBRACKET}
  def lc        = "{" ^^ {_ => LBRACE}
  def rc        = "}" ^^ {_ => RBRACE}
  def deref     = "&" ^^ {_ => DEREF}
  def _if       = "if" ^^ {_ => IF}
  def _then     = "then" ^^ {_ => THEN}
  def _else     = "else" ^^ {_ => ELSE}
  def _while    = "while" ^^ {_ => WHILE}
  def _do       = "do" ^^ {_ => DO}
  def semi      = ";" ^^ {_ => SEMICOLON}
  def skip      = "skip" ^^ {_ => SKIP}
  def abort     = "abort" ^^ {_ => ABORT}
  def assert    = "assert" ^^ {_ => ASSERT}

  def tokens: Parser[List[ImpToken]] = {
    phrase(rep1(assign | _new | lp | rp | lb | rb | lc | rc | _if | _then | _else | _while | _do | semi | skip | abort | assert | operation | boolean | integer | identifier | deref))
  }

  def apply(code: String): Either[ImpLexerError, List[ImpToken]] = {
    parse(tokens, code) match {
      //case NoSuccess(msg, next) => Left(ImpLexerError(msg))
      case Success(result, next) => Right(result)
      case Failure(msg,_) => Left(ImpLexerError("FAILURE: " + msg))
      case Error(msg,_) => Left(ImpLexerError("ERROR: " + msg))
    }
  }
}

case class ImpParserError(msg: String) extends ImpCompilationError

object ImpParser extends PackratParsers {
  override type Elem = ImpToken
  case class TokenPosition(l: Int, c: Int) extends Position {
    override def line: Int = l
    override def column: Int = c
    override def lineContents: String = "Token Sequence"
    override def longString: String = lineContents + toString
  }
  class ImpTokenReader(tokens: Seq[ImpToken]) extends Reader[ImpToken] {
    override def first: ImpToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = TokenPosition(0, -tokens.size)
    //override def pos: Position = NoPosition
    override def rest: Reader[ImpToken] = new ImpTokenReader(tokens.tail)
  }

  private def identifier: Parser[IDENTIFIER] = {
    accept("identifier", { case id @ IDENTIFIER(str) => id })
  }

  private def integer: Parser[INTEGER] = {
    accept("integer", { case i @ INTEGER(x) => i })
  }

  private def bool: Parser[BOOLEAN] = {
    accept("boolean", { case b @ BOOLEAN(x) => b })
  }

  private def op: Parser[OPERATION] = {
    accept("operation", { case op @ OPERATION(x) => op })
  }

  import Expr._
  import Stmt._

  lazy val var_sugar: PackratParser[Expr] = {
    identifier ^^ {case id => FieldRead(Addr(id.str), Const(0:Int))}
  }

  lazy val atomic_expr: PackratParser[Expr] = {
    lazy val const_int = integer ^^ { x => Const(x.x)}
    lazy val const_bool = bool ^^ { b => Const(b.x)}
    lazy val addr = (DEREF ~ identifier) ^^ {case _ ~ id => Addr(id.str)}
    const_int | const_bool | addr
  }

  lazy val elem_expr: PackratParser[Expr] = {
    lazy val p_expr = (LPARENTHESIS ~ expression ~ RPARENTHESIS) ^^ {case _ ~ e ~ _ => e}
    atomic_expr | var_sugar | p_expr
  }

  lazy val expression: PackratParser[Expr] = {
    lazy val binop = (expression ~ op ~ expression) ^^ {case e1 ~ op_s ~ e2 => BinOp(op_s.op, e1, e2)}
    lazy val fileread = (expression ~ LBRACKET ~ expression ~ RBRACKET) ^^ {case e1 ~ _ ~ e2 ~ _ => FieldRead(e1, e2)}
    binop | fileread | elem_expr
  }

  lazy val statement: PackratParser[Stmt] = {
    lazy val allocation = (identifier ~ ASSIGN ~ NEW) ^^ { case id ~ _ ~ _ => Alloc(id.str)}
    lazy val assignment_1 = (elem_expr ~ LBRACKET ~ expression ~ RBRACKET ~ ASSIGN ~ expression) ^^ { case e1 ~ _  ~ e2 ~ _ ~ _ ~ e3 => Assign(e1, e2, e3)}
    lazy val assignment_2 = (identifier ~ ASSIGN ~ expression) ^^ { case x ~ _  ~ e => Assign(Addr(x.str), Const(0:Int), e)}
    lazy val conditional = (IF ~ expression ~ THEN ~ statementbody ~ ELSE ~ statementbody) ^^ { case _ ~ cond ~ _ ~ then_s ~ _ ~ else_s => Cond(cond, then_s, else_s)}
    lazy val loop = (WHILE ~ expression ~ DO ~ statementbody) ^^ { case _ ~ e ~ _ ~ s => While(e, s)}
    lazy val skip = SKIP ^^ { _ => Skip()}
    lazy val abort = ABORT ^^ { _ => Abort()}
    lazy val assert = (ASSERT ~ expression) ^^ { case _ ~ e => Cond(e, Skip(), Abort())}
    allocation | assignment_1 | assignment_2 | conditional | loop | skip | abort | assert
  }

  lazy val tail_stmt: PackratParser[Stmt] = (statement ~ rep(SEMICOLON)) ^^ { case s ~ _ => s}

  lazy val statementlist: PackratParser[Stmt] = {
    lazy val sequence = (statement ~ rep1(SEMICOLON) ~ statementlist) ^^ { case s1 ~ _ ~ s2 => Seq(s1, s2)}
    sequence | tail_stmt
  }

  lazy val statementbody: PackratParser[Stmt] = {
    lazy val p_body = (LBRACE ~ statementlist ~ RBRACE) ^^ { case _ ~ s ~ _ => s}
    p_body | statement
  }

  def program: PackratParser[Stmt] = {
    phrase(statementlist)
  }

  def apply(tokens: scala.collection.immutable.Seq[ImpToken]): Either[ImpParserError, Stmt] = {
    val reader = new PackratReader(new ImpTokenReader(tokens))
    program(reader) match {
      //case NoSuccess(msg, next) => Left(ImpParserError(msg))
      case Success(result, next) => Right(result)
      case Failure(msg,_) => Left(ImpParserError("FAILURE: " + msg))
      case Error(msg,_) => Left(ImpParserError("ERROR: " + msg))
    }
  }
}

object ImpCompiler {
  import Expr._
  import Stmt._

  def apply(code: String): Either[ImpCompilationError, Stmt] = {
    for {
      tokens <- ImpLexer(code).right
      ast <- ImpParser(tokens).right
    } yield ast
  }
}

object TestSimpleParser {
  import ImpLexer._
  def main(args: Array[String]) = {
    val prog1 = """x := 0;
                   if(x > 0) then { x := 1; x:= 2; } else x := 3;
                   while (x < 10) do x:= x + 1;
                   y := new;
                   z := y[0];
                   """
    println(ImpCompiler(prog1))
  }
}

/*
case class WordFreq(word: String, count: Int) {
  override def toString = "Word <" + word + "> " +
                          "occurs with frequency " + count
}

class ImpParser extends RegexParsers {
  def word: Parser[String]    = """[a-z]+""".r        ^^ { _.toString }
  def const_int: Parser[Expr]     = """(0|[1-9]\d*)""".r  ^^ { x => Const(x.toInt) }
  def const_bool: Parser[Expr]    = """true|false""".r  ^^ { x => Const(x.toBoolean) }
  def number: Parser[Int]     = """(0|[1-9]\d*)""".r  ^^ { _.toInt }
  def freq: Parser[WordFreq]  = word ~ number         ^^ { case wd ~ fr => WordFreq(wd,fr) }
}

object TestSimpleParser extends ImpParser {
  def main(args: Array[String]) = {
    parse(const_int, "121") match {
      case Success(matched,_) => println(matched)
      case Failure(msg,_) => println("FAILURE: " + msg)
      case Error(msg,_) => println("ERROR: " + msg)
    }
  }
}*/