package wacc.front

import parsley.Parsley
import parsley.token.descriptions._
import parsley.token.errors.{ErrorConfig, LabelAndReason}
import parsley.token.predicate.Unicode
import parsley.token.{Lexer, predicate}

object ErrConfig extends ErrorConfig {
  override def labelEscapeEnd = LabelAndReason("end of escape sequence", 
    "valid escape sequence includes \\0, \\b, \\t, \\n, \\f, \\r, \\', \\\" or \\\\")
  override def labelEscapeSequence = LabelAndReason("escaped sequence", 
  "valid escape sequence includes \\0, \\b, \\t, \\n, \\f, \\r, \\', \\\" or \\\\")
  override def labelGraphicCharacter = LabelAndReason("ascii graphic character", 
  "ascii graphic letter(except \\, \' and \")")
}

object Lexer {
  final val idStartSet = Set('_') ++ ('a' to 'z').toSet ++ ('A' to 'Z').toSet
  final val idCoSet = idStartSet ++ ('0' to '9').toSet
  final val excludedChar = Set('\"'.toInt, '\''.toInt, '\\'.toInt)
  
  final val keys = Set("begin", "end", 
  "is", 
  "skip", "read", "free", "return", "exit", 
  "print", "println", "if", "then", "else", "fi", "while", "do", "done",
  "fst", "snd", "newpair", "call", "int", "bool", "char", "string",
  "pair", "true", "false", "null", "len", "ord", "chr")

  final val ops = Set("=", "!", "-", "*", "/", "%", "+", "-",
  ">", ">=", "<", "<=", "==", "!=", "&&", "||")

  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(idStartSet(_)),
      identifierLetter = predicate.Basic(idCoSet(_))
    ),
    spaceDesc = SpaceDesc.plain.copy(
      commentLine = "#",
      space = predicate.Basic(Character.isWhitespace)
    ),
    textDesc = text.TextDesc.plain.copy(
      escapeSequences = text.EscapeDesc.plain.copy(
        //escBegin = '\\',
        literals = Set('\'', '\"', '\\'),
        singleMap = Map('0' -> 0x0000,
                        'b' -> 0x0008,
                        't' -> 0x0009,
                        'n' -> 0x000a,
                        'f' -> 0x000c,
                        'r' -> 0x000d)
      ),
      graphicCharacter = Unicode(c => c >= ' '.toInt && !excludedChar.contains(c))
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = keys,
      hardOperators = ops,
    )
  )

  private val lexer = new Lexer(desc, ErrConfig)
  val num = lexer.lexeme.numeric.integer.decimal32
  val identifier = lexer.lexeme.names.identifier
  val str = lexer.lexeme.text.string.latin1
  val char = lexer.lexeme.text.character.latin1

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
  val implicits = lexer.lexeme.symbol.implicits
}
