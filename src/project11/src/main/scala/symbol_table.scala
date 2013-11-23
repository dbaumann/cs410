package compiler

import scala.collection.mutable._

/** Manages variable bindings for the class and subroutine scopes.
  */
object SymbolTable {

  private var classContext = HashMap.empty[String, SymbolTableEntry]
  private var subroutineContext = HashMap.empty[String, SymbolTableEntry]
  private var currentContext = classContext

  private var className = ""
  private var subroutineName = ""

  /** Kind of bindings that can stored in `SymbolTable`.
    */
  object VarKind extends Enumeration("static", "field", "arg", "var") {
    type VarKind = Value
    val STATIC, FIELD, ARG, VAR = Value
  }

  /** Value type which is stored in the map of bindings.
    */
  case class SymbolTableEntry(varKind: VarKind.Value, varType: String, varIndex: Int)

  /** Prepares `SymbolTable` to operate in a new class scope.
    */
  def startClass(name: String) {
    currentContext = classContext
    classContext.clear
    subroutineContext.clear
    className = name
  }

  /** Prepares `SymbolTable` to operate in a new subroutine scope.
  */
  def startSubroutine(name: String) {
    currentContext = subroutineContext
    subroutineContext.clear
    subroutineName = name
  }

  def scopeName = className + "." + subroutineName

  /** Defines a new identifier for `varName` with type `varType` and kind `varKind`. The new
    * binding is assigned a unique index.
    * @param varName the name of the new variable.
    * @param varType the type name of the new variable
    * @param varKind the kind of the new variable; should be "static", "field", "arg", or "var".
    */
  def define(varName: String, varType: String, varKind: String) {
    val kind = VarKind.withName(varKind)
    currentContext += (varName -> SymbolTableEntry(kind, varType, varCount(kind)))
  }

  /** Counts bindings in the current scope with a kind of `varKind`.
    * @return the number of variables of the given kind already defined in the current scope.
    */
  def varCount(varKind: VarKind.Value) = currentContext.count { kv => 
    val SymbolTableEntry(kind, _, _) = kv._2
    kind == varKind
  }

  /** Optionally returns the kind of the binding identified by `name`.
    * @return `Some`(`SymbolTable#VarKind.Value`) for the named identifier in the symbol table,
              or `None` if the identifier is unknown.
    */
  def kindOf(name: String): Option[VarKind.Value] = {
    var entry = currentContext.get(name)
    if(entry == None && currentContext == subroutineContext) entry = classContext.get(name)

    if(entry == None) return None
    
    val SymbolTableEntry(entryKind, _, _) = entry.get
    return Some(entryKind)
  }

  /** @return the type name of the binding identified by `name`.
    */
  def typeOf(name: String) = {
    var entry = currentContext.get(name)
    if(entry == None && currentContext == subroutineContext) entry = classContext.get(name)

    if(entry == None) throw new RuntimeException("failed to find `" + name + "` in either scope")

    val SymbolTableEntry(_, entryType, _) = entry.get
    entryType
  }

  /** @return the the index of the binding identified by `name`.
    */
  def indexOf(name: String) = {
    var entry = currentContext.get(name)
    if(entry == None && currentContext == subroutineContext) entry = classContext.get(name)

    if(entry == None) throw new RuntimeException("failed to find `" + name + "` in either scope")

    val SymbolTableEntry(_, _, entryIndex) = entry.get
    entryIndex
  }
}