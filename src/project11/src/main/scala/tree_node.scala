package compiler

import scala.collection.mutable.LinkedList

/** A data structure for representing a parse tree.
  */
case class TreeNode[T](value: T, var children: Option[LinkedList[TreeNode[T]]]) {
  /** Creates a new leaf node in this tree and stores `value` in it. If invoked on a leaf,
    * this operation has no effect.
    * @return void
    */
  def appendLeaf(value: T) {
    if(children != None) children = Some(children.get :+ TreeNode[T](value, None))
  }

  /** Creates a new tree node in this tree and stores `value` in it.
    * @return a reference to the newly created subtree, or `this` if invoked on a leaf.
    */
  def appendTree(value: T): TreeNode[T] = {
    if(children != None) {
      val appendedChild = TreeNode[T](value, Some(new LinkedList[TreeNode[T]]()))
      children = Some(children.get :+ appendedChild)
      return appendedChild
    } else return this
  }
}