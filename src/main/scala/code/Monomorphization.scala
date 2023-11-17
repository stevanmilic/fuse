package code

import core.Bindings.Bind

object Monomorphization {

  /** Replaces each generic function invocation with monomorphic function.
    *
    * To perform replacement it is required to:
    *   - counts all instantiations of generic function (constructor)
    *   - creates a specialized function for each instantiation
    *   - replace each generic function invocation to a specialized function
    *
    * The process also includes replacing type class instance function
    * invocations wtih a specific implementations.
    * @return
    *   monomorphic bindings
    */
  def replace(binds: List[Bind]): List[Bind] = ???
}
