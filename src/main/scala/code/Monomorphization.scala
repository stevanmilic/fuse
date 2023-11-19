package code

import cats.data.EitherT
import cats.data.OptionT
import cats.data.State
import cats.data.StateT
import cats.implicits.*
import core.Context.*
import core.Bindings.*
import core.Types.Type
import core.Terms.*
import core.Bindings.*
import core.TypeChecker.*
import core.Types.*
import core.Instantiations
import core.Context.addBinding
import code.GrinUtils.toContextState
import core.Instantiations.Instantiation
import core.Context
import core.Shifting.*

object Monomorphization {

  /** Replaces each generic function invocation with monomorphic function.
    *
    * To perform replacement it is required to:
    *   - find all instantiations of generic function
    *   - creates a specialized function for each instantiation
    *   - replace each generic function invocation to a specialized function
    *
    * The process also includes replacing type class instance function
    * invocations wtih a specific implementations.
    *
    * @return
    *   monomorphic bindings
    */
  def replace(binds: List[Bind]): List[Bind] =
    val s = for {
      // Collect instantiations
      insts <- binds
        .traverse(bind => addBinding(bind.i, bind.b).map(_ => bind.insts))
        .map(_.flatten)
      // Create specialilized functions for each instantiation.
      specializedBinds <- insts.traverse(inst =>
        for {
          binding <- buildSpecializedBinding(inst)
          id <- Context.addBinding(inst.i, binding)
        } yield Bind(id, binding)
      )
      // Replace each generic function invocation with a specialized function.
      modifiedBinds <- binds.traverse(
        replaceInstantiations(_, specializedBinds)
      )
    } yield (modifiedBinds ::: specializedBinds)
    s.runEmptyA.value

  def buildSpecializedBinding(
      instantiation: Instantiation
  ): ContextState[Binding] =
    for {
      binding <- State.inspect { (ctx: Context) =>
        nameToIndex(ctx, instantiation.i).flatMap(
          getNotes(ctx).lift(_).map(_._2)
        )
      }
      specBinding <- binding match {
        case Some(TermAbbBind(term: TermTAbs, ty)) =>
          val tys = instantiation.tys.zipWithIndex
          TermAbbBind(
            tys.foldRight(term: Term) { case ((ty, idx), t) =>
              specilizeTerm(t, idx, ty)
            },
            ty.map(
              tys.foldRight(_) { case ((tyS, idx), tyT) =>
                specilizeType(tyT, idx, tyS)
              }
            )
          ).pure[ContextState]
        // TODO: Raise proper error here.
        case _ => ???
      }
    } yield specBinding

  def specilizeTerm(
      term: Term,
      typeVarIndex: Int,
      tyS: Type
  ): Term =
    term match {
      case TermTAbs(info, i, _, body) =>
        termSubstituteType(tyS, body, typeVarIndex)
      case _ => ???
    }

  def specilizeType(
      ty: Type,
      typeVarIndex: Int,
      tyS: Type
  ): Type =
    ty match {
      case TypeAll(_, _, _, _, tyT) =>
        typeSubstitute(tyS, typeVarIndex, tyT)
      case _ => ???
    }

  def replaceInstantiations(
      bind: Bind,
      specializedBinds: List[Bind]
  ): ContextState[Bind] =
    // TODO: Implement replacing instantiations with specialized binds.
    bind.pure
}
