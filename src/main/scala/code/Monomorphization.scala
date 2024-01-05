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
    // Collect instantiations.
    val insts = binds.map(_.insts).flatten
    val s = for {
      // Create specialilized functions for each bindig that has an instantiation.
      specializedBinds <- binds.flatTraverse(bind =>
        // TODO: We should create specialized binds for all constructors for an ADT.
        insts.filter(_.i == bind.i) match {
          case Nil => addBinding(bind.i, bind.b).map(_ => List(bind))
          case i =>
            i.traverse(inst =>
              for {
                binding <- buildSpecializedBinding(bind.b, inst)
                name <- toContextState(inst.bindName())
                id <- addBinding(name, binding)
              } yield Bind(id, binding, bind.insts)
            )
        }
      )
      // Replace each generic function invocation with a specialized function.
      modifiedBinds <- specializedBinds.traverse(replaceInstantiations(_))
    } yield modifiedBinds
    s.runEmptyA.value

  def buildSpecializedBinding(
      binding: Binding,
      instantiation: Instantiation
  ): ContextState[Binding] =
    binding match {
      case TermAbbBind(term: TermTAbs, ty) =>
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

  def specilizeTerm(
      term: Term,
      typeVarIndex: Int,
      tyS: Type
  ): Term =
    term match {
      case TermTAbs(info, i, _, body) =>
        termSubstituteType(tyS, typeVarIndex, body)
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

  def replaceInstantiations(bind: Bind): ContextState[Bind] =
    bind.insts.foldM(bind)((acc, inst) =>
      for {
        // genericBindIndex <- nameToIndexWithContext(inst.i)
        specializedBindName <- toContextState(inst.bindName())
        specializedBindIndex <- nameToIndexWithContext(specializedBindName)
        replacedBinding = (
          acc.b,
          // genericBindIndex,
          specializedBindIndex
        ) match {
          case (TermAbbBind(tT, ty), Some(s)) =>
            TermAbbBind(termVarAbsSubstitute(s, inst.idx, tT), ty)
          case (b, _) => b
        }
      } yield Bind(bind.i, replacedBinding)
    )

  def nameToIndexWithContext(name: String): ContextState[Option[Int]] =
    State.inspect { (ctx: Context) => nameToIndex(ctx, name) }

}
