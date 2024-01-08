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
import parser.Info.UnknownInfo

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
    // // Collect instantiations.
    // val insts = Instantiations.distinct(binds.map(_.insts).flatten)
    // // Create specialilized functions for each bindig that has an instantiation.
    // val specializedBinds = toSpecializedBinds(binds, insts)
    // // Replace each generic function invocation with a specialized function.
    // specializedBinds
    //   .flatMap(_.traverse(replaceInstantiations(_)))
    //   .runEmptyA
    //   .value
    replaceM(binds).runEmptyA.value

  def replaceM(binds: List[Bind]): ContextState[List[Bind]] =
    // Collect instantiations.
    val insts = Instantiations.distinct(binds.map(_.insts).flatten)
    insts match {
      case Nil => binds.pure[ContextState]
      case _ =>
        for {
          // Create specialilized functions for each bindig that has an instantiation.
          specializedBinds <- toSpecializedBinds(binds, insts)
          // Replace each generic function invocation with a specialized function.
          modifiedBinds <- specializedBinds.traverse(replaceInstantiations(_))
          ibinds <- replaceM(modifiedBinds)
        } yield ibinds
    }

  /** Finds all bind instantiations in a specified list.
    *
    * In case bind is an ADT all instantiations for that type are associated to
    * it.
    */
  def getBindInstantiations(
      bind: Bind,
      insts: List[Instantiation]
  ): ContextState[List[Instantiation]] = for {
    typeNameOption <- getAlgebraicDataTypeName(UnknownInfo, bind.i)
    bindInsts <- typeNameOption match {
      case None => insts.filter(_.i == bind.i).pure[ContextState]
      case Some(typeName) =>
        insts
          .filterA(i =>
            getAlgebraicDataTypeName(UnknownInfo, i.i).map(
              _.map(_ == typeName).getOrElse(false)
            )
          )
          .map(_.map(i => Instantiation(bind.i, i.term, i.tys)))
          .map(Instantiations.distinct(_))
    }
  } yield bindInsts.filter(_.tys.forall(_.isPrimitive))

  def toSpecializedBinds(
      binds: List[Bind],
      insts: List[Instantiation]
  ): ContextState[List[Bind]] =
    binds
      .foldLeftM((List[Bind](), 0)) { case ((acc, shift), bind) =>
        getBindInstantiations(bind, insts).flatMap(_ match {
          case Nil =>
            val sbind = bindShift(shift, bind)
            addBinding(sbind.i, sbind.b).map(_ => (acc :+ sbind, shift))
          case i =>
            i.zipWithIndex
              .traverse((inst, idx) =>
                for {
                  // NOTE: Specialized binding is shifted based on the number of
                  // bindings built, as they also shift the context.
                  binding <- buildSpecializedBinding(bind.b, inst)
                    .map(bindingShift(idx, _))
                  name <- toContextState(inst.bindName())
                  id <- addBinding(name, binding)
                } yield Bind(id, binding, bind.insts)
              )
              .map(l => (acc ::: l, shift + l.length - 1))
        })
      }
      .map(_._1)

// TODO: On specializing binding we also gotta specilize instantiation types that exist on
// on the binding itself. In order to replace binding instantations later in the flow.
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
      case _ =>
        throw new RuntimeException(
          s"can't build specialized binding ${instantiation.i}"
        )
    }

  def specilizeTerm(
      term: Term,
      typeVarIndex: Int,
      tyS: Type
  ): Term =
    term match {
      case TermTAbs(info, i, _, body) =>
        termSubstituteType(tyS, typeVarIndex, body)
      case _ =>
        throw new RuntimeException(s"can't specialize term ${term}")
    }

  def specilizeType(
      ty: Type,
      typeVarIndex: Int,
      tyS: Type
  ): Type =
    ty match {
      case TypeAll(_, _, _, _, tyT) =>
        typeSubstitute(tyS, typeVarIndex, tyT)
      case _ => throw new RuntimeException(s"can't specialize type ${ty}")
    }

  /** Replaces all instantiations found on specified bind with specialized
    * functions (binds).
    */
  def replaceInstantiations(bind: Bind): ContextState[Bind] =
    bind.insts.foldM(bind)((acc, inst) =>
      for {
        specializedBindName <- toContextState(inst.bindName())
        specializedBindIndex <- State.inspect { (ctx: Context) =>
          nameToIndex(ctx, specializedBindName)
        }
        (replacedBinding, insts) = (
          acc.b,
          inst.term,
          specializedBindIndex
        ) match {
          case (TermAbbBind(tT, ty), tC: TermVar, Some(s)) =>
            (
              TermAbbBind(termVarSubstitute(s, tC, tT), ty),
              bind.insts.filterNot(_ == inst)
            )
          case (b, _, _) => (b, bind.insts)
        }
      } yield Bind(bind.i, replacedBinding, insts)
    )

}
