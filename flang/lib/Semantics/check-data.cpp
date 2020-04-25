//===-- lib/Semantics/check-data.cpp --------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "check-data.h"

namespace Fortran::semantics {

template <typename T> void DataChecker::CheckIfConstantSubscript(const T &x) {
  evaluate::ExpressionAnalyzer exprAnalyzer{context_};
  if (MaybeExpr checked{exprAnalyzer.Analyze(x)}) {
    if (!evaluate::IsConstantExpr(*checked)) { // C875,C881
      context_.Say(parser::FindSourceLocation(x),
          "Data object must have constant bounds"_err_en_US);
    }
  }
}

void DataChecker::CheckDesignator(const parser::Designator &designator) {
  const Scope &scope{context_.FindScope(designator.source)};
  evaluate::ExpressionAnalyzer exprAnalyzer{context_};
  if (MaybeExpr expr{exprAnalyzer.Analyze(designator)}) {
  if (const Symbol * symbol{evaluate::GetFirstSymbol(*expr)}) {
    if (symbol->IsDummy()) {
      context_.Say(
          "Data object must not be a dummy argument"_err_en_US);
    } else if (IsFunction(*symbol)) {
      context_.Say(
          "Data object must not be a function name"_err_en_US);
    } else if (symbol->IsFuncResult()) {
      context_.Say(
          "Data object must not be a function result"_err_en_US);
    } else if (IsHostAssociated(*symbol, scope)) {
      context_.Say(
          "Data object must not be accessed by host association"_err_en_US);
    } else if (IsUseAssociated(*symbol, scope)) {
      context_.Say(
          "Data object must not be accessed by use association"_err_en_US);
    }
  }
  for (const Symbol &symbol : evaluate::CollectSymbols(*expr)) {
    if (FindCommonBlockContaining(symbol)) {
      if (const auto *details{
              symbol.detailsIf<semantics::ObjectEntityDetails>()}) {
        if (details->commonBlock()) {
          if (details->commonBlock()->name().empty()) {
            context_.Say(designator.source,
                "Data object part '%s' must not be in blank COMMON"_err_en_US,
                symbol.name().ToString());
          } else if (scope.kind() != Scope::Kind::BlockData) {
            context_.Say(designator.source,
                "Data object part '%s' must not be in a named COMMON block outside a BLOCK DATA program unit"_err_en_US,
                symbol.name().ToString());
          }
        }
      }
    } else if (IsAutomaticArray(symbol)) {
      context_.Say(designator.source,
          "Data object part '%s' must not be an automatic array"_err_en_US,
          symbol.name().ToString());
    } else if (IsAllocatable(symbol)) {
      context_.Say(designator.source,
          "Data object part '%s' must not be an allocatable object"_err_en_US,
          symbol.name().ToString());
    }
  }
  }
}

void DataChecker::CheckSubscript(const parser::SectionSubscript &subscript) {
  std::visit(common::visitors{
                 [&](const parser::SubscriptTriplet &triplet) {
                   CheckIfConstantSubscript(std::get<0>(triplet.t));
                   CheckIfConstantSubscript(std::get<1>(triplet.t));
                   CheckIfConstantSubscript(std::get<2>(triplet.t));
                 },
                 [&](const parser::IntExpr &intExpr) {
                   CheckIfConstantSubscript(intExpr);
                 },
             },
      subscript.u);
}

// Returns false if  DataRef has no subscript
bool DataChecker::CheckDataRef(const parser::DataRef &dataRef,
    parser::CharBlock source, bool isRightMostRef) {
  if (!isRightMostRef) {
    const auto name{parser::GetLastName(dataRef)};
    const Symbol *symbol{name.symbol ? &name.symbol->GetUltimate() : nullptr};
    if (IsPointer(*symbol)) { // C877
      context_.Say(name.source,
          "Only right-most part of data object can be a pointer"_err_en_US);
    }
  }
  return std::visit(
      common::visitors{
          [&](const parser::Name &) { return false; },
          [&](const common::Indirection<parser::StructureComponent>
                  &structureComp) {
            return CheckDataRef(structureComp.value().base, source, false);
          },
          [&](const common::Indirection<parser::ArrayElement> &arrayElem) {
            for (auto &subscript : arrayElem.value().subscripts) {
              CheckSubscript(subscript);
            }
            CheckDataRef(arrayElem.value().base, source, false);
            return true;
          },
          [&](const common::Indirection<parser::CoindexedNamedObject>
                  &coindexedObj) { // C874
            context_.Say(source,
                "Data object must not be a coindexed variable"_err_en_US);
            CheckDataRef(coindexedObj.value().base, source, false);
            return true;
          },
      },
      dataRef.u);
}

void DataChecker::Leave(const parser::DataStmtConstant &dataConst) {
  if (const auto *structure{
          std::get_if<parser::StructureConstructor>(&dataConst.u)}) {
    for (const auto &component :
        std::get<std::list<parser::ComponentSpec>>(structure->t)) {
      const parser::Expr &parsedExpr{
          std::get<parser::ComponentDataSource>(component.t).v.value()};
      if (const auto *expr{GetExpr(parsedExpr)}) {
        if (!evaluate::IsConstantExpr(*expr)) { // C884
          context_.Say(parsedExpr.source,
              "Structure constructor in data value must be a constant expression"_err_en_US);
        }
      }
    }
  }
}

// TODO: C879
void DataChecker::Leave(const parser::DataImpliedDo &dataImpliedDo) {
  for (const auto &object :
      std::get<std::list<parser::DataIDoObject>>(dataImpliedDo.t)) {
    if (const auto *designator{parser::Unwrap<parser::Designator>(object)}) {
      CheckDesignator(*designator);
      if (auto *dataRef{std::get_if<parser::DataRef>(&designator->u)}) {
        evaluate::ExpressionAnalyzer exprAnalyzer{context_};
        if (MaybeExpr checked{exprAnalyzer.Analyze(*dataRef)}) {
          if (evaluate::IsConstantExpr(*checked)) { // C878
            context_.Say(designator->source,
                "Data implied do object must be a variable"_err_en_US);
          }
        }
        if (!CheckDataRef(*dataRef, designator->source, true)) { // C880
          context_.Say(designator->source,
              "Data implied do object must be subscripted"_err_en_US);
        }
      }
    }
  }
}

void DataChecker::Leave(const parser::DataStmtObject &dataObject) {
  if (std::get_if<common::Indirection<parser::Variable>>(&dataObject.u)) {
    if (const auto *designator{
            parser::Unwrap<parser::Designator>(dataObject)}) {
      CheckDesignator(*designator);
      if (auto *dataRef{std::get_if<parser::DataRef>(&designator->u)}) {
        CheckDataRef(*dataRef, designator->source, true);
      }
    } else { // C875
      context_.Say(parser::FindSourceLocation(dataObject),
          "Data object variable must not be a function reference"_err_en_US);
    }
  }
}

void DataChecker::Leave(const parser::DataStmtRepeat &dataRepeat) {
  if (const auto *designator{parser::Unwrap<parser::Designator>(dataRepeat)}) {
    if (auto *dataRef{std::get_if<parser::DataRef>(&designator->u)}) {
      evaluate::ExpressionAnalyzer exprAnalyzer{context_};
      if (MaybeExpr checked{exprAnalyzer.Analyze(*dataRef)}) {
        auto expr{
            evaluate::Fold(context_.foldingContext(), std::move(checked))};
        if (auto i64{ToInt64(expr)}) {
          if (*i64 < 0) { // C882
            context_.Say(designator->source,
                "Repeat count for data value must not be negative"_err_en_US);
          }
        }
      }
    }
  }
}
} // namespace Fortran::semantics
