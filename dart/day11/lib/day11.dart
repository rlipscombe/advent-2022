class Monkey {
  final int id;
  List<int> items;
  final Expr operation;
  final Pred pred;
  final int ifTrue;
  final int ifFalse;

  Monkey(this.id, this.items, this.operation, this.pred, this.ifTrue,
      this.ifFalse);
}

class Expr {}

class LiteralExpr extends Expr {
  final int value;

  LiteralExpr(this.value);
}

class LookupExpr extends Expr {}

class AddExpr extends Expr {
  final Expr lhs;
  final Expr rhs;

  AddExpr(this.lhs, this.rhs);
}

class MulExpr extends Expr {
  final Expr lhs;
  final Expr rhs;

  MulExpr(this.lhs, this.rhs);
}

class Pred {}

class IsDivisibleBy extends Pred {
  final int divisor;
  IsDivisibleBy(this.divisor);
}
