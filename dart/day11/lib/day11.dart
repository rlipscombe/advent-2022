class Monkey {
  final int id;
  List<int> items;
  final Expr operation;
  final int divisor;
  final int ifTrue;
  final int ifFalse;

  int inspections;

  Monkey(this.id, this.items, this.operation, this.divisor, this.ifTrue,
      this.ifFalse,
      {this.inspections = 0});
}

abstract class Expr {
  int apply(int old);
}

class LiteralExpr extends Expr {
  final int value;

  LiteralExpr(this.value);

  @override
  int apply(int old) {
    return value;
  }
}

class LookupExpr extends Expr {
  @override
  int apply(int old) {
    return old;
  }
}

class AddExpr extends Expr {
  final Expr lhs;
  final Expr rhs;

  AddExpr(this.lhs, this.rhs);

  @override
  int apply(int old) {
    var by = rhs.apply(old);
    var result = lhs.apply(old) + by;
    // print("    Worry level increases by $by to $result.");
    return result;
  }
}

class MulExpr extends Expr {
  final Expr lhs;
  final Expr rhs;

  MulExpr(this.lhs, this.rhs);

  @override
  int apply(int old) {
    var by = rhs.apply(old);
    var result = lhs.apply(old) * by;
    // print("    Worry level is multiplied by $by to $result.");
    return result;
  }
}
