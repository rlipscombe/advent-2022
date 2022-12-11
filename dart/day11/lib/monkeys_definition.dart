import 'package:day11/day11.dart';
import 'package:petitparser/petitparser.dart';

class MonkeysDefinition extends GrammarDefinition {
  @override
  Parser start() => ref0(monkey).trim().star();

  Parser<Monkey> monkey() => (ref0(monkeyId) &
              ref0(startingItems) &
              ref0(operation) &
              ref0(test) &
              ref0(ifTrue) &
              ref0(ifFalse))
          .map((values) {
        print(values);
        return Monkey(
            values[0], values[1], values[2], values[3], values[4], values[5]);
      });

  Parser<int> monkeyId() =>
      (string("Monkey ") & ref0(number) & string(":")).map((values) {
        return values[1];
      });

  Parser<List<int>> startingItems() =>
      (string("Starting items: ").trim() & ref0(numbers)).map((values) {
        return values[1];
      });

  Parser<Expr> operation() =>
      (string("Operation: new = ").trim() & ref0(expression)).map((values) {
        return values[1];
      });

  Parser<Pred> test() =>
      (string("Test: divisible by ").trim() & ref0(number)).map((values) {
        return IsDivisibleBy(values[1]);
      });

  Parser<int> ifTrue() =>
      (string("If true: throw to monkey ").trim() & ref0(number)).map((values) {
        return values[1];
      });

  Parser<int> ifFalse() =>
      (string("If false: throw to monkey ").trim() & ref0(number))
          .map((values) {
        return values[1];
      });

  Parser<Expr> expression() => (addition() | multiplication()).cast<Expr>();

  Parser<Expr> addition() => binary('+', (lhs, rhs) => AddExpr(lhs, rhs));
  Parser<Expr> multiplication() => binary('*', (lhs, rhs) => MulExpr(lhs, rhs));

  Parser<R> binary<R>(String op, R Function(Expr lhs, Expr rhs) f) =>
      (ref0(old) & string(op).trim() & ref0(either))
          .map((values) => f(values[0], values[2]));

  Parser<Expr> either() => (ref0(old) | ref0(numberLiteral)).cast<Expr>();

  Parser<LookupExpr> old() => (string("old").trim()).map((_) => LookupExpr());
  Parser<LiteralExpr> numberLiteral() =>
      (digit().plus().flatten()).map((value) => LiteralExpr(int.parse(value)));

  Parser<int> number() =>
      (digit().plus().flatten()).map((value) => int.parse(value));

  Parser<List<int>> numbers() =>
      ref0(number).separatedBy(char(',').trim(), includeSeparators: false);
}
