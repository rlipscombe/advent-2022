import 'dart:io';

import 'package:day11/day11.dart';
import 'package:day11/monkeys_definition.dart';

void main(List<String> arguments) {
  var input = File(arguments[0]).readAsStringSync();

  var definition = MonkeysDefinition();
  var parser = definition.build();

  List<Monkey> monkeys = parser.parse(input).value;

  var rounds = 20;
  for (var round = 1; round <= rounds; ++round) {
    for (var monkey in monkeys) {
      // print("Monkey ${monkey.id}:");

      while (monkey.items.isNotEmpty) {
        var item = monkey.items.removeAt(0);
        monkey.inspections++;
        // print("  Monkey inspects an item with a worry level of $item");
        item = monkey.operation.apply(item);
        item = (item / 3).floor();
        // print(
        //     "    Monkey gets bored with item. Worry level is divided by 3 to $item.");

        if (item % monkey.divisor == 0) {
          // print("    Current worry level is divisible by ${monkey.divisor}.");
          // print(
          //     "    Item with worry level $item is thrown to monkey ${monkey.ifTrue}.");
          monkeys[monkey.ifTrue].items.add(item);
        } else {
          // print(
          //     "    Current worry level is not divisible by ${monkey.divisor}.");
          // print(
          //     "    Item with worry level $item is thrown to monkey ${monkey.ifFalse}.");
          monkeys[monkey.ifFalse].items.add(item);
        }
      }
    }

    // print(
    //     "After round $round, the monkeys are holding items with these worry levels:");
    // for (var monkey in monkeys) {
    //   print("Monkey ${monkey.id}: ${monkey.items.join(", ")}");
    // }
  }

  for (var monkey in monkeys) {
    print("Monkey ${monkey.id} inspected items ${monkey.inspections} times.");
  }

  var inspections = monkeys.map((m) => m.inspections).toList();
  inspections.sort((a, b) => b.compareTo(a));
  var result = inspections[0] * inspections[1];
  print("$result");
}
