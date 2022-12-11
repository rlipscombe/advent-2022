import 'dart:io';

import 'package:day11/monkeys_definition.dart';

void main(List<String> arguments) {
  var input = File(arguments[0]).readAsStringSync();

  var definition = MonkeysDefinition();
  var parser = definition.build();

  var results = parser.parse(input).value;
  print(results);
}
