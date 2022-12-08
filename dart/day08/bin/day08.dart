import 'dart:io';

import 'package:day08/day08.dart';

void main(List<String> arguments) {
  var input = File(arguments[0]).readAsStringSync();

  Trees trees = Trees.fromString(input);

  Set<Point> visible = {};
  findTaller(max, col, row, height) {
    if (height > max) {
      visible.add(Point(col, row));
      return height;
    }
    return max;
  }

  for (var row = 0; row < trees.rowCount; ++row) {
    trees.foldRow(row, -1, findTaller);
    trees.foldRowReverse(row, -1, findTaller);
  }

  for (var col = 0; col < trees.colCount; ++col) {
    trees.foldColumn(col, -1, findTaller);
    trees.foldColumnReverse(col, -1, findTaller);
  }

  print("part 1: ${visible.length}");
}
