import 'dart:io';

import 'package:day08/day08.dart' as day08;

class Point {
  final int col;
  final int row;

  Point(this.col, this.row);

  @override
  bool operator ==(Object other) {
    return other is Point && (other.col == col) && (other.row == row);
  }

  @override
  int get hashCode => col * 137 + row;
}

void main(List<String> arguments) {
  var lines = File(arguments[0]).readAsStringSync().trim().split("\n");
  var colCount = lines[0].length;
  var rowCount = lines.length;

  List<List<int>> trees = [];
  for (var line in lines) {
    trees.add(line.codeUnits.map((e) => e - 48).toList());
  }

  Set<Point> visible = {};
  for (var row = 0; row < rowCount; ++row) {
    var max = -1;
    for (var col = 0; col < colCount; ++col) {
      var height = trees[row][col];
      if (height > max) {
        visible.add(Point(col, row));
        max = height;
      }
    }

    max = -1;
    for (var col = colCount - 1; col >= 0; --col) {
      var height = trees[row][col];
      if (height > max) {
        visible.add(Point(col, row));
        max = height;
      }
    }
  }

  for (var col = 0; col < colCount; ++col) {
    var max = -1;
    for (var row = 0; row < rowCount; ++row) {
      var height = trees[row][col];
      if (height > max) {
        visible.add(Point(col, row));
        max = height;
      }
    }

    max = -1;
    for (var row = rowCount - 1; row >= 0; --row) {
      var height = trees[row][col];
      if (height > max) {
        visible.add(Point(col, row));
        max = height;
      }
    }
  }

  print("part 1: ${visible.length}");
}
