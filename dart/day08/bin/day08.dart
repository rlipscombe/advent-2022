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
  var width = lines[0].length;
  var height = lines.length;

  List<List<int>> trees = [];
  for (var line in lines) {
    trees.add(line.codeUnits.map((e) => e - 48).toList());
  }

  Set<Point> visible = {};
  for (var row = 0; row < height; ++row) {
    var max = -1;
    for (var col = 0; col < width; ++col) {
      var height = trees[row][col];
      if (height > max) {
        visible.add(Point(col, row));
        max = height;
      }
    }

    max = -1;
    for (var col = width - 1; col >= 0; --col) {
      var height = trees[row][col];
      if (height > max) {
        visible.add(Point(col, row));
        max = height;
      }
    }
  }

  for (var col = 0; col < width; ++col) {
    var max = -1;
    for (var row = 0; row < height; ++row) {
      var height = trees[row][col];
      if (height > max) {
        visible.add(Point(col, row));
        max = height;
      }
    }

    max = -1;
    for (var row = height - 1; row >= 0; --row) {
      var height = trees[row][col];
      if (height > max) {
        visible.add(Point(col, row));
        max = height;
      }
    }
  }

  print("part 1: ${visible.length}");
}
