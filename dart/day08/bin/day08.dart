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

class Trees {
  final List<List<int>> trees;
  final int colCount;
  final int rowCount;

  Trees(this.trees, this.colCount, this.rowCount);

  static Trees fromString(String input) {
    var lines = input.trim().split("\n");

    List<List<int>> data = [];
    for (var line in lines) {
      data.add(line.codeUnits.map((e) => e - 48).toList());
    }

    var colCount = lines[0].length;
    var rowCount = lines.length;

    return Trees(data, colCount, rowCount);
  }

  void traverseRow(int row, Set<Point> visible) {
    var max = -1;
    for (var col = 0; col < colCount; ++col) {
      var height = trees[row][col];
      if (height > max) {
        visible.add(Point(col, row));
        max = height;
      }
    }
  }

  void traverseRowReverse(int row, Set<Point> visible) {
    var max = -1;
    for (var col = colCount - 1; col >= 0; --col) {
      var height = trees[row][col];
      if (height > max) {
        visible.add(Point(col, row));
        max = height;
      }
    }
  }

  void traverseColumn(int col, Set<Point> visible) {
    var max = -1;
    for (var row = 0; row < rowCount; ++row) {
      var height = trees[row][col];
      if (height > max) {
        visible.add(Point(col, row));
        max = height;
      }
    }
  }

  void traverseColumnReverse(int col, Set<Point> visible) {
    var max = -1;
    for (var row = rowCount - 1; row >= 0; --row) {
      var height = trees[row][col];
      if (height > max) {
        visible.add(Point(col, row));
        max = height;
      }
    }
  }
}

void main(List<String> arguments) {
  var input = File(arguments[0]).readAsStringSync();

  Trees trees = Trees.fromString(input);

  Set<Point> visible = {};
  for (var row = 0; row < trees.rowCount; ++row) {
    trees.traverseRow(row, visible);
    trees.traverseRowReverse(row, visible);
  }

  for (var col = 0; col < trees.colCount; ++col) {
    trees.traverseColumn(col, visible);
    trees.traverseColumnReverse(col, visible);
  }

  print("part 1: ${visible.length}");
}
