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

  TAcc foldRow<TAcc>(int row, TAcc acc,
      TAcc Function(TAcc acc, int col, int row, int height) combine) {
    for (var col = 0; col < colCount; ++col) {
      var height = trees[row][col];
      acc = combine(acc, col, row, height);
    }

    return acc;
  }

  TAcc foldRowReverse<TAcc>(int row, TAcc acc,
      TAcc Function(TAcc acc, int col, int row, int height) combine) {
    for (var col = colCount - 1; col >= 0; --col) {
      var height = trees[row][col];
      acc = combine(acc, col, row, height);
    }

    return acc;
  }

  TAcc foldColumn<TAcc>(int col, TAcc acc,
      TAcc Function(TAcc acc, int col, int row, int height) combine) {
    for (var row = 0; row < rowCount; ++row) {
      var height = trees[row][col];
      acc = combine(acc, col, row, height);
    }

    return acc;
  }

  TAcc foldColumnReverse<TAcc>(int col, TAcc acc,
      TAcc Function(TAcc acc, int col, int row, int height) combine) {
    for (var row = rowCount - 1; row >= 0; --row) {
      var height = trees[row][col];
      acc = combine(acc, col, row, height);
    }

    return acc;
  }
}
