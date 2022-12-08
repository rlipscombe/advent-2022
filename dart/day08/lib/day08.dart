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

class Range {
  final int begin;
  final int end;

  Range(this.begin, this.end);

  ReversedRange reverse() {
    return ReversedRange(begin, end);
  }

  TAcc fold<TAcc>(TAcc acc, Function(TAcc acc, int value) combine) {
    for (var value = begin; value < end; ++value) {
      acc = combine(acc, value);
    }

    return acc;
  }
}

class ReversedRange {
  final int begin;
  final int end;

  ReversedRange(this.begin, this.end);

  TAcc fold<TAcc>(TAcc acc, TAcc Function(TAcc acc, int value) combine) {
    for (var value = end - 1; value >= 0; --value) {
      acc = combine(acc, value);
    }
    return acc;
  }
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
    return columns().fold(acc, (acc, col) {
      var height = trees[row][col];
      return combine(acc, col, row, height);
    });
  }

  TAcc foldRowReverse<TAcc>(int row, TAcc acc,
      TAcc Function(TAcc acc, int col, int row, int height) combine) {
    return columns().reverse().fold(acc, (acc, col) {
      var height = trees[row][col];
      return combine(acc, col, row, height);
    });
  }

  TAcc foldColumn<TAcc>(int col, TAcc acc,
      TAcc Function(TAcc acc, int col, int row, int height) combine) {
    return rows().fold(acc, (acc, row) {
      var height = trees[row][col];
      return combine(acc, col, row, height);
    });
  }

  TAcc foldColumnReverse<TAcc>(int col, TAcc acc,
      TAcc Function(TAcc acc, int col, int row, int height) combine) {
    return rows().reverse().fold(acc, (acc, row) {
      var height = trees[row][col];
      return combine(acc, col, row, height);
    });
  }

  Range columns() {
    return Range(0, colCount);
  }

  Range rows() {
    return Range(0, rowCount);
  }

  int searchNorth(int col, int row, int height) {
    int count = 0;
    for (var r = row - 1; r >= 0; --r) {
      var other = trees[r][col];
      ++count;
      if (other >= height) {
        break;
      }
    }

    return count;
  }

  int searchEast(int col, int row, int height) {
    int count = 0;
    for (var c = col + 1; c < colCount; ++c) {
      var other = trees[row][c];
      ++count;
      if (other >= height) {
        break;
      }
    }

    return count;
  }

  int searchSouth(int col, int row, int height) {
    int count = 0;
    for (var r = row + 1; r < rowCount; ++r) {
      var other = trees[r][col];
      ++count;
      if (other >= height) {
        break;
      }
    }

    return count;
  }

  int searchWest(int col, int row, int height) {
    int count = 0;
    for (var c = col - 1; c >= 0; --c) {
      var other = trees[row][c];
      ++count;
      if (other >= height) {
        break;
      }
    }

    return count;
  }

  int at(int col, int row) {
    return trees[row][col];
  }
}
