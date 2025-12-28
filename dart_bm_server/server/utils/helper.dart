import 'dart:math';

Point<double> preciseAdd(Point point, Point value) {
  double factor = 100.0;
  return Point(
    ((point.x * factor) + (value.x * factor)).round() / factor,
    ((point.y * factor) + (value.y * factor)).round() / factor,
  );
}
