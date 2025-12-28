class GameConstants {
  static const int gridRows = 13;
  static const int gridCols = 15;
  static const int defaultPort = 15800;
  static const int defaultMaxPlayers = 2;
  static const int defaultGameDuration = 60;
  static const int minGameDuration = 38;
  static const int maxGameDuration = 600;
  static const int bombExplosionTime = 3; // seconds
  static const int explosionDuration = 1; // seconds
  static const double softBlockSpawnChance = 0.4; // 40%
  static const double powerupSpawnChance = 0.1; // 10%
  static const double playerWidth = 0.5;

  static const double baseSpeed = 0.1;
  static const double speedUpIncrement = 0.5;

  static const int serverTickRate = 60;
}
