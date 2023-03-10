#include <vector>

#include "player.hpp"

#define X_LENGTH 1000;
#define Y_LENGTH 500;

class Game {

private:

    std::vector<Player> players;
    Point dimensions;

public:

    Game() {
        dimensions = {X_LENGTH, Y_LENGTH};
    }

    void add_player(Player& p) {
        players.push_back(p);
    }

};