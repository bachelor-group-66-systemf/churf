#include <vector>

#include "player.hpp"
#include "heap.hpp"

#define X_LENGTH 1000;
#define Y_LENGTH 500;

class Game {

private:

    std::vector<Player> players;
    Point dimensions;

public:

    Game() : dimensions(1000, 500) {}

    void add_player(Player& p) {
        players.push_back(p);
    }

    Player* create_player(string s, Point pos, Point size, Point dir) {
       Player *p = static_cast<Player*>(GC::Heap::alloc(sizeof(Player)));
       p->Player(s, pos, size, dir); // This will probably both be allocated on "our" heap as well as the heap for this program
       return p; 
    }

    std::vector<Player> create_players(int nr) {
        for (int i = 0; i < nr; i++) {
            Player *p = create_player(std::to_string(i), Point(i, i), Point(2, 2), Point(0, 0)); 
            add_player(*p);
        }
    }

};


int main() {
    GC::Heap::init();
    GC::Heap *gc = GC::Heap::debug_the();
    gc->check_init();

    Game *game = static_cast<Game*>(gc->alloc(sizeof(Game)));
    game->create_players(100);

    gc->collect(GC::MARK);     
    gc->print_contents();

    return 0;
}