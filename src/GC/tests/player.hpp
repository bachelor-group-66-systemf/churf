#include <string>

using std::string; 

class Player {

private:

    string name;
    Point position;
    Point size;
    Point direction;

public:

    Player(string n, Point pos, Point s, Point dir) {
        name = n;
        position = pos;
        size = s;
        direction = dir;
    }

    void move() {
        position.x += direction.x; 
        position.y += direction.y;
    }

    void set_speed(int dx, int dy) {
        direction.x = dx;
        direction.y = dy;
    }

};

struct Point {
    int x, y;
};