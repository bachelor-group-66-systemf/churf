#include <string>

using std::string; 

class Point {

public:

    int x, y;
    Point(int _x, int _y) : x(_x), y(_y) {} 
};

class Player {

private:

    string name;
    Point position;
    Point size;
    Point direction;

public:

    Player(string n, Point pos, Point s, Point dir) 
        : name(n), position(pos.x, pos.y), size(s.x, s.y), direction(dir.x, dir.y)
    {}

    void move() {
        position.x += direction.x; 
        position.y += direction.y;
    }

    void set_speed(int dx, int dy) {
        direction.x = dx;
        direction.y = dy;
    }

};
