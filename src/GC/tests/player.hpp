#include <string>

using std::string; 

class Point {

public:

    int x, y;
    Point() {}
    Point(int _x, int _y) : x(_x), y(_y) {} 
};

class Player {

private:

    string *name;
    Point *position;
    Point *size;
    Point *direction;

public:

    Player() {}

/*     Player(string n, Point pos, Point s, Point dir) 
        : name(n), position(pos.x, pos.y), size(s.x, s.y), direction(dir.x, dir.y)
    {} */

    void move() {
        position->x += direction->x; 
        position->y += direction->y;
    }

    void set_speed(int dx, int dy) {
        direction->x = dx;
        direction->y = dy;
    }

    // This is probably neccessary to initialize an object with our GC
    // Since allocation and construction cannot be done at the same time
    void init(string *n, Point *pos, Point *s, Point *dir) {
        name = n;
        position = pos;
        size = s;
        direction = dir;

    }

};
