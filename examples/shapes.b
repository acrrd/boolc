
class Main {
  int main() {
    Shape s1; s1 = new Rectangle(10, 20, 5, 6);
    Shape s2; s2 = new Circle(15, 25, 8);
    
    this.moveAndDraw(s1);
    this.moveAndDraw(s2);

    Rectangle rect;
    rect = (Rectangle) s1;
    rect.setWidth(30);
    rect.draw();
    //rect = (Rectangle) s2;
  }

  void moveAndDraw(Shape s)
  {
    s.draw();
    s.rMoveTo(100, 100);
    s.draw();
  } 
}

class Shape {
  int x;
  int y;

  int getX() { return this.x; }
  int getY() { return this.y; }
  void setX(int newx) { this.x = newx; }
  void setY(int newy) { this.y = newy; }

  void moveTo(int newx, int newy) {
    this.setX(newx);
    this.setY(newy);
  }
  void rMoveTo(int deltax, int deltay) {
     this.moveTo(this.getX() + deltax, this.getY() + deltay);
  }

  void draw() {}
}

class Rectangle extends Shape {
  int width;
  int height;

  int getWidth() { return this.width; }
  int getHeight() { return this.height; }
  void setWidth(int newwidth) { this.width = newwidth; }
  void setHeight(int newheight) { this.height = newheight; }

  void draw() {
    System.print("Drawing a Rectangle at:("); System.print(this.getX());
    System.print(", "); System.print(this.getY()); System.print("), width ");
    System.print(this.getWidth()); System.print(", height "); System.println(this.getHeight());
  }
}

class Circle extends Shape {
  int radius;

  int getRadius() { return this.radius; }
  void setRadius(int newradius) { this.radius = newradius; }

  void draw() {
    System.print("Drawing a Circle at:("); System.print(this.getX()); System.print(", ");
    System.print(this.getY()); System.print("), radius "); System.println(this.getRadius());
  }
}

