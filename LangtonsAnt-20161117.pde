int gridSize = 5;
int antX, antY, boardW, boardH;
// 0 - up, 1 - right, 2 - down, 3 - left
// right = +1 mod4, left = -1 mod4.
int antDirection = 0;
// 0 - black, 1 - white;
int[][] board;
PImage black, white, ant;

void initImages() {
  black = createImage(gridSize,gridSize,RGB);
  white = createImage(gridSize,gridSize,RGB);
  ant = createImage(gridSize,gridSize,RGB);
  for(int i=0;i<5;i++) for(int j=0;j<5;j++)
    { black.pixels[i*gridSize+j]=color(0);
      white.pixels[i*gridSize+j]=color(255);
      ant.pixels[i*gridSize+j]=color(255,0,0); }
  black.updatePixels(); white.updatePixels(); ant.updatePixels();
}

void initBoard() {
  board = new int[ceil(width/float(gridSize))]
                 [ceil(height/float(gridSize))];
  for(int i=0;i<width/gridSize;i++)
    for(int j=0;j<height/gridSize;j++)
      board[i][j] = 1;
  boardW = width/gridSize; boardH = height/gridSize;
}

void turnLeft() {
  antDirection=(antDirection==0?3:antDirection-1);
}
void turnRight() {
  antDirection=(antDirection==3?0:antDirection+1);
}
void stepOut() {
  // 0 - x+0, y+(-1)
  // 1 - x+1, y+0
  // 2 - x+0, y+1
  // 3 - x+(-1), y+0
  antX+=denoteX(antDirection);
  antY+=denoteY(antDirection);
} int denoteX(int d) { return (d%2==0?0:2-d); }
  int denoteY(int d) { return (d%2==0?d-1:0); }

void flipColor() { board[antX][antY]=flip(board[antX][antY]); }
int flip(int x) { return (x==0?1:0); }

int determineNow(int x,int y) {
  if(x==antX&&y==antY) return 2;
  else return (board[x][y]);
}
void dispFunc() {
  for(int i=0;i<boardW;i++) for(int j=0;j<boardH;j++)
    { int now = determineNow(i,j);
      image((now==0?black:
             now==1?white:
             ant),i*gridSize,j*gridSize); }
}

void setup() {
  size(600,600);
  background(255);
  System.out.println("Langton's ant.");
  // init the board
  initBoard();
  // put the ant
  antX = boardW/2; antY = boardH/2;
  antDirection = 0;
  // prepare the images
  initImages();
}
void draw() {
  int under = board[antX][antY];
  if(under==0) turnRight(); else turnLeft();
  flipColor(); stepOut();
  dispFunc();
}
