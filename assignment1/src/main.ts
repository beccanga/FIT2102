import "./style.css";
import { interval, fromEvent, merge} from 'rxjs'
import { map, scan, filter} from 'rxjs/operators'
type Key = "w" | "a" | "s" | "d" | "Space";
type Event = "keydown" | "keyup" ;

function main() {
  /**
   * Inside this function you will use the classes and functions from rx.js
   * to add visuals to the svg element in pong.html, animate them, and make them interactive.
   *
   * Study and complete the tasks in observable examples first to get ideas.
   *
   * Course Notes showing Asteroids in FRP: https://tgdwyer.github.io/asteroids/
   *
   * You will be marked on your functional programming style
   * as well as the functionality that you implement.
   *
   * Document your code!
   */

  /**
   * This is the view for your game to add and update your game elements.
   */
  const svg = document.querySelector("#svgCanvas") as SVGElement & HTMLElement;
  
  // Various game transitions within this game
  // Move is for moving horizontally and MoveVertically is for moving vertically (frog use)
  class Move {constructor(public readonly direction: number) {}}
  class MoveVertically {constructor(public readonly direction: number) {}}
  class Tick{constructor(public readonly elapsed: number){}}
  class Restart {constructor() {}}

  // Game clock is for the entire game, to check the position of frog during the game
  const gameClock = interval(10).pipe(map(elapsed => new Tick(elapsed)))
  // Moving the frog 
  // frog to move forward, backward, left and right using keyboard //creating an observable
  const keyObservable = <T>(e:Event, k:Key, result:()=>T)=>
  fromEvent<KeyboardEvent>(document,e)
    .pipe(filter(event => event.key == k),map(result)),
  upMove = keyObservable("keydown", "w", () => new MoveVertically(-10)),
  leftMove = keyObservable("keydown", "a", () => new Move(-10)),
  downMove = keyObservable("keydown", "s", () => new MoveVertically(10)),
  rightMove = keyObservable("keydown", "d", () => new Move(10)),
  restart = keyObservable("keydown", "Space", () => new Restart()); 

  //Declaring types for objects, so that the objects will not be mutable after this
  type frog = Element;
  type car = ReadonlyArray<Element>;
  type river = ReadonlyArray<Element>;
  type ground = ReadonlyArray<Element>;
  type otherObjects = ReadonlyArray<Element>;
  type plank = ReadonlyArray<Element>;
  type log = ReadonlyArray<Element>;

  //Game state, using the Readonly will help us maintain functional purity.
  type State = Readonly<{
    x: number,
    y: number,
    ground: ground,
    river: river,
    log: log,
    plank: plank,
    car: car,
    otherObjects: otherObjects,
    frog: frog,
    score: number, // keep track of frog's scores, when reached target
    status: ReadonlyArray<Element>,
    exit: ReadonlyArray<Element>, // keep tracks of every body which needs to be removed when svg is being updated
    gameOver: boolean,
    winGame: boolean;
  }>;

  // ===========================================================================================
  // Blue long rect, representing river/water
  // ===========================================================================================  
  function createRiver(): river{
    const river = document.createElementNS(svg.namespaceURI, "rect");
    river.setAttribute("id","river")
    river.setAttribute("x", "0");
    river.setAttribute("y", "410");
    river.setAttribute("width", "600");
    river.setAttribute("height", "95");
    river.setAttribute("fill","#0379B2"); //0379b2#120377
    svg.appendChild(river);
    return [river];
  }

  // ===========================================================================================
  // Green long rect, representing ground
  // ===========================================================================================  
  function createGround(): ground {
    const ground = document.createElementNS(svg.namespaceURI, "rect");
    ground.setAttribute("id","ground");
    ground.setAttribute("x", "0");
    ground.setAttribute("y", "240");
    ground.setAttribute("width", "600");
    ground.setAttribute("height", "140");
    ground.setAttribute("fill","#334D02"); //4D4802#334D02
    ground.setAttribute("stroke", "black")
    ground.setAttribute("stroke-width","2px")
    svg.appendChild(ground);
    return [ground];
  }

  // ===========================================================================================
  // Car objects, representing various cars
  // ===========================================================================================  
  function createCar():car{
    const car = document.createElementNS(svg.namespaceURI, "rect");
    car.setAttribute("id","car");
    car.setAttribute("x","500");
    car.setAttribute("y","250");
    car.setAttribute("width","30");
    car.setAttribute("height","30");
    car.setAttribute("fill","#F1C40F");

    //Make car move right to left at different speed
    const animateCar= setInterval(
    () => car.setAttribute("x", String(-1 + Number(car.getAttribute("x")))),10);
    const carTimer = setInterval(() => {
    clearInterval(animateCar);
    clearInterval(carTimer);
   }, 4000);

    //Car #2
    const car2 = document.createElementNS(svg.namespaceURI, "rect");
    car2.setAttribute("id","car2");
    car2.setAttribute("x","300");
    car2.setAttribute("y","250");
    car2.setAttribute("width","30");
    car2.setAttribute("height","30");
    car2.setAttribute("fill","#E26A6A");

    //Make car 2 move left to right at different speed
    const animateCar2= setInterval(
    () => car2.setAttribute("x", String(1 + Number(car2.getAttribute("x")))),10);
    const carTimer2 = setInterval(() => {
    clearInterval(animateCar2);
    clearInterval(carTimer2);
   }, 2500);

    //Car #3
    const car3 = document.createElementNS(svg.namespaceURI, "rect");
    car3.setAttribute("id","car3");
    car3.setAttribute("x","100");
    car3.setAttribute("y","250");
    car3.setAttribute("width","30");
    car3.setAttribute("height","30");
    car3.setAttribute("fill","#B1E9FA");

    //Make car 3 move left to right at different speed
    const animateCar3= setInterval(
      () => car3.setAttribute("x", String(1 + Number(car3.getAttribute("x")))),10);
      const carTimer3 = setInterval(() => {
      clearInterval(animateCar3);
      clearInterval(carTimer3);
     }, 4000);

    svg.appendChild(car);
    svg.appendChild(car2);
    svg.appendChild(car3);
    return [car, car2, car3];
  }

  // ===========================================================================================
  // Brown rect objects, representing various sizes planks in the river
  // ===========================================================================================  
  function createPlank(): plank {
    // this plank is like bridge (for frog to reach target easily)
    const plank = document.createElementNS(svg.namespaceURI, "rect");
    plank.setAttribute("id","plank");
    plank.setAttribute("x", "270");
    plank.setAttribute("y", "410"); 
    plank.setAttribute("width", "55");
    plank.setAttribute("height", "95");
    plank.setAttribute("fill","#946635");

    //Plank #2
    const plank2 = document.createElementNS(svg.namespaceURI, "rect");
    plank2.setAttribute("id","plank2");
    plank2.setAttribute("x", "400");
    plank2.setAttribute("y", "430"); 
    plank2.setAttribute("width", "70");
    plank2.setAttribute("height", "40");
    plank2.setAttribute("fill","#946635");

    //Plank #3
    const plank3 = document.createElementNS(svg.namespaceURI, "rect");
    plank3.setAttribute("id","plank3");
    plank3.setAttribute("x", "60");
    plank3.setAttribute("y", "420"); 
    plank3.setAttribute("width", "80");
    plank3.setAttribute("height", "50");
    plank3.setAttribute("fill","#946635");

    //Plank #4
    const plank4 = document.createElementNS(svg.namespaceURI, "rect");
    plank4.setAttribute("id","plank4");
    plank4.setAttribute("x", "420");
    plank4.setAttribute("y", "473"); 
    plank4.setAttribute("width", "120");
    plank4.setAttribute("height", "30");
    plank4.setAttribute("fill","#946635");   
     
    //Plank #5
    const plank5 = document.createElementNS(svg.namespaceURI, "rect");
    plank5.setAttribute("id","plank5");
    plank5.setAttribute("x", "485");
    plank5.setAttribute("y", "435"); 
    plank5.setAttribute("width", "50");
    plank5.setAttribute("height", "35");
    plank5.setAttribute("fill","#946635");   

    //Plank #6
    const plank6 = document.createElementNS(svg.namespaceURI, "rect");
    plank6.setAttribute("id","plank6");
    plank6.setAttribute("x", "145");
    plank6.setAttribute("y", "425"); 
    plank6.setAttribute("width", "35");
    plank6.setAttribute("height", "70");
    plank6.setAttribute("fill","#946635");   

    //Plank #7
    const plank7 = document.createElementNS(svg.namespaceURI, "rect");
    plank7.setAttribute("id","plank7");
    plank7.setAttribute("x", "330");
    plank7.setAttribute("y", "430"); 
    plank7.setAttribute("width", "50");
    plank7.setAttribute("height", "60");
    plank7.setAttribute("fill","#946635");   

    svg.appendChild(plank);
    svg.appendChild(plank2);
    svg.appendChild(plank3);
    svg.appendChild(plank4);
    svg.appendChild(plank5);
    svg.appendChild(plank6);
    svg.appendChild(plank7);
    return [plank, plank2, plank3, plank4, plank5, plank6, plank7];
  }

  // ===========================================================================================
  // Brown rect, representing target area, calling them logs
  // =========================================================================================== 
  function createLog(): log {
    const log = document.createElementNS(svg.namespaceURI, "rect");
    log.setAttribute("x", "250");
    log.setAttribute("y", "551");
    log.setAttribute("width", "80");
    log.setAttribute("height", "45");
    log.setAttribute("fill","#583918");
    log.setAttribute("stroke","#583918");
    log.setAttribute("stroke-width","5px");

    const log2 = document.createElementNS(svg.namespaceURI, "rect");
    log2.setAttribute("x", "450");
    log2.setAttribute("y", "551");
    log2.setAttribute("width", "80");
    log2.setAttribute("height", "45");
    log2.setAttribute("fill","#583918");
    log2.setAttribute("stroke","#583918");
    log2.setAttribute("stroke-width","5px");

    const log3 = document.createElementNS(svg.namespaceURI, "rect");
    log3.setAttribute("x", "70");
    log3.setAttribute("y", "551");
    log3.setAttribute("width", "80");
    log3.setAttribute("height", "45");
    log3.setAttribute("fill","#583918");
    log3.setAttribute("stroke","#583918");
    log3.setAttribute("stroke-width","5px");

    svg.appendChild(log);
    svg.appendChild(log2);
    svg.appendChild(log3);
    return [log, log2, log3];
  }
  // ===========================================================================================
  // Other objects, representing other various objects like stone, bee, snail, snake
  // =========================================================================================== 
  function createObjects(): otherObjects{
    const stone = document.createElementNS(svg.namespaceURI,"rect");
    stone.setAttribute("id","stone");
    stone.setAttribute("x", "380");
    stone.setAttribute("y", "300");
    stone.setAttribute("width", "20");
    stone.setAttribute("height", "20");
    stone.setAttribute("fill", "grey");  
    
    const bee = document.createElementNS(svg.namespaceURI,"rect");
    bee.setAttribute("id","bee");
    bee.setAttribute("x", "300");
    bee.setAttribute("y", "300");
    bee.setAttribute("width", "20");
    bee.setAttribute("height", "20");
    bee.setAttribute("fill", "yellow");  

  //Move object bee, moves(flies) left to right in slow speed
  const animateBee= setInterval(
    () => bee.setAttribute("x", String(1 + Number(bee.getAttribute("x")))),
    100
    );
    const beeTimer = setInterval(() => {
    clearInterval(animateBee);
    clearInterval(beeTimer);
    }, 2000);

    const snail = document.createElementNS(svg.namespaceURI,"rect");
    snail.setAttribute("id","snail");
    snail.setAttribute("x", "180");
    snail.setAttribute("y", "300");
    snail.setAttribute("width", "20");
    snail.setAttribute("height", "20");
    snail.setAttribute("fill", "#946635");

    //Move object snail, moves right to left in slow speed
    const animateSnail= setInterval(
      () => snail.setAttribute("x", String(-1 + Number(snail.getAttribute("x")))),
      100
      );
      const snailTimer = setInterval(() => {
      clearInterval(animateSnail);
      clearInterval(snailTimer);
     }, 7000);

    //Snake, 1st enemy created
    const snake = document.createElementNS(svg.namespaceURI, "rect");
    snake.setAttribute("id","snake")
    snake.setAttribute("x", "20");
    snake.setAttribute("y", "560");
    snake.setAttribute("width", "20");
    snake.setAttribute("height", "70");
    snake.setAttribute("fill","yellow");
    snake.setAttribute("stroke","#E26A6A");
    snake.setAttribute("stroke-width","2px");

    //Move object snail, moves up to the ground from the logs when game starts
    const animateSnake= setInterval(
      () => snake.setAttribute("y", String(1 + Number(snail.getAttribute("y")))),
      1000
      );
      const snakeTimer = setInterval(() => {
      clearInterval(animateSnake);
      clearInterval(snakeTimer);
     }, 10000);

    svg.appendChild(stone);
    svg.appendChild(bee);
    svg.appendChild(snail);
    svg.appendChild(snake);
    return [stone, bee, snail, snake];
  }
  
  // ===========================================================================================
  // Main frog object
  // =========================================================================================== 
  function createFrog(): frog{
    const frog = document.createElementNS(svg.namespaceURI, "circle") ;
    frog.setAttribute("id","frog");
    frog.setAttribute("r", "20");
    frog.setAttribute("cx", "300");
    frog.setAttribute("cy", "50");
    frog.setAttribute(
    "style",
    "fill: green; stroke: green; stroke-width: 1px;"
  );
    svg.appendChild(frog);
    return frog;
  }

  //Initial state, keeping track of each game state as the game is progressing 
  const initialState:State = {
    x: 270, // the x and y here shows where the frog should be at every start of the game
    y: 30,
    ground: createGround(),
    river: createRiver(),
    log: createLog(),
    plank: createPlank(),
    car: createCar(),
    otherObjects: createObjects(), 
    frog: createFrog(),
    score: 0, //score will be 0 until the frog lands in the log/target area
    status: [],
    exit: [],
    gameOver: false,
    winGame: false
  }

  //All collision will be handled within this function and the game will either be over or user will win the game
  /**
   * handleCollision function handles the collisions from the main frog object and other elements
   * This function is referenced from : https://developer.mozilla.org/en-US/docs/Games/Techniques/2D_collision_detection
   * @param s State
   * @returns boolean of the collision occured
   */
  function handleCollision(s:State){
    //using frog from state
    const frog = s.frog;
    function collided(element:Element){
      const distanceX = Math.abs(Number(frog.getAttribute("cx")) - Number(element.getAttribute("x")) - Number(element.getAttribute("width"))/2);
      const distanceY = Math.abs(Number(frog.getAttribute("cy")) - Number(element.getAttribute("y")) - Number(element.getAttribute("height"))/2);
      
      if (distanceX > (Number(element.getAttribute("width")))/2 + Number(frog.getAttribute("r"))) { return false; }
      if (distanceY > (Number(element.getAttribute("height")))/2 +  Number(frog.getAttribute("r"))) { return false; }
      if (distanceX <= (Number(element.getAttribute("width")))/2) { return true; } 
      if (distanceY <= (Number(element.getAttribute("height")))/2) { return true; }

      const checkCorner = (distanceX  - Number(element.getAttribute("width"))/2)^2 +(distanceY - Number(element.getAttribute("height"))/2)^2;
    return (checkCorner <= ((Number(frog.getAttribute("r")))^2));
  }
  
  //Conditions for different type of collisions, to check whether the frog has collided with any other elements/objects
  const win = (s:State) =>
  s.log.length > 0 ? {...s, winGame: true, score: s.score + 1} : {...s, score: 0}
  
  //if the frog collide with car, game would be over
  return s.car.filter(collided).length > 0 ? {...s, gameOver: true}:

    //collision with other objects in ground, game would be over
    s.otherObjects.filter(collided).length > 0 ? {...s, gameOver: true}:

    // if frog touch river, frog dies & game would be over
    s.river.filter(collided).length > 0 && s.plank.filter(collided).length == 0
    ? {...s, gameOver: true}:

    // if frog on plank, game would not be over
    s.plank.filter(collided).length > 1 ? {...s, gameOver: false}:

    // if frog collide with target/log, win game
    s.log.filter(collided).length > 0 ? win({...s,log:s.log.filter(not(collided))}): s
  }

  //This function is used to check the states of the objects and handles the functionalities of the game state at every interval while game is running
  function tick(s:State, elapsed: number){
    const frog = document.getElementById("frog")!
    return handleCollision({...s,
    frog: frog})
  }

  //To keep track of the changes from all states according to various transitions
  const reduceState=(s: State, e: Move| MoveVertically | Tick | Restart)=>
  (e instanceof Move ? 
   {...s, x: s.x + e.direction}:
   e instanceof MoveVertically ?
   {...s, y: s.y + e.direction}:
   e instanceof Restart ? //restart the game when the key is pressed
   {...initialState, exit: s.exit.concat(s.frog).concat(s.car).concat(s.otherObjects).concat(s.plank)} :
   tick(s, e.elapsed))

  //Subscription to the svg by altering the initialState by using reduceState 
  const subscribe =  merge(leftMove, upMove, rightMove, downMove, restart, gameClock)
    .pipe(scan(reduceState,initialState)).subscribe(updateView)

  //All the changes to the state is updated here
  function updateView(s:State){
    s.frog.setAttribute("cx",String(s.x));
    s.frog.setAttribute("cy",String(s.y)); 

    //Game ends when frog dies/collides
    if (s.gameOver){
      subscribe.unsubscribe();
      const status = document.getElementById("currentStatus");
      const gameOver = "<p> Frog has died! </p>";
      status?.insertAdjacentHTML("afterend", gameOver);
      status?.remove(); //remove previous status

      //show status of game on the game screen
      const loseGameStatus = document.createElementNS(svg.namespaceURI, "text")!;
      loseGameStatus.setAttribute("x","50");
      loseGameStatus.setAttribute("y","100")
      loseGameStatus.setAttribute("fill","grey");
      loseGameStatus.setAttribute("font-size","30px");
      loseGameStatus.textContent ="Game Over! Restart game to try again!"
      svg.append(loseGameStatus)
    }
   
    //Win game when frog reach target
    if(s.winGame){
      subscribe.unsubscribe()
      const status = document.getElementById("currentStatus");
      const winGame = "<p> Hit Target! You have arrived at the log! </p>";
      status?.insertAdjacentHTML("afterend", winGame);
      status?.remove(); //remove previous status
      
      //increment score board for reaching target
      const scoreboard = document.getElementById("score")
      scoreboard?.insertAdjacentHTML("afterend", "<p>" + String(s.score)+ "</p>");
      scoreboard?.remove();

      //show status of game on the game screen
      const winGameStatus = document.createElementNS(svg.namespaceURI, "text")!;
      winGameStatus.setAttribute("x","240");
      winGameStatus.setAttribute("y","100")
      winGameStatus.setAttribute("fill","grey");
      winGameStatus.setAttribute("font-size","30px");
      winGameStatus.textContent ="You Win!"
      svg.append(winGameStatus) 
    }
    //Restart game
    if (s.exit) {
      s.exit.map(o => document.getElementById(o.id))
          .filter(isNotNullOrUndefined)
          .forEach(v => svg.removeChild(v))
    }
  }
}

// The following simply runs your main function on window load.  Make sure to leave it in place.
if (typeof window !== "undefined") {
  window.onload = () => {
    main();
  };
}


// ===========================================================================================
// Ultility functions (implemented from Asteroid's functions)
// =========================================================================================== 
  /**
  * Composable not: invert boolean result of given function
  * @param f a function returning boolean
  * @param x the value that will be tested with f
  */  
  const not = <T>(f:(x:T)=>boolean)=> (x:T)=> !f(x),

  /**
  * set a number of attributes on an Element at once
  * @param e the Element
  * @param o a property bag
  */
  attr = (e: Element, o: Object) => {
  for (const k in o) e.setAttribute(k, String(o[k as keyof object ]));
  }
  /**
  * Type guard for use in filters
  * @param input something that might be null or undefined
  */
  function isNotNullOrUndefined<T extends Object>(input: null | undefined | T): input is T {
    return input != null;
  }