
interface ISnapOptions {
  element:HTMLElement;
  dragger?:any;
  disable?:string;
  addBodyClasses?:boolean;
  hyperextensible?:boolean;
  resistance?:number;
  flickThreshold?:number;
  transitionSpeed?:number;
  easing?:string;
  maxPosition?:number;
  minPosition?:number;
  tapToClose?:boolean;
  touchToDrag?:boolean;
  slideIntent?:number; // degrees
  minDragDistance?:number;
}

interface ISimpleStates {
  opening:string;
  towards:string;
  hyperExtending:boolean;
  halfway:boolean;
  flick:boolean;
  translation: {
    absolute:number;
    relative:number;
    sinceDirectionChange:number;
    percentage:number;
  }
}

interface ISnapState {
  state:string;
  info: ISimpleStates;
}

declare module 'snap' {

  class Snap {
    constructor(userOpts:ISnapOptions);
    open(side:string):void;
    close():void;
    state():ISnapState;
    enable():void;
    disable():void;
  }

  export = Snap;
}
