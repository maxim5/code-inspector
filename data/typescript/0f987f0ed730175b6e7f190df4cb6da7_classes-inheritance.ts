class Man {
    
    constructor (private description: string) { 
    }
    
    public getPower() {
        return 1;
    }
   
    getDescription() {
        return this.description;
    };
}

class Superhero extends Man {
     getPower() {
        return super.getPower()*2;
    };
}

class Batman extends Superhero {
     getPower() {
        return super.getPower()*3;
    }
}

class Superman extends Superhero {
     getPower() {
        return super.getPower()*10;
    }
}



var batman = new Batman("Drives batmobile");
console.log("Batman's power level " + batman.getPower());
console.log("Batman: " + batman.getDescription());

// Javascript type checks
console.log("Batman is of type Batman");
console.log(batman instanceof Batman);

console.log("Batman is of type Superhero");
console.log(batman instanceof Superhero);

console.log("Batman is of type Man");
console.log(batman instanceof Man);

console.log("Batman is of type Superman");
console.log(batman instanceof Superman);



var superman = new Superman("Flies at super speed");
console.log("Superman's power level " + superman.getPower());
console.log("Superman: " + superman.getDescription());

// Javascript type checks
console.log("Superman is of type Superman");
console.log(superman instanceof Superman);

console.log("Superman is of type Superhero");
console.log(superman instanceof Superhero);

console.log("Superman is of type Man");
console.log(superman instanceof Man);

console.log("Superman is of type Batman");
console.log(superman instanceof Batman);