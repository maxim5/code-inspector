// Simple class take 1
class Car1 {
    
    private horsePower: number;
    private make: string;
    private model: string;

    constructor (horsePower: number, make: string, model: string) { 
        this.horsePower = horsePower;
        this.make = make;
        this.model = model;
        console.log("Car1 has been created");
    }
    
    getHorsePower() {
        return this.horsePower;
    }
    
    getMake() {
        return this.make;
    }
    
    getModel() {
        return this.model;
    }
}


console.log("Car class Take 1");
var car1 = new Car1(100, "Toyota", "Camry");
console.log("Car's power level (HP)" + car1.getHorsePower());
console.log("Car's make " + car1.getMake());
console.log("Car's model " + car1.getModel());

// Simple class take 2
// identical to take 1 but less verbose
class Car2 {
    
    constructor (private horsePower: number, private make: string, private model: string) { 
        console.log("Car2 has been created");
    }
    
    getHorsePower() {
        return this.horsePower;
    }
    
    getMake() {
        return this.make;
    }
    
    getModel() {
        return this.model;
    }
}


console.log("Car class Take 2");
var car2 = new Car2(100, "Toyota", "Camry");
console.log("Car's power level (HP)" + car2.getHorsePower());
console.log("Car's make " + car2.getMake());
console.log("Car's model " + car2.getModel());
