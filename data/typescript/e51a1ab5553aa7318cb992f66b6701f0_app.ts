var amy = new People.Worker('Amy', 'Walker', 25, Sex.Female);
amy.greet();
amy.getAJob('JavaScript Developer');
amy.getAJob('TypeScript Trainer');
console.log(amy.firstName + ' quit the job as ' + amy.quitAJob('TypeScript Trainer'));

var peter = new People.Student('Peter', 'Johnson', 21, Sex.Male);
peter.greet();
peter.addCourse('Math');
peter.addCourse('Science');
console.log(peter.removeCourse('Math') + ' was removed from the list');

var kitty = new Animals.Cat('Kitty', 2, Sex.Female, 'white');
kitty.greet();

var someToys = new Collections.List<string>();
var sharo = new Animals.DomesticDog('Sharo', 5, Sex.Male, 'dalmatian', someToys);
someToys.add('artificialBone');
sharo.greet();
console.log(someToys.remove('artificialBone') + ' was removed from the list');