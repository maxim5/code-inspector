//Here lies the algorithm for Diplomacy
//http://www.floc.net/dpjudge/?page=Algorithm

//input game state and list of moves
//output new game state



function sameToFrom(x,y)
{
  return (x.order.from==y.order.from) && (x.order.to==y.order.to)
}

//list: list of provinces
//goal: target province
//return: true/false if a fleet can move there
function validMove(list,goal,type)
{
  if(type=="f" || type=="F")
    for(x in list)
    {
      if($.inArray(goal,MAP[list[x]].fleet_moves))
        return true;
    }
  else if (type=="a" || type=="A")
    for(x in list)
    {
      if($.inArray(goal,MAP[list[x]].army_moves))
        return true;
    }
  return false;
}

function invalidateUnit(unit)
{
  unit.order.tag="invalid";
  unit.order.move="h";
  unit.order.from=unit.province;
  unit.order.to=unit.province;
}


//constants.
var MAP = {
  NAt : {fullname: "North Atlantic",
        army_moves: [],
        fleet_moves: ["Cly","Lvp","Iri","Mid","Nrg"],
        belongsto: "",
        supply: 0,
        combatlist: []},
  Nrg : {fullname: "Norwegian Sea",
        army_moves: [],
        fleet_moves: ["Edi","Nth","Nwy","Bar","NAt","Cly"],
        belongsto: "",
        supply: 0,
        combatlist: []},
  Nth : {fullname: "North Sea",
        army_moves: [],
        fleet_moves: ["Edi","Nrg","Nwy","Ska","Den","Hel","Hol","Bel","Eng","Lon","Yor"],
        belongsto: "",
        supply: 0,
        combatlist: []},
  Cly : {fullname: "Clyde",
        army_moves: ["Edi","Lvp"],
        fleet_moves: ["Edi","Lvp","Nrg","NAt"],
        belongsto: "Eng",
        supply: 0,
        combatlist: []},
  Edi : {fullname: "Edinburgh",
        army_moves: ["Cly","Lvp","Yor",],
        fleet_moves: ["Cly","Nth","Yor","Nrg"],
        belongsto: "Eng",
        supply: 1,
        combatlist: []},
  Lvp : {fullname: "Liverpool",
        army_moves: ["Edi","Cly","Yor","Wal"],
        fleet_moves: ["Cly","Atl","Wal","Iri"],
        belongsto: "Eng",
        supply: 1,
        combatlist: []},




};

var COUNTRY = ["Aus","Eng","Fra","Ger","Ita","Rus","Tur"];


//example use: valid("Lvp","Edi","A")
function valid(from,to,type)
{
  if(type=="A" || type == "a")
    return ( _.include(MAP[from].army_moves,to));
  else if (type=="F" || type=="f")
    return _.include(MAP[from].fleet_moves,to);
  console.log("invalid army type encountered")
}


/*
cutSupport, executed for a particular moving unit
    If the move is to a location occupied by a supporting unit that fits all of the following criteria:

        is not already marked "cut" or "void,"
        is not owned by the same power as the moving unit,
        if (and only if) the moving unit is a convoyed army, the supporting unit is not offering support for or against any convoying fleet (whose order is not marked "void"), and
        either we are executing Step 9 of the main algorithm or the supporting unit is not offering support for a move into the space from which the moving unit originated. 

    then do the following:

        Mark the supporting unit "cut"
        Decrement the number of supports that the supported unit has.
        Remove the supporting unit from the supported unit's "no help list" (if it appears there). 
*/
function cutSupport(unit)
{
  
}








/*
For all convoying armies:

    If trying to convoy through a non-existent fleet, either change the army's order to HOLD or mark the army "no convoy" (the decision as to which should be done is based on a game option).
    Otherwise, if trying to convoy through a fleet that did not order a matching convoy order, mark the army "no convoy."
    Otherwise, include this army in a "convoying armies list." 
*/
function resolveconvoyarmy(units, convoyarmylist)
{
  //select all convoying armies
  var convoy = _.select(units,function(unit){
    return ( (unit.order.move=="c") && (unit.utype=="a") );
  });

  //remove invalid convoy orders
  for(x in convoy)
  {
    if(unit.province!=unit.order.from)
      invalidateUnit(convoy[x]);
    convoy.splice(x,1);
  }

  //for each convoying army
  for(x in convoy)
  {
    //find all fleet with matching order and in water
    var fleet = _.select(units,function(f){
      return ( (f.order.move=="c") && (f.utype=="f")
      && (f.order.from==convoy[x].order.from)
      && (f.order.to==convoy[x].order.to)
      && (MAP[f.province].army_moves==[]) /*in water*/ );
    });

    var working = convoy[x].province;//set default BFS to army's prov

    while(1)
    {
      var w2;
      for (y in fleet)//for each remaining fleet of interest
      {
        if(validMove(working,fleet[y],"F"))//check if it is connected to BFS leading edge
          w2.push(fleet[y]);//if so, add to w2
      }
      
      working = w2;//w2 is now the new leading edge
      fleet = _.difference(fleet,working);//remove working form fleet

      if(validMove(working,goal,"F"))//we found a path to goal
      {
        convoyarmylist.push(convoy[x]);
        break;
      }
      if(fleet==[] || working==[])//end of BFS, cannot find goal
      {
        invalidateUnit(convoy[x]);
        break;
      }
    }
  }
}

/*
For all fleets issuing convoy orders:

    If the army being convoyed does not exist, either change the fleet's order to HOLD or mark the fleet "void" (the decision as to which should be done is based on a game option).
    Otherwise, if the army being convoyed did not issue the matching order, mark the fleet "void." 
*/
function resolveconvoyfleet(units)
{
  //for all valid convoying armies
  var convoy = _.select(units,function(unit){
    return ( (unit.order.move=="c") && (unit.utype=="a")
      && (unit.province==unit.order.from) );
  });
  //list all convoying fleets
  var allfleets = _.select(units,function(f){
    return ( (f.order.move=="c") && (f.utype=="f"));
  });
  var validfleets;
  for(x in convoy)//for all concoying armies
  {
    validfleet.concat( _.select(allfleets,function(f){
      return ( sameToFrom(f,convoy[x])
      && (MAP[f.province].army_moves==[]) /*in water*/ );
    }));
  }

  var invalidfleets = _.difference(allfleets,validfleets)
  for (x in invalidfleets)
  {
    invalidateUnit(invalidfleets[x]);
  }
}

/*
For all units issuing support orders:

    If the unit being supported does not exist, either change the supporting unit's order to HOLD or mark it "void" (the decision as to which should be done is based on a game option)
    Otherwise, if the unit being supported did not issue the matching order, mark the supporting unit "void."
    Otherwise:
        Increment the "support count" of the supported unit.
        If the supported unit is attacking a position that is occupied by a unit owned by the same power that owns the supporting unit, add the supporting unit to the supported unit's "no help list." 
        */
function markInvalidSupport(units)
{
  //support: all units issuing support order
  var support = _.select(units,function(unit){
    return (unit.order.move=="s");
  });
  //does unit being supported exist, and is it valid?
  var validsup = _.select(support, function(u){
    for(x in units)
    {
      if(units[x].province==u.order.from)//if u is supporting unit[x]
      {  
       if( sameToFrom(unit[x],u)
          && validMove(MAP[u.province],u.order.to,u.utype) )
          return true;
        else
          return false;
      }
    }
    return false;
  });
  //invalidate invalid support
  var invalidsup=_.difference(support,validsupport);
  for (x in invalidsup)
  {
    invalidateUnit(invalidsup[x]);
  }
  //count up support
  for (x in validsup)
  {
    if(validsup[x].owner!=units[validsup[x].order.to].owner)//not trying to displace your own
      unit[validsup[x].order.from].order.support+=1;
    else//trying to displace your own
      unit[validsup[x].order.from].order.nohelp.push(validsup[x].province);
  }
}

/*
    For all non-convoyed move orders:
        Execute the cutSupport procedure (found below). 
    For every space on the board:
        Create a "combat list" containing all units that are attempting to either move to or remain in that space. 
*/

function calcInitStr(units)
{
  for(x in units)
  {
    if(units[x].order.move!="c")
      cutSupport(unit[x]);
    
    if(units[x].order.move=="m" || units[x].order.move=="h")
      MAP[units[x].order.to].combatlist.push(unit[x].province);
  }
}












function resolve(game)
{
  //Does "owner" have right to move "order.from"
  var units = game.units;//TODO: magic
  var convoyarmylist;
  var convoysuccesslist;

  for(x in units)
  {
    
  }

  resolveconvoyarmy(units,convoyarmylist);
  resolveconvoyfleet(units);
  markInvalidSupport(units);

  //TODO: reset variables
  //MAP[Abc].entrylist[], unit.order.valid, unit.support, unit.nohelp
  return new_game_state;
}











