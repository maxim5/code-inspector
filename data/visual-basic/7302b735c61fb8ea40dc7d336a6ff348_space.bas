function make_spacemap() as short
    dim as short a,f,b,c,d,e,astcou,gascou,x,y,i
    dim as byte makelog=1
    dim as _cords p1,p2,p3
    dim as _planet del 
    dim showclouds as byte
    if makelog=1 then
        f=freefile
        open "creation.log" for output as #f
    endif
    showclouds=0
    set__color( 11,0)
    print
    Print "Generating sector"
    set__color( 7,0)
    for a=0 to max_maps
        for x=0 to 60
            for y=0 to 20
                planetmap(x,y,a)=0
            next
        next
        planets(a)=del
    next
    if makelog=1 then print #f,,"Generated sector"
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    
    for a=0 to 1024
        portal(a).oneway=0
    next
    if makelog=1 then print #f,,"Portals done" &lastitem
    rerollshops
    if makelog=1 then print #f,,"Reroll shops done" &lastitem
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    add_stars
    if makelog=1 then print #f,,"add_stars done" &lastitem
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    add_wormholes
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    if makelog=1 then print #f,,"add_wormholes done" &lastitem
    distribute_stars
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    if makelog=1 then print #f,,"distribute_stars done" &lastitem
    make_clouds()
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    if makelog=1 then print #f,"make_clouds done" &lastitem
    
    gen_traderoutes()
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    
    if makelog=1 then print #f,"gen_traderoutes done" &lastitem
    
    gascou+=1
    for a=0 to laststar
        if spacemap(map(a).c.x,map(a).c.y)<>0 then gascou+=1
    next
    print "stars in gasclouds:";gascou
    set__color( 7,0)
    print
    print "Pregenerating planets ";
    
    add_easy_planets(targetlist(firstwaypoint))
    if makelog=1 then print #f,"add_easy_planets done" &lastitem
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    add_special_planets
    if makelog=1 then print #f,"add_special_planets done" &lastitem
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    add_event_planets
    if makelog=1 then print #f,"addeventplanets done" &lastitem
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    print "checking for starmap errors: ";
    fixstarmap()
    if makelog=1 then print #f,"Fixstarmap" &lastitem
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    add_caves
    if makelog=1 then print #f,"addcaves" &lastitem
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    add_piratebase
    if makelog=1 then print #f,"addpiratbase" &lastitem
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    add_drifters
    if makelog=1 then print #f,"adddrifters" &lastitem
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    
    
    
    print "loading bones"
    loadbones
    if makelog=1 then print #f,"loadbones done" &lastitem
    if makelog=1 then
        for i=0 to lastplanet
            if planets(i).grav=0 and planets(i).temp=0 then print #f,,"Planet "&i &" is empty"
        next
    endif
    
    for a=0 to laststar
        if map(a).discovered=2 then map(a).discovered=show_specials
        if map(a).discovered=3 then map(a).discovered=show_eventp
        if map(a).discovered=4 then map(a).discovered=_debug_bones
        if map(a).discovered=5 then map(a).discovered=show_portals
        if map(a).discovered=6 then map(a).discovered=-1
        if abs(spacemap(map(a).c.x,map(a).c.y))>1 and map(a).spec<8 then
            map(a).spec=map(a).spec+abs(spacemap(map(a).c.x,map(a).c.y))/2
            if map(a).spec>7 then map(a).spec=7
            map(a).ti_no=map(a).spec+68
        endif
        scount(map(a).spec)+=1
        for b=1 to 9
            if map(a).planets(b)<-20000 then gascou+=1
            if map(a).planets(b)>-20000 and map(a).planets(b)<0  then astcou+=1
        next
    next
    if show_specials<>0 then
        for a=0 to laststar
            for b=1 to 9
                if map(a).planets(b)=show_specials then map(a).discovered=1
            next
        next
        if map(a).spec=10 then map(a).discovered=1
    endif
    if show_all_specials=1 then
        for a=0 to laststar
            for b=1 to 9
                for c=0 to lastspecial
                    if map(a).planets(b)=specialplanet(c) then map(a).discovered=1
                next            
            next
        next
    endif
    if show_dangerous=1 then
        for a=0 to laststar
            for b=1 to 9
                if map(a).planets(b)=specialplanet(2) then map(a).discovered=1
                if map(a).planets(b)=specialplanet(3) then map(a).discovered=1
                if map(a).planets(b)=specialplanet(4) then map(a).discovered=1
                if map(a).planets(b)=specialplanet(26) then map(a).discovered=1
                if map(a).planets(b)=specialplanet(27) then map(a).discovered=1
            next
        next
        
    endif
    print

    make_civilisation(0,specialplanet(7))
    if makelog=1 then print #f,"makeciv1 done" &lastitem
    
    make_civilisation(1,specialplanet(46))
    if makelog=1 then print #f,"makeciv2 done" &lastitem
    
    add_questguys
    
    if findcompany(1)=0 then specialplanet(40)=32767
    if findcompany(2)=0 then specialplanet(41)=32767
    if findcompany(3)=0 then specialplanet(42)=32767
    if findcompany(4)=0 then specialplanet(43)=32767
    
    
    if makelog=1 then print #f,"delete company specials" &lastitem
    
    fleet(1).mem(1)=makeship(33)
    fleet(1).ty=1
    fleet(1).c=basis(0).c
    fleet(2).mem(1)=makeship(33)
    fleet(2).ty=1
    fleet(2).c=basis(1).c
    fleet(3).mem(1)=makeship(33)
    fleet(3).ty=1
    fleet(3).c=basis(2).c
    fleet(5).mem(1)=makeship(33)
    fleet(5).ty=1
    fleet(5).c=basis(4).c
    
    lastfleet=12
    for a=6 to lastfleet
        fleet(a)=makemerchantfleet
        fleet(a).t=rnd_range(firstwaypoint,lastwaypoint)
        fleet(a).c=targetlist(fleet(a).t)
        e=999
        for b=1 to 15
            if fleet(a).mem(b).movepoints(0)<e and fleet(a).mem(b).movepoints(0)>0 then e=fleet(a).mem(b).movepoints(0)
        next
        fleet(a).mem(0).engine=e
        if fleet(a).mem(0).engine<1 then fleet(a).mem(0).engine=1
        
    next
    
    if _clearmap=1 then
        for a=0 to laststar+wormhole+1
            if map(a).discovered>0 then 
                map(a).discovered=0
                for b=1 to 9
                    if map(a).planets(b)>0 then planets(map(a).planets(b)).visited=0
                next
'            else
'                print "system "&a &" at "& map(a).c.x &":"& map (a).c.y
'                map(a).discovered=1
'                sleep
            endif
        next
    endif
    if showclouds=1 then
        for x=0 to sm_x
            for y=0 to sm_y
                spacemap(x,y)=abs(spacemap(x,y))
            next
        next
        for a=0 to laststar+wormhole+1
            player.discovered(map(a).spec)+=1
            map(a).desig=spectralshrt(map(a).spec)&player.discovered(map(a).spec)&"-"&int(disnbase(map(a).c))&"("&map(a).c.x &":"& map(a).c.y &")"
        
        next
    endif
    
    if makelog=1 then 
        print #f,"Clear stuff" &lastitem
        close #f
    endif
    
    print
    set__color( 11,0)
    print "Universe created with "&laststar &" stars and "&lastplanet-lastdrifting &" planets."
    set__color( 15,0)
    print "Star distribution:"
    for a=1 to 10
        print spectralname(a);":";scount(a)
    next
    print "Asteroid belts:";astcou
    print "Gas giants:";gascou
    sleep 1250
    return 0
end function

function add_stars() as short
    dim as short a,cc,b,f,debug
    dim as string l
    dim as _stars del
    dim as _planet delpl
    
    'debug=2
    for a=0 to max_maps
        planets(a)=delpl
    next
    
    cc=0
    for a=0 to laststar
        map(a)=del
        for b=1 to 9
            map(a).planets(b)=0
        next
        print ".";
        map(a).c.x=rnd_range(0,sm_x)
        map(a).c.y=rnd_range(0,sm_y)
        if rnd_range(1,100)<36 then
            map(a).spec=rnd_range(2,4)
        else
            map(a).spec=rnd_range(1,7)
        endif
        if rnd_range(1,100)<91 then
            for b=1 to 9
                map(a).planets(b)=rnd_range(1,24)-((map(a).spec-3)^2+rnd_range(1,12))
                if map(a).planets(b)>0 then                    
                    if rnd_range(1,100)<77 then
                        cc+=1
                        map(a).planets(b)=cc
                    else
                        if rnd_range(1,100)<64 then
                            map(a).planets(b)=-rnd_range(1,6)
                        else
                            if rnd_range(1,100)<45+b*5 then
                                if b<7 then
                                    map(a).planets(b)=-20001
                                else
                                    map(a).planets(b)=-20002
                                endif
                                if b=1 then map(a).planets(b)=-20003
                            else
                                map(a).planets(b)=0
                            endif
                        endif
                    endif
                else
                    map(a).planets(b)=0
                endif
            next
        else
            if rnd_range(1,100)<50 then
                map(a).spec=8
                cc+=1
                map(a).planets(1)=cc
            else
                if debug=1 then dprint map(a).c.x &":"&map(a).c.y
                map(a).spec=10
                map(a).planets(1)=-20002
                if rnd_range(1,100)<25 then 
                    cc+=1
                    map(a).planets(2)=cc
                endif
                if rnd_range(1,100)<15 then 
                    cc+=1
                    map(a).planets(rnd_range(3,5))=cc
                endif
                
                if rnd_range(1,100)<5 then 
                    cc+=1
                    map(a).planets(rnd_range(6,9))=cc
                endif
            endif
        endif
    next
    for a=0 to cc
        planets(a)=delpl
    next
    for a=0 to laststar
        map(a).ti_no=89
    next
    lastplanet=cc
    return 0
end function

function add_wormholes() as short
    dim a as short
    for a=laststar+1 to laststar+wormhole-1 step 2
        map(a).c.x=rnd_range(0,sm_x)
        map(a).c.y=rnd_range(0,sm_y)
        map(a).spec=9
        map(a).ti_no=77
        map(a).planets(1)=a+1
        map(a).discovered=maximum(show_all,show_wormholes)
        map(a+1).planets(1)=a
        map(a+1).c.x=rnd_range(0,sm_x)
        map(a+1).c.y=rnd_range(0,sm_y)
        map(a+1).spec=9
        map(a+1).ti_no=77
        map(a+1).discovered=maximum(show_all,show_wormholes)
    next
    return 0
end function


function add_special_planets() as short
    dim as short a,sys,mp,d,gas,cgas,cc
    for a=0 to laststar
        if spacemap(map(a).c.x,map(a).c.y)<>0 and map(a).discovered=0 and map(a).spec<8 then cgas+=1
    next
    for a=0 to lastspecial
        do
            if a=2 or a=3 or a=4 or a=26 or a=27 then 'These specials are always generated in gasclouds
                cgas-=1
                gas=1
            else
                gas=0
            endif
            if cgas<=0 then 
                gas=0
                print "No more stars in gas clouds"
            endif
            sys=getrandomsystem(1,gas,1)
            if sys<0 then sys=getrandomsystem(1)
            if a=7 or a=46 and disnbase(map(sys).c)<25 then 
                cc=0
                do
                    cc+=1
                    sys=getrandomsystem(1,gas,1)
                    if sys<0 then sys=getrandomsystem(1)
                loop until disnbase(map(sys).c)>=25-cc/20 or cc>500
            endif
        loop until  map(sys).spec<8 
        mp=getrandomplanet(sys)
        if mp=-1 and a<>18 then 
            mp=rnd_range(1,9)
            map(sys).planets(mp)=lastplanet+1
            lastplanet=lastplanet+1
            mp=lastplanet
        endif
        map(sys).discovered=2
        if a=18 then
            map(sys).planets(3)=lastplanet+1
            specialplanet(18)=lastplanet+1
            map(sys).planets(9)=lastplanet+2
            specialplanet(19)=lastplanet+2
            lastplanet=lastplanet+2
            a=19
        endif
        
        if a<>18 then specialplanet(a)=mp
        
        print ".";
        if specialplanet(a)<0 then
            set__color( 12,0)
            print a;" ";sys;" ";mp
            print lastplanet
            set__color( 15,0)
        endif
    next
    
    specialplanet(30)=lastplanet+1
    lastplanet=lastplanet+1
    
'    a=sysfrommap(specialplanet(20))
'    print a
'    c=999
'    for b=0 to laststar
'        if b<>a then
'            if cint(distance(map(a).c,map(b).c))<c then
'                c=cint(distance(map(a).c,map(b).c))
'                d=b
'            endif
'        endif
'    next
'    
'    a=sysfrommap(specialplanet(26))
'    swap map(a).c,map(d).c
'    
    
    return 0
end function


function add_easy_planets(start as _cords) as short
    dim as short closest(5)
    dim as short a,b,c,f,r
    dim as single d
    dim as short list(laststar)
    for a=0 to laststar
        list(a)=a
    next
    
        
    for b=0 to 4
        d=999
        for a=0 to laststar
            if distance(start,map(a).c)<=d and map(a).spec<8 then
                f=0
                for c=0 to 4
                    if a=closest(c) then f=-1
                next
                if f=0 then
                    r=a
                    d=distance(start,map(a).c)
                endif
            endif
        next
        closest(b)=r
    next
    print
    print "Generating starting planets"
    for b=0 to 4
        for a=1 to 9
            map(closest(b)).discovered=6
            print ".";
            if map(closest(b)).planets(a)>0 then
                if is_special(map(b).planets(a))=0 then
                    makeplanetmap(map(closest(b)).planets(a),a,map(closest(b)).spec+5)
                    for c=0 to 16
                        if planets(map(closest(b)).planets(a)).mon_template(c).hpmax>3 then planets(map(closest(b)).planets(a)).mon_template(c).hpmax=3 
                        planets(map(closest(b)).planets(a)).mon_template(c).hp=planets(map(closest(b)).planets(a)).mon_template(c).hpmax
                        planets(map(closest(b)).planets(a)).mon_template(c).armor=0
                        planets(map(closest(b)).planets(a)).mon_template(c).weapon=0
                        planets(map(closest(b)).planets(a)).mon_template(c).range=1.5
                    next
                endif
            endif
        next
    next
    
    return 0
end function

function add_event_planets() as short
    dim as short sys,d,planet,debug,f
    
    for d=0 to 5
        sys=getrandomsystem()
        if sys>0 then
            if debug=1 then print "disc:";map(sys).discovered
            if map(sys).discovered=0 then
                planet=getrandomplanet(sys)
                
                if planet>0 then
                    if is_special(planet)=0 then
                        makeplanetmap(planet,rnd_range(1,9),map(sys).spec)
                        map(sys).discovered=3
                        planet_event(planet)
                    endif
                endif
            endif
        endif
    next
    return 0
end function

'    
'    debug=1
'    if debug=1 then 
'        f=freefile
'        open "eventplanet.txt" for output as #f
'    endif
'    for d=0 to 5
'        print ".";
'        a=getrandomsystem()
'        if a>0 then
'            if debug=1 then print a;
'            if map(a).discovered=0 then
'                if debug=1 then print ":";map(a).discovered
'                b=getrandomplanet(a)
'                if debug=1 then print #f,":"& b &"x:" & map(a).c.x &"y:" &map(a).c.y &"sys:"&a
'                if b>0 and b<=lastplanet then
'                    if is_special(b)=0 and b<>pirateplanet(0) and b<>pirateplanet(1) and b<>pirateplanet(2) then
'                        makeplanetmap(b,rnd_range(1,9),map(a).spec)
'                        planet_event(b)
'                        map(a).discovered=3
'                    endif
'                endif
'            else
'                print "EVENT PLANET GENERATED IN WRONG PLACE:"&map(a).discovered
'                sleep
'            endif
'        endif
'    next
'    if debug=1 then close #f


function add_drifters() as short
    dim as short a,b,c,d
    dim as _cords p1
    
    print
    print "Making drifters";
    for a=1 to lastdrifting
        print ".";
        lastplanet=lastplanet+1
        drifting(a).x=rnd_range(0,sm_x)
        drifting(a).y=rnd_range(0,sm_y)
        drifting(a).s=rnd_range(1,a)
        if drifting(a).s>16 then drifting(a).s=rnd_range(1,12)
        if all_drifters_are>0 then drifting(a).s=all_drifters_are
        drifting(a).m=lastplanet
        if a=1 then drifting(a).s=20
        if a=2 then drifting(a).s=20
        if a=3 then drifting(a).s=20
        if a=4 then drifting(a).s=17
        if a=5 then drifting(a).s=18 
        if a=6 then drifting(a).s=19 
        if drifting(a).s<=22 then make_drifter(drifting(a))
        drifting(a).p=show_all
    next
    print
    drifting(1).x=targetlist(firstwaypoint).x
    drifting(1).y=targetlist(firstwaypoint).y
    planets(drifting(1).m).atmos=5
    planets(drifting(1).m).depth=1
    planets_flavortext(drifting(1).m)="A cheerfull sign says 'Welcome! Please enjoy our services'"
    deletemonsters(drifting(1).m)
    planets(drifting(1).m).mon_template(0)=makemonster(88,drifting(1).m)
    planets(drifting(1).m).mon_noamin(0)=2
    planets(drifting(1).m).mon_noamax(0)=6
    planets(drifting(1).m).mon_template(2)=makemonster(99,drifting(1).m)
    planets(drifting(1).m).mon_noamin(2)=1
    planets(drifting(1).m).mon_noamax(2)=1
    planets(drifting(1).m).flags(29)=5
    planets(drifting(2).m).flags(29)=6
    planets(drifting(3).m).flags(29)=7
    
    planetmap(19,10,drifting(1).m)=-287
    if rnd_range(1,100)<66 then planetmap(39,13,drifting(1).m)=(298+rnd_range(1,4))*-1
    planetmap(46,18,drifting(1).m)=-222
    for a=1 to rnd_range(2,5) 'Some spacesuits in the starting station
        placeitem(makeitem(320),46,18,drifting(1).m)
    next
    do
        a=rnd_range(firststationw,lastwaypoint)
    loop until targetlist(a).x>=20 and targetlist(a).x<=25
    drifting(2).x=targetlist(a).x
    drifting(2).y=targetlist(a).y
    planets(drifting(2).m).atmos=5
    planets(drifting(2).m).depth=1
    deletemonsters(drifting(2).m)
    planets_flavortext(drifting(2).m)="A cheerfull sign says 'Welcome! Please enjoy our services'"
    planets(drifting(2).m).mon_template(0)=makemonster(88,drifting(2).m)
    planets(drifting(2).m).mon_noamin(0)=3
    planets(drifting(2).m).mon_noamax(0)=6
    planetmap(19,10,drifting(2).m)=-287
    if rnd_range(1,100)<33 then planetmap(39,13,drifting(2).m)=(298+rnd_range(1,4))*-1
    
    do
        a=rnd_range(firststationw,lastwaypoint)
    loop until targetlist(a).x>=45 and targetlist(a).x<=50
    drifting(3).x=targetlist(a).x
    drifting(3).y=targetlist(a).y
    planets(drifting(3).m).atmos=5
    planets(drifting(3).m).depth=1
    deletemonsters(drifting(3).m)
    planets_flavortext(drifting(3).m)="A cheerfull sign says 'Welcome! Please enjoy our services'"
    planets(drifting(3).m).mon_template(0)=makemonster(88,drifting(3).m)
    planets(drifting(3).m).mon_noamin(0)=3
    planets(drifting(3).m).mon_noamax(0)=6
    planetmap(19,10,drifting(3).m)=-287
    if rnd_range(1,100)<33 then planetmap(39,13,drifting(2).m)=(298+rnd_range(1,4))*-1
    
    drifting(4).x=map(sysfrommap(specialplanet(18))).c.x-5+rnd_range(1,10)
    drifting(4).y=map(sysfrommap(specialplanet(18))).c.y-5+rnd_range(1,10)
    if drifting(4).x<0 then drifting(lastdrifting).x=0
    if drifting(4).y<0 then drifting(lastdrifting).y=0
    if drifting(4).x>sm_x then drifting(lastdrifting).x=sm_x
    if drifting(4).y>sm_y then drifting(lastdrifting).y=sm_y
    planets_flavortext(drifting(4).m)="You enter the alien vessel. The air is breathable. Most of the ship seems to be a huge hall illuminated in blue light. Strange trees grow in orderly rows and stranger insect creatures scurry about." 
    planets(drifting(4).m).atmos=4
    planets(drifting(4).m).depth=1
    deletemonsters(drifting(4).m)
    planets(drifting(4).m).mon_template(0)=makemonster(37,drifting(4).m)
    planets(drifting(4).m).mon_noamin(0)=16
    planets(drifting(4).m).mon_noamax(0)=26
    
    planets_flavortext(drifting(5).m)="This ship has been drifting here for millenia. The air is gone. Propably some asteroid punched a hole into the hull. Dim green lights on the floor barely illuminate the corridors."
    planets(drifting(5).m).darkness=5
    planets(drifting(5).m).atmos=1
    planets(drifting(5).m).depth=1
    deletemonsters(drifting(5).m)
    planets(drifting(5).m).mon_template(0)=makemonster(9,drifting(5).m)
    planets(drifting(5).m).mon_noamin(0)=16
    planets(drifting(5).m).mon_noamax(0)=26
    planets(drifting(5).m).flags(6)=66
    planets(drifting(5).m).flags(7)=66
    planets(drifting(5).m).flags(4)=6
    
    planets_flavortext(drifting(6).m)="You dock at the ancient space probe."
    planets(drifting(6).m).darkness=5
    deletemonsters(drifting(6).m)
    planets(drifting(6).m).atmos=1
    planets(drifting(6).m).depth=1
    
    if map(sysfrommap(specialplanet(17))).c.x>=sm_x-4 then map(sysfrommap(specialplanet(17))).c.x-=4
    if map(sysfrommap(specialplanet(17))).c.y>=sm_y-4 then map(sysfrommap(specialplanet(17))).c.y-=4
    drifting(6).x=map(sysfrommap(specialplanet(17))).c.x+rnd_range(1,3)
    drifting(6).y=map(sysfrommap(specialplanet(17))).c.y+rnd_range(1,3)
    
    
    do
        for c=0 to 2    
            d=0
            for a=1 to lastdrifting
                for b=0 to 2
                    if drifting(a).x=basis(b).c.x and drifting(a).y=basis(b).c.y then
                        d=d+1
                        drifting(a).x=rnd_range(0,sm_x)
                        drifting(a).y=rnd_range(0,sm_y)
                    endif
                next
                for b=1 to lastdrifting
                    if a<> b then
                        if drifting(a).x=drifting(b).x and drifting(a).y=drifting(b).y then
                            d=d+1
                            drifting(a).x=rnd_range(0,sm_x)
                            drifting(a).y=rnd_range(0,sm_y)
                        endif
                    endif
                next
                
            next
        next
    loop until d=0
    
    for a=0 to 15
        p1=rnd_point(drifting(5).m,0)
        planetmap(p1.x,p1.y,drifting(5).m)=-81
    next
    for a=0 to 15
        p1=rnd_point(drifting(5).m,0)
        planetmap(p1.x,p1.y,drifting(5).m)=-158
    next
    
    return 0
end function

function add_caves() as short
    dim as short a,b,debug
    lastportal=22
    for a=0 to lastportal
         
            portal(a).desig="A natural tunnel. "
            portal(a).tile=111
            portal(a).col=7
            portal(a).ti_no=3001
            print ".";
            portal(a).from.m=get_nonspecialplanet(1)
            if portal(a).from.m<=0 then
                b=rnd_range(1,9)
                if portal(a).from.s=-1 then
                    do
                        portal(a).from.s=rnd_range(0,laststar)
                    loop until map(portal(a).from.s).discovered<>2
                endif
                if map(portal(a).from.s).planets(b)<=0 then
                    'makenewplanet
                    lastplanet=lastplanet+1
                    map(portal(a).from.s).planets(b)=lastplanet
                    'print portal(a).from.s &":" & map(portal(a).from.s).planets(b)
                else
                    portal(a).from.m=map(portal(a).from.s).planets(b)
                endif
            else
                portal(a).from.s=sysfrommap(portal(a).from.m)
                portal(a).dest.s=sysfrommap(portal(a).from.m)
            endif
            'portal(a).from.m=map(portal(a).from.s).planets(portal(a).from.p)
            lastplanet+=1
            portal(a).from.x=rnd_range(1,59)
            portal(a).from.y=rnd_range(1,19)
            portal(a).dest.m=lastplanet
            portal(a).dest.s=portal(a).from.s
            portal(a).dest.x=rnd_range(1,59)
            portal(a).dest.y=rnd_range(1,19)
            portal(a).discovered=show_portals
            portal(a).dimod=2-rnd_range(1,4)
            portal(a).tumod=4-rnd_range(1,8)
            portal(a).oneway=0
            map(sysfrommap(portal(a).from.m)).discovered=5
            
            print ".";
            if debug=1 then
                portal(a).from.x=30
                portal(a).from.y=10
                
            endif
    next
    if debug=1 then
        a=freefile
        open "portals.csv" for output as #a
        for b=0 to lastportal
            print #a,portal(b).from.x;";";portal(b).from.y;";";portal(b).from.m;";";portal(b).dest.x;";";portal(b).dest.y;";";portal(b).dest.m
        next
        close #a
    endif
    return 0
end function

function add_piratebase() as short
    dim as short a,b,c,d
    for a=0 to _NoPB
        lastplanet=lastplanet+1
        pirateplanet(a)=lastplanet
        piratebase(a)=getrandomsystem
        if piratebase(a)=-1 then piratebase(a)=rnd_range(0,laststar)
        map(piratebase(a)).discovered=show_pirates
        map(piratebase(a)).planets(rnd_range(1,9))=pirateplanet(a)
        print ".";
    next
    
    'print pirateplanet
    'sleep
    do
        c=c+1
        b=0
        for a=0 to _nopb
           if _minsafe=0 and disnbase(map(piratebase(a)).c)<4 then 
               d=getrandomsystem(0)
               if d>=0 then
                   swap map(piratebase(a)),map(d)
                   piratebase(a)=d
               endif
           endif
           if abs(spacemap(map(piratebase(a)).c.x,map(piratebase(a)).c.y))>1 then 
               d=getrandomsystem(0)
               if d>=0 then
                   swap map(piratebase(a)),map(d)
                   piratebase(a)=d
               endif
           endif
           b=b+disnbase(map(piratebase(a)).c)
        next
    loop until b>24 or c>255
    return 0
end function

function add_questguys() as short
    dim as short i,debug,f
    dim alreadyhere(17) as byte
    for i=0 to lastquestguy 'For the deletion questguy
        questguy(i).location=-2 'Everybody starts nowhere
    next
    debug=1
    f=freefile
    if debug=1 then open "questguys.log" for output as #f
    for i=1 to lastquestguy
        print #f,"Making qg "&i
        questguy(i)=questguy(0) 'Delete quest guy
        questguy(i).n=character_name(questguy(i).gender)
        if i=1 then 
            questguy(i).location=0
            questguy(i).job=1
        endif
        if i=2 then
            questguy(i).location=1
            questguy(i).job=1
        endif
        if i=3 then 
            questguy(i).location=2
            questguy(i).job=1
        endif
        if i=4 then questguy(i).job=14
        if i>4 then 
            do
                questguy(i).job=rnd_range(2,17)
            loop until alreadyhere(questguy(i).job)=0
            alreadyhere(questguy(i).job)=1
            questguy_newloc(i)
        endif
        questguy(i).risk=rnd_range(3,9)
        select case questguy(i).job
        case 1
            questguy(i).risk+=1
        case 2 to 9
            questguy(i).risk+=2
        end select
        if questguy(i).risk>9 then questguy(i).risk=9 
        if debug=1 then print #f,"Doing newquest"
        questguy_newquest(i)
        questguy(i).friendly=rnd_range(0,2) '0 hates you, 2 loves you
        questguy(i).money=rnd_range(1,10)*100
        select case questguy(i).job
        case 1
            questguy(i).money+=rnd_range(5,10)*100
        case 2,14,15,16,17
            questguy(i).money+=rnd_range(5,10)*10
        case 10,11,12,13
            questguy(i).money+=rnd_range(5,15)*100
        case 9
            questguy(i).money+=rnd_range(5,10)*10
        case else
            questguy(i).money-=rnd_range(1,10)*100
            if questguy(i).money<=0 then questguy(i).money=rnd_range(100,300)
        end select'        
    next
    if debug=1 then close #f
    return 0
end function

function distribute_stars() as short
    dim a as short
    print
    print "distributing ";
    for a=0 to laststar
        map(a).discovered=show_all
        pwa(a)=map(a).c
        print ".";
    next
    for a=laststar+1 to laststar+wormhole
        map(a).discovered=show_wormholes
        pwa(a)=map(a).c
    next
    a=distributepoints(pwa(),pwa(),laststar+wormhole)
    for a=0 to laststar+wormhole
        map(a).c=pwa(a)
        print ".";
    next
    return 0
end function

function gen_traderoutes() as short
    dim as _cords start,goal,wpl(40680)
    dim as _cords p
    dim t as short
    dim map(sm_x,sm_y) as short
    dim as integer x,y,i,offset,a,d,r
    dim as integer fp,lp
    dim as byte debug=0
    
    set__color( 7,0)
    print "generating traderoutes"
    
    lastwaypoint=5
    firstwaypoint=5
    for a=5 to 4068
        targetlist(a).x=0
        targetlist(a).y=0
    next
    for x=0 to sm_x
        for y=0 to sm_y
            p.x=x
            p.y=y
            if abs(spacemap(x,y))>=2 and abs(spacemap(x,y))<=5 then map(x,y)=abs(spacemap(x,y))*2
            if abs(spacemap(x,y))>5 then map(x,y)=1
            'if abs(spacemap(x-1,y))>=2 then map(x,y)=1
            'if abs(spacemap(x,y-1))>=2 then map(x,y)=1
            'if spacemap(x,y-1)<>0 and spacemap(x,y-1)<>1 and disnbase(p)>3 then map(x,y)=1
            'if spacemap(x,y-1)<>0 and spacemap(x,y-1)<>1 and disnbase(p)>3 then map(x,y)=1
            'if spacemap(x-1,y)<>0 and spacemap(x-1,y)<>1 and disnbase(p)>3 then map(x,y)=1
            'if spacemap(x-1,y)<>0 and spacemap(x-1,y)<>1 and disnbase(p)>3 then map(x,y)=1
            if map(x,y)>0 and show_NPCs=1 then
                locate y+1,x+1
                print "#"
            endif
        next
    next
    map(basis(0).c.x,basis(0).c.y)=0
    map(basis(1).c.x,basis(1).c.y)=0
    map(basis(2).c.x,basis(2).c.y)=0
    start.x=targetlist(0).x
    start.y=targetlist(0).y
    d=99999
    for a=0 to 2
        if distance(basis(a).c,start)<d then
            d=distance(basis(a).c,start)
            r=a
        endif
    next
    
    goal.x=basis(r).c.x
    goal.y=basis(r).c.y
    lp=A_Star(wpl(),goal,start,map(),sm_x,sm_y)
    offset=11
    if lp>0 then
        for i=0 to lp
            targetlist(i+offset).x=wpl(i).x
            targetlist(i+offset).y=wpl(i).y
            spacemap(wpl(i).x,wpl(i).y)=0
            wpl(i).x=0
            wpl(i).y=0
        next
    else
        
    endif
    
    lastwaypoint=lp+offset
    print offset;"-";lastwaypoint
    offset=lastwaypoint+1
    firststationw=lastwaypoint
    for a=1 to 2
        start.x=basis(a-1).c.x
        start.y=basis(a-1).c.y
        goal.x=basis(a).c.x
        goal.y=basis(a).c.y
        lp=A_Star(wpl(),goal,start,map(),sm_x,sm_y)
        for i=0 to lp
            targetlist(i+offset).x=wpl(i).x
            targetlist(i+offset).y=wpl(i).y

            wpl(i).x=0
            wpl(i).y=0
        next
        print offset;"-";lastwaypoint+lp
        lastwaypoint=lastwaypoint+lp+1
        offset=lastwaypoint
    next
    start.x=basis(2).c.x
    start.y=basis(2).c.y
    goal.x=basis(0).c.x
    goal.y=basis(0).c.y
    lp=A_star(wpl(),goal,start,map(),sm_x,sm_y)
    for i=0 to lp
        targetlist(i+offset).x=wpl(i).x
        targetlist(i+offset).y=wpl(i).y
    next
    offset=offset+lp+1
    print offset;"-";lastwaypoint+lp
    lastwaypoint=lastwaypoint+lp
    firstwaypoint=11
    if targetlist(firstwaypoint).x>1 and targetlist(firstwaypoint).y>1 then
        if spacemap(targetlist(firstwaypoint).x,0)=0 or spacemap(targetlist(firstwaypoint).x,0)=1 then
            targetlist(firstwaypoint).y=0
        else
            targetlist(firstwaypoint).x=0
        endif
    endif
    lastwaypoint-=1
    print firstwaypoint &"-"&lastwaypoint
    if debug=5 then
        for x=0 to sm_x
            for y=0 to sm_y
                if spacemap(x,y)<>0 and spacemap(x,y)<>1 then 
                    map(x,y)=2
                    if abs(spacemap(x,y))>=2 and abs(spacemap(x,y))<=5 then map(x,y)=1
                
                else
                    map(x,y)=0
                endif
                locate y+1,x+1
                if map(x,y)>=4 and map(x,y)<=10 then
                    set__color( 15,0)
                    print "#";
                endif
                if map(x,y)=2 then
                    set__color( 15,0)
                    print ":";
                endif
                
                if map(x,y)=0 then
                    set__color( 1,0)
                    print ".";
                endif
            next
        next
        set__color( 10,0)
        x=firstwaypoint
        do
            set__color( 10,0)
            x+=1
            if x>lastwaypoint then x=firststationw
            for y=0 to 2
                if basis(y).c.x=targetlist(x).x and basis(y).c.y=targetlist(x).y then t=y
            next
            locate targetlist(x).y+1,targetlist(x).x+1
            if t=0 then print "0";
            if t=1 then print "1";
            if t=2 then print "2";
            sleep 66
            set__color( 1,0)
            locate targetlist(x).y+1,targetlist(x).x+1
            print "."
        loop until inkey<>""
        sleep
        
    endif
    if debug=5 then
        for a=5 to lastwaypoint
            locate targetlist(a).y+1,targetlist(a).x+1
            print "*"
            sleep 100
        next
    endif
'    
'    targetlist(0)=basis(0).c
'    targetlist(1).x=rnd_range(0,30)
'    targetlist(1).y=rnd_range(15,20)
'    targetlist(2)=basis(1).c
'    
'    targetlist(3).x=rnd_range(0,60)
'    targetlist(3).y=rnd_range(1,20)
'    
'    targetlist(4).x=rnd_range(30,59)
'    targetlist(4).y=rnd_range(15,20)
'    targetlist(5)=basis(2).c
'    targetlist(6).x=rnd_range(0,60)
'    targetlist(6).y=rnd_range(0,20)
    return 0
end function


function make_clouds() as short
    dim wmap(sm_x,sm_y)as ubyte
    dim as short x,y,bx,by,highest,count,a,b,c,r
    dim as single attempt
    dim debug as short
    
    dim as _cords p1,p2,p3,p4
    print
    print "Creating clouds";
    do
        if debug=1 then print attempt
        print ".";
        for x=0 to sm_x
            for y=0 to sm_y
                wmap(x,y)=0
                spacemap(x,y)=0
            next
        next
        highest=0
        do
            count=0
            bx=rnd_range(3,7)
            p1.x=rnd_range(bx,sm_x-bx)
            p1.y=rnd_range(by,sm_y-by)
            r=rnd_range(1,100)
            if r<=20 then p1.y=sm_y
            if r<=15 then p1.x=sm_x
            if r<=10 then p1.y=0
            if r<=5 then p1.x=0
            for x=0 to sm_x
                for y=0 to sm_y
                    p2.x=x
                    p2.y=y
                    if distance(p1,p2)<bx then wmap(x,y)=wmap(x,y)+rnd_range(1,6)
                    if wmap(x,y)>highest then highest=wmap(x,y)
                    if wmap(x,y)>=8 then count+=1
                next
            next
        loop until count>=sm_x*sm_y*(0.20-attempt)
        for x=0 to sm_x
            for y=0 to sm_y
                if wmap(x,y)=8 or wmap(x,y)=9  then spacemap(x,y)=-2                    
                if wmap(x,y)=10 or wmap(x,y)=11  then spacemap(x,y)=-3                    
                if wmap(x,y)=12 or wmap(x,y)=13  then spacemap(x,y)=-4                    
                if wmap(x,y)>13 then spacemap(x,y)=-5                    
                if abs(spacemap(x,y))>2 then count+=1
            next
        next
    
    if rnd_range(1,100)<33 then
        p1.x=rnd_range(1,sm_x)
        p1.y=rnd_range(1,sm_y)
    else
        p1=map(rnd_range(laststar+1,laststar+wormhole)).c
    endif
    
    
    for a=0 to 65 'Space - Time Anomalies
        do
            if rnd_range(1,100)<33 then
                if rnd_range(1,100)<33 then
                    p1.x=rnd_range(1,sm_x)
                    p1.y=rnd_range(1,sm_y)
                else
                    p1=map(rnd_range(laststar+1,laststar+wormhole)).c
                endif
            endif
            if spacemap(p1.x,p1.y)=-6 or spacemap(p1.x,p1.y)=-7 or spacemap(p1.x,p1.y)=-8 then
                p1.x=rnd_range(1,sm_x)
                p1.y=rnd_range(1,sm_y)
            endif
        loop until not((p1.x=10 and p1.y=30) or (p1.x=60 and p1.y=10) or (p1.x=35 and p1.y=20))
        select case rnd_range(1,100)
        case is<20 
            c=-6
        case 20 to 40
            c=-7
        case 41 to 60
            c=-8
        case 61 to 90
            c=-9
        case else
            c=-10
        end select
        
        c=-6
        if rnd_range(1,100)<33 then c=-7
        if rnd_range(1,100)<33 then c=-8
        if rnd_range(1,100)<66 then c=-9
        
        for b=1 to 9
            if rnd_range(1,100)>66 then
                p2=p1
                do
                    if rnd_range(1,100)<66 then
                        p2=movepoint(p2,b,,1)
                    else
                        p2=movepoint(p2,5,,1)
                    endif
                    spacemap(p2.x,p2.y)=c                    
                loop until rnd_range(1,100)>10+a/2 or (p2.x=10 and p2.y=30) or (p2.x=60 and p2.y=10) or (p2.x=35 and p2.y=20)
            endif
        next
    next
        attempt=attempt+.01
        if attempt>0.1 then attempt=0.1
        flood_fill(sm_x/2,sm_y/2,spacemap(),1)
    loop until spacemap(10,sm_y-10)=255 and spacemap(sm_x-10,10)=255
    do
        x=rnd_range(0,sm_x)
        y=rnd_range(0,sm_y)
        if spacemap(x,1)=255 then
            targetlist(0).x=x
            targetlist(0).y=0
        endif
        
        if spacemap(x,sm_y)=255 then
            targetlist(0).x=x
            targetlist(0).y=sm_y
        endif
        
        if spacemap(0,y)=255 then
            targetlist(0).x=0
            targetlist(0).y=y
        endif
        
        if spacemap(sm_x,y)=255 then
            targetlist(0).x=sm_x
            targetlist(0).y=y
        endif
    loop until targetlist(0).x=0 xor targetlist(0).y=0
    
    if spacemap(map(piratebase(0)).c.x,map(piratebase(0)).c.y)<>11 then
        p1=map(piratebase(0)).c
        do
            spacemap(p1.x,p1.y)=255
            p1=movepoint(p1,5)
            print ".";
        loop until spacemap(p1.x,p1.y)=255
        
    endif
    
    for x=0 to sm_x
        for y=0 to sm_y
            if spacemap(x,y)=255 then spacemap(x,y)=0
        next
    next
    
    if show_all=1 then
        for x=0 to sm_x
            for y=0 to sm_y
                if spacemap(x,y)=0  then spacemap(x,y)=1                    
                if abs(spacemap(x,y))>1  then spacemap(x,y)=abs(spacemap(x,y))
            next
        next    
    endif
    'print highest;lowest
    'sleep
    return 0
end function

function make_civilisation(slot as short,m as short) as short
    dim  as _cords p,p1,p2,p3,p4,p5
    dim as short x,y,a,b,row
'    n as string
'    ship as _ship
'    spec as _monster
'    aggr as byte
'    inte as byte
'    tech as byte
'    phil as byte
    
    specialflag(7)=0
    specialflag(46)=0
    civ(slot).n=alienname(3)
    civ(slot).popu=rnd_range(1,4)+rnd_range(1,4)+rnd_range(1,4)
    civ(slot).aggr=rnd_range(1,3) '1=submissive 2=neutral 3=agressive
    civ(slot).inte=rnd_range(1,3) '1=Science 2=Trade 3=conquest
    civ(slot).tech=rnd_range(1,3) 
    civ(slot).phil=rnd_range(1,3)'1=individualists 2=organized 3=collectivists
    if rnd_range(1,100)<50 then
        civ(slot).prefweap=0
    else
        civ(slot).prefweap=5
    endif
    civ(slot).spec=makemonster(1,m,1)
    civ(slot).spec.sdesc=civ(slot).n
    civ(slot).spec.ldesc="A " &civ(slot).n &". " &civ(slot).spec.ldesc
    civ(slot).spec.hpmax=civ(slot).spec.hpmax+rnd_range(1,civ(slot).tech)+rnd_range(1,civ(slot).aggr)
    civ(slot).spec.hp=civ(slot).spec.hpmax
    civ(slot).spec.armor=rnd_range(1,civ(slot).tech)
    civ(slot).spec.lang=32+slot
    civ(slot).spec.col=rnd_range(8,15)
    civ(slot).spec.aggr=1
    civ(slot).spec.faction=1+slot
    civ(slot).spec.allied=6+slot
    civ(slot).spec.hasoxy=1
    civ(slot).spec.cmmod=10-civ(slot).aggr
    civ(slot).spec.items(1)=201+slot*2
    civ(slot).spec.itemch(1)=35+civ(slot).aggr*10
    civ(slot).spec.items(2)=202+slot*2
    civ(slot).spec.itemch(2)=35+civ(slot).aggr*10
    civ(slot).spec.items(3)=-21
    civ(slot).spec.itemch(3)=88
    civ(slot).spec.items(3)=96
    civ(slot).spec.itemch(3)=25
    civ(slot).spec.items(4)=96
    civ(slot).spec.itemch(4)=77
    civ(slot).spec.items(5)=96
    civ(slot).spec.itemch(5)=66
    civ(slot).spec.swhat=" fires an exotic weapon"
    civ(slot).spec.scol=rnd_range(1,6)
    civ(slot).home=map(sysfrommap(m)).c
    for a=0 to 7
        faction(slot+6).war(a)=civ(slot).aggr*10 'Other Civ
    next
    makeplanetmap(m,3,3)
    deletemonsters(m)
    planets(m).mon_template(0)=civ(slot).spec
    planets(m).mon_noamin(0)=10
    planets(m).mon_noamax(0)=20
    make_aliencolony(slot,m,civ(slot).popu)
    planetmap(rnd_range(1,60),rnd_range(1,20),m)=276+slot
    planetmap(rnd_range(1,60),rnd_range(1,20),m)=278+slot

    civ(slot).item(0).desig=civ(slot).n &" gun"
    civ(slot).item(0).desigp=civ(slot).n &" guns"
    civ(slot).item(0).ty=2
    civ(slot).item(0).id=201+slot*2
    civ(slot).item(0).v1=(rnd_range(0,3)+rnd_range(0,3)+rnd_range(0,3)+rnd_range(0,civ(slot).aggr)+rnd_range(0,civ(slot).tech)-2)/10'damage
    civ(slot).item(0).v2=(rnd_range(1,3)+rnd_range(1,3)+civ(slot).aggr+civ(slot).tech-2)'range
    civ(slot).item(0).v3=(rnd_range(0,3)+rnd_range(0,civ(slot).aggr)+rnd_range(0,civ(slot).tech)-5)'tohit
    if civ(slot).item(0).v1<=0 then civ(slot).item(0).v1=.1
    if civ(slot).item(0).v2<=0 then civ(slot).item(0).v2=1
    if civ(slot).item(0).v3<=0 then civ(slot).item(0).v3=1
    civ(slot).item(0).price=(civ(slot).item(0).v1*10+civ(slot).item(0).v2+civ(slot).item(0).v3)*75
    civ(slot).item(0).col=civ(slot).spec.col
    civ(slot).item(0).icon="-"
    civ(slot).item(0).ldesc="A gun used by the "&civ(slot).n &"  | | Accuracy: "&civ(slot).item(0).v3 &" | Damage: "&civ(slot).item(0).v1 &" | Range:"&civ(slot).item(0).v2
    
    civ(slot).item(1).desig=civ(slot).n &" blade"
    civ(slot).item(1).desigp=civ(slot).n &" blades"
    civ(slot).item(1).ty=4
    civ(slot).item(1).id=202+slot*2
    civ(slot).item(1).v1=(rnd_range(0,3)+rnd_range(0,3)+rnd_range(0,3)+civ(slot).aggr+civ(slot).tech-2)/10'damage
    civ(slot).item(1).v3=(rnd_range(0,3)+civ(slot).aggr+civ(slot).tech-5)'tohit
    if civ(slot).item(1).v1<=0 then civ(slot).item(1).v1=.1
    if civ(slot).item(1).v3<=0 then civ(slot).item(1).v3=1
    civ(slot).item(1).price=(civ(slot).item(1).v1*10+civ(slot).item(1).v3)*50
    civ(slot).item(1).col=civ(slot).spec.col
    civ(slot).item(1).icon="("
    civ(slot).item(1).ldesc="A close combat weapon used by the "&civ(slot).n &"  | | Accuracy: "&civ(slot).item(1).v3 &" | Damage: "&civ(slot).item(1).v1 
    
    make_alienship(slot,0)
    make_alienship(slot,1)
    
    for a=0 to 6
        if rnd_range(1,100)<66 then
            civ(slot).culture(a)=rnd_range(1,5+civ(slot).aggr+civ(slot).phil)
            if civ(slot).culture(a)>6 then civ(slot).culture(a)=6
        endif
    next
        
    tiles(272+slot).no=272+slot
    tiles(272+slot).tile=64 
    tiles(272+slot).col=civ(slot).spec.col
    tiles(272+slot).bgcol=0
    tiles(272+slot).desc="A "&civ(slot).n &" spaceship."
    tiles(272+slot).seetru=1
    tiles(272+slot).hides=2
    
    if rnd_range(1,100)<55 then
        tiles(274+slot).tile=asc("#") 
        tiles(274+slot).col=17
    else
        tiles(274+slot).tile=asc("O") 
        tiles(274+slot).col=137
    endif
    tiles(274+slot).bgcol=0
    tiles(274+slot).desc="A "&civ(slot).n &" building."
    tiles(274+slot).seetru=1
    tiles(274+slot).hides=2
    
    tiles(276+slot).tile=tiles(274+slot).tile
    tiles(276+slot).col=tiles(274+slot).col+2
    tiles(276+slot).seetru=1
    tiles(276+slot).gives=301+slot
    tiles(276+slot).hides=2
    tiles(276+slot).turnsinto=276+slot
    
    tiles(278+slot).tile=tiles(274+slot).tile
    tiles(278+slot).col=tiles(274+slot).col+3
    tiles(278+slot).seetru=1
    tiles(278+slot).gives=311+slot
    tiles(278+slot).hides=2
    tiles(278+slot).turnsinto=278+slot
    
    tiles(280+slot).tile=tiles(274+slot).tile
    tiles(280+slot).col=tiles(274+slot).col+3
    tiles(280+slot).seetru=1
    tiles(280+slot).gives=321+slot
    tiles(280+slot).hides=2
    tiles(280+slot).turnsinto=280+slot
    
    tiles(282)=tiles(280)
    tiles(282).gives=330
    tiles(282).turnsinto=282
    
    
    tiles(283+slot).tile=64 
    tiles(283+slot).col=civ(slot).spec.col
    tiles(283+slot).bgcol=0
    tiles(283+slot).desc="An alien scoutship"
    tiles(283+slot).seetru=1
    tiles(283+slot).walktru=0
    tiles(283+slot).firetru=1
    tiles(283+slot).shootable=1
    tiles(283+slot).locked=3
    tiles(283+slot).spawnson=100
    tiles(283+slot).spawnswhat=81+slot
    tiles(283+slot).spawnsmax=1
    tiles(283+slot).spawnblock=1
    tiles(283+slot).turnsinto=62
    tiles(283+slot).dr=2
    tiles(283+slot).hp=25
    tiles(283+slot).succt="It is slightly dented now"
    tiles(283+slot).failt="Your handwapons arent powerful enough to damage a spaceship"
    tiles(283+slot).killt="That will teach them a lesson!"
    tiles(283+slot).hides=2
    
    if slot=0 then
        if civ(slot).phil=1 then specialplanettext(7,0)="A planet with many small, modern structures dotting the landscape"
        if civ(slot).phil=2 then specialplanettext(7,0)="A planet with several medium to large cities distributed on the surface"
        if civ(slot).phil=3 then specialplanettext(7,0)="A planet with a huge, dominating megacity"
        specialplanettext(7,1)="The homeworld of the "&civ(slot).n
        if civ(slot).culture(4)=5 then 
            specialplanettext(7,0)=specialplanettext(7,0) &". In its orbit you discover a spacestation, connected to the ground by a gargantuan cable. A space lift!"
            specialplanettext(7,1)=specialplanettext(7,1) &". In its orbit you discover a spacestation, connected to the ground by a gargantuan cable. A space lift!"
        endif
    endif
    if slot=1 then
        if civ(slot).phil=1 then specialplanettext(46,0)="A planet with many small, modern structures dotting the landscape"
        if civ(slot).phil=2 then specialplanettext(46,0)="A planet with several medium to large cities distributed on the surface"
        if civ(slot).phil=3 then specialplanettext(46,0)="A planet with a huge, dominating megacity"
        specialplanettext(46,1)="The homeworld of the "&civ(slot).n
        if civ(slot).culture(4)=5 then 
            specialplanettext(46,0)=specialplanettext(46,0) &". In its orbit you discover a spacestation, connected to the ground by a gargantuan cable. A space lift!" 
            specialplanettext(46,1)=specialplanettext(46,1) &". In its orbit you discover a spacestation, connected to the ground by a gargantuan cable. A space lift!" 
        endif
    endif
    if civ(slot).culture(3)=5 then 'Tamed robots
        planets(m).mon_template(2)=makemonster(8,m)
        planets(m).mon_template(2).faction=1+slot
        planets(m).mon_template(2).cmmod=6
        planets(m).mon_noamin(2)=3
        planets(m).mon_noamax(2)=8
    endif
    if civ(slot).culture(4)=2 then
        lastplanet+=1
        p1=rnd_point
        p1.m=lastplanet
        makecavemap(p1,rnd_range(1,4),rnd_range(1,4),0,0)
        p2=rnd_point
        p2.m=m
        addportal(p1,p2,0,asc("o"),"A natural tunnel",7)
        planets(lastplanet)=planets(m)
        planets(lastplanet).life=planets(lastplanet).life+5
        planets(lastplanet).depth=1
        planets(lastplanet).weat=0
        for b=0 to planets(a).life
            planets(lastplanet).mon_noamin(b)=0
            planets(lastplanet).mon_noamax(b)=0
            if rnd_range(1,100)<100-b*10 then 
                planets(lastplanet).mon_template(b)=makemonster(1,lastplanet)
                planets(lastplanet).mon_noamin(b)=rnd_range(1,planets(lastplanet).life)*planets(lastplanet).mon_template(b).diet
                planets(lastplanet).mon_noamax(b)=rnd_range(1,planets(lastplanet).life)*2*planets(lastplanet).mon_template(b).diet
                if planets(lastplanet).mon_noamin(b)>planets(lastplanet).mon_noamax(b) then swap planets(lastplanet).mon_noamin(b),planets(lastplanet).mon_noamax(b)
            endif
        next
        p1=rnd_point
        placeitem(makeitem(205,slot),p1.x,p1.y,lastplanet)
        placeitem(makeitem(201+slot*2),p1.x,p1.y,lastplanet)
        placeitem(makeitem(202+slot*2),p1.x,p1.y,lastplanet)
    endif
    if civ(slot).culture(4)=4 then 'slave population
        planets(m).mon_template(2)=makemonster(1,m,1)
        planets(m).mon_template(2).faction=1+slot
        planets(m).mon_template(2).lang=34
        planets(m).mon_template(2).cmmod=6
        planets(m).mon_noamin(2)=5
        planets(m).mon_noamax(2)=10
    endif
    if civ(slot).culture(4)=5 then 'Space lift
        lastplanet+=1
        p1.x=30
        p1.y=5
        p2.x=30
        p2.y=15
        p3.x=5
        p3.y=10
        p4.x=55
        p4.y=10
        for x=0 to 60
            for y=0 to 20
                p.x=x
                p.y=y
                planetmap(x,y,lastplanet)=-200
                if distance(p,p1)<=9 then planetmap(x,y,lastplanet)=-68
                if distance(p,p2)<=9 then planetmap(x,y,lastplanet)=-68
                if distance(p,p3)<=9 then planetmap(x,y,lastplanet)=-68
                if distance(p,p4)<=9 then planetmap(x,y,lastplanet)=-68
                if distance(p,p1)<=7 then planetmap(x,y,lastplanet)=-201
                if distance(p,p2)<=7 then planetmap(x,y,lastplanet)=-201
                if distance(p,p3)<=7 then planetmap(x,y,lastplanet)=-201
                if distance(p,p4)<=7 then planetmap(x,y,lastplanet)=-201
                if distance(p,p1)<5 then planetmap(x,y,lastplanet)=-202
                if distance(p,p2)<5 then planetmap(x,y,lastplanet)=-202
                if distance(p,p3)<5 then planetmap(x,y,lastplanet)=-202
                if distance(p,p4)<5 then planetmap(x,y,lastplanet)=-202
            next
        next
        for x=0 to 60
            for y=0 to 20
                if y=9 and x>5 and x<55 then planetmap(x,y,lastplanet)=-202
                if y=10 and x>5 and x<55 then planetmap(x,y,lastplanet)=-202
                if y=11 and x>5 and x<55 then planetmap(x,y,lastplanet)=-202
                if x>5 and x<55 and (y=8 or y=12) then
                    if planetmap(x,y,lastplanet)=-200 or planetmap(x,y,lastplanet)=-68 then 
                        if y=8 then planetmap(x,y-1,lastplanet)=-68
                        if y=12 then planetmap(x,y+1,lastplanet)=-68
                        planetmap(x,y,lastplanet)=-201
                    endif
                endif
            next
        next
        planets(lastplanet)=planets(m)
        planets(lastplanet).grav=planets(lastplanet).grav/5
        planets(lastplanet).depth=1
        planets(lastplanet).weat=0
        p.x=30
        p.y=10
        p.m=lastplanet
        p2.x=30
        p2.y=10
        p2.m=m
        addportal(p,p2,0,asc("o"),"A space lift",14)
        for a=0 to 5
            p=rnd_point(lastplanet,0)
            planetmap(p.x,p.y,lastplanet)=-267
        next
        p=rnd_point(lastplanet,0)
        planetmap(p.x,p.y,lastplanet)=-282
        p=rnd_point(lastplanet,0)
        planetmap(p.x,p.y,lastplanet)=-266
        p=rnd_point(lastplanet,0)
        planetmap(p.x,p.y,lastplanet)=-280-slot 'Shop
        p=rnd_point(lastplanet,0)
        planetmap(p.x,p.y,lastplanet)=-280-slot 'Shop
        planetmap(12,8,lastplanet)=-203
        planetmap(48,12,lastplanet)=-203
    endif 
    
    'shop21+slot= alienshop
    shopitem(1,22+slot)=civ(slot).item(0)
    shopitem(2,22+slot)=civ(slot).item(1)
    b=2
    for a=0 to civ(slot).tech*2+civ(slot).inte
       b+=1
       shopitem(b,22+slot)=(rnd_item(5))
    next
    return 0
end function

function make_aliencolony(slot as short,map as short, popu as short) as short
    dim as short a,x,y,xw,yw,sh
    dim as _cords p,p2,ps(20)
    if civ(slot).phil=1 then
        for a=0 to popu*10
            p=rnd_point
            planetmap(p.x,p.y,map)=-274-slot
        next
        for a=0 to popu*3
            p=rnd_point
            planetmap(p.x,p.y,map)=-68
            if rnd_range(1,100)<civ(slot).tech*10 then planetmap(p.x,p.y,map)=-272-slot
        next
    endif
    if civ(slot).phil=2 then
        for a=0 to popu*10
            if a<=20 then ps(a)=rnd_point
        next
        for a=0 to popu*10-1 step 2 
            if a<=19 then makeroad(ps(a),ps(a+1),map)
        next
        for a=0 to popu*10
            if a<=20 then 
                p=ps(a)
            else
                p=rnd_point
            endif
            if p.x<1 then p.x=1
            if p.x>59 then p.x=59
            if p.y<1 then p.y=1
            if p.y>19 then p.y=19
            planetmap(p.x,p.y,map)=-274-slot
            planetmap(p.x,p.y-1,map)=-274-slot
            planetmap(p.x,p.y+1,map)=-274-slot
            planetmap(p.x+1,p.y,map)=-274-slot
            planetmap(p.x-1,p.y,map)=-274-slot
        next
        for a=1 to rnd_range(1,3)
            p=rnd_point
            xw=rnd_range(2,4)
            yw=rnd_range(2,4)
            for x=p.x to p.x+xw
                for y=p.y to p.y+yw
                    if x>=0 and x<=60 and y>=0 and y<=20 then
                        planetmap(x,y,map)=-68
                        if rnd_range(1,100)<civ(slot).tech*10 then planetmap(x,y,map)=-272-slot
                    endif
                next
            next
        next
    endif
    if civ(slot).phil=3 then
        p=rnd_point
        for x=p.x-10 to p.x+10
            for y=p.y-10 to p.y+10
                if x>=0 and x<=60 and y>=0 and y<=20 then
                    p2.x=x
                    p2.y=y
                    if distance(p2,p)<popu+2 then planetmap(x,y,map)=-68
                    if rnd_range(1,100)<civ(slot).tech*10 then planetmap(p.x,p.y,map)=-272-slot
                    if distance(p2,p)<popu then planetmap(x,y,map)=-274-slot
                endif
            next
        next
        planetmap(p.x,p.y,map)=-68
    endif
    if civ(slot).inte=2 then
        sh=3
    else
        sh=1
    endif
    for a=0 to sh
        if rnd_range(1,100)<(popu+sh)*5 then
            p=rnd_point(map,0)
            planetmap(p.x,p.y,map)=-280-slot
        endif
    next
    return 0
end function


function make_alienship(slot as short,t as short) as short
    dim as short c,wc,cc,roll
    dim chance(3) as short '3 chance for weapon 2 chance for cargo 1 chance for sensors
    if civ(slot).inte=1 then
        chance(1)=20
    else
        chance(1)=10
    endif
    
    if civ(slot).inte=2 then
        chance(2)=20
    else
        chance(2)=10
    endif
    
    if civ(slot).inte=3 then
        chance(3)=30
    else
        chance(3)=10
    endif
    
    
    civ(slot).ship(t).hull=5+rnd_range(1,civ(slot).tech)*5*(t+1)-rnd_range(1,civ(slot).phil)*3*(t+1)
    if civ(slot).ship(t).hull<1 then civ(slot).ship(t).hull=1
    civ(slot).ship(t).shieldmax=rnd_range(1,civ(slot).tech)-2
    if civ(slot).ship(t).shieldmax<0 then civ(slot).ship(t).shieldmax=0
    civ(slot).ship(t).shield=civ(slot).ship(t).shieldmax
    civ(slot).ship(t).sensors=rnd_range(1,civ(slot).tech)
    civ(slot).ship(t).col=civ(slot).spec.col
    civ(slot).ship(t).engine=1
    civ(slot).ship(t).pipilot=3
    civ(slot).ship(t).pigunner=3
    civ(slot).ship(t).ti_no=25+slot
    wc=2
    cc=1
    civ(slot).ship(t).weapons(1)=makeweapon(rnd_range(1,civ(slot).tech)+civ(slot).prefweap)
    for c=0 to civ(slot).ship(t).hull step 5
        if rnd_range(1,100)<75 then
            civ(slot).ship(t).engine+=1
        else
            civ(slot).ship(t).engine-=1
        endif
        if rnd_range(1,100)<50 then
            civ(slot).ship(t).pipilot+=1
        else
            civ(slot).ship(t).pipilot-=1
        endif
        if rnd_range(1,100)<50 then
            civ(slot).ship(t).pigunner+=1
        else
            civ(slot).ship(t).pigunner-=1
        endif
            
        roll=rnd_range(1,100)
        select case roll
        case 0 to chance(1)
            civ(slot).ship(t).weapons(wc)=makeweapon(rnd_range(1,civ(slot).tech)+civ(slot).prefweap)
            wc+=1
        case chance(1)+1 to chance(1)+chance(2)
            civ(slot).ship(t).cargo(cc).x=1
            cc+=1
        case chance(1)+chance(2)+1 to chance(1)+chance(2)+chance(3)
            civ(slot).ship(t).sensors+=1
        case else
            if civ(slot).inte=1 then
                civ(slot).ship(t).sensors+=1
            endif
            if civ(slot).inte=2 then
                civ(slot).ship(t).cargo(cc).x=1
                cc+=1
            endif
            if civ(slot).inte=3 then
                civ(slot).ship(t).weapons(wc)=makeweapon(rnd_range(1,civ(slot).tech)+civ(slot).prefweap)
                wc+=1
            endif
        end select
        
    
    next
    civ(slot).ship(t).turnrate=1
    if civ(slot).ship(t).engine>3 then civ(slot).ship(t).turnrate+=1
    civ(slot).ship(t).c.x=rnd_range(1,60)
    civ(slot).ship(t).c.y=rnd_range(1,20)
    if civ(slot).ship(t).engine<1 then civ(slot).ship(t).engine=1
    if civ(slot).ship(t).pigunner<1 then civ(slot).ship(t).pigunner=1
    if civ(slot).ship(t).pipilot<1 then civ(slot).ship(t).pipilot=1
    civ(slot).ship(t).h_maxweaponslot=wc-1
    civ(slot).ship(t).h_maxhull=civ(slot).ship(t).hull
    if t=0 then civ(slot).ship(t).icon=left(civ(slot).n,1)
    if t=1 then civ(slot).ship(t).icon=lcase(left(civ(slot).n,1))
    if civ(slot).inte=1 then
        if t=0 then civ(slot).ship(t).desig=civ(slot).n &" explorer"
        if t=1 then civ(slot).ship(t).desig=civ(slot).n &" scout"
    endif
    if civ(slot).inte=2 then 
        if t=0 then civ(slot).ship(t).desig=civ(slot).n &" merchantman"
        if t=1 then civ(slot).ship(t).desig=civ(slot).n &" transporter"
    endif
    if civ(slot).inte=3 then 
        if t=0 then civ(slot).ship(t).desig=civ(slot).n &" warship"
        if t=1 then civ(slot).ship(t).desig=civ(slot).n &" fighter"
    endif
    civ(0).ship(0).ti_no=35
    civ(0).ship(1).ti_no=35
    civ(1).ship(0).ti_no=36
    civ(1).ship(1).ti_no=36
    return 0
end function
