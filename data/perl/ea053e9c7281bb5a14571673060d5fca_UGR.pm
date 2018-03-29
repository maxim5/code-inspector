package YAPC::Europe::UGR;

use warnings;
use strict;

use feature qw(say switch);
use experimental qw(smartmatch);

use version;
our $VERSION = qv('0.0.4');

use Exporter qw(import);
our @EXPORT_OK = qw(pick_best_venue);

sub pick_best_venue {
    my $year = shift // 2015;
    given ($year) {
        when ('2015') { # force string comparison
            return "Granada";
        }
    }
    default {
        # TODO: update this as new proposals come out
        return 'Cluj-Napoca';
    }
}

1;

__END__

=encoding utf8

=head1 NAME

YAPC::Europe::UGR - University of Granada proposal for YAPC::EU 2015

=head1 SYNOPSIS

    use YAPC::Europe::UGR qw(pick_best_venue);

    say "And the winner is... ", pick_best_venue(2015);


=head1 DESCRIPTION

The L<OSL|http://osl.ugr.es> (Oficina de Software Libre, Free Software
Office in Spanish) at the University of Granada with the support of
and Barcelona.pm presents this bid for YAPC::EU 2015.

=head2 Organizers

We are the The OSL at the University of Granada, led by L<JJ
Merelo|http://search.cpan.org/~jmerelo/>. Under the name of Granada.pm
we have a great involvement in the Spanish Perl Community.

We have organized the virtual course L<Perl|http://cevug.ugr.es/perl>
for more than a dozen editions and lately also the course
L<Perl Avanzado (Advanced Perl)|http://cevug.ugr.es/perl_avanzado>.

We organized the first Perl workshop in Spain L<the Granada Perl
Workshop|http://workshop.granada.pm>, which took place in Granada on
the 27th of June, 2014.

The OSL has also the support of the two strongest Perl Monger groups
in Spain, Madrid.pm and Barcelona.pm, and of several other Spanish
Perl hackers.

The key local persons supporting the proposal are as follows:

=over 4

=item * JJ Merelo

He has already organized other conferences in the past such as PPSN
2002 and ECAL 1995, in the evolutionary algorithm area and has also
collaborated in the organization of other conferences such as CEC 2011
or L<EvoStar 2014|http://evostar.org> (~150 persons), and many events
such as NotBarraLibreCamp. He collaborated also with the organization
of CIG 2012 (~100 persons), JENUI 2009 (local
conference on Informatics Teaching, ~100 persons).

He has attended several Perl events in the past and made
presentations, including several YAPC::Europe (2002, 2010) or FOSDEM
Perl devrooms (2013, 2014).

=item * Antonio Mora

He was the local organizer for the L<Computational
Inteligence in Games conference|http://geneura.ugr.es/cig2012/> and is
currently a postdoc at the University of Granada.

=item * Pedro Castillo

He is a long-time Perl hacker and instructor.

He is the head of the GeNeura research group who is also collaborating
in the organization of this conference, and has been involved in the
organization of several conferences, including CIG and CEDI (big
Spanish annual CS event).

=back

Other local organizations and groups of people supporting the event
are as follows:

=over 4

=item * OSL volunteers

Volunteers and other people attached to the
L<OSL|http://osl.ugr.es/about/quienes-somos/>.

=item * IT Delegation of the University of Granada

Additional help from the
L<IT delegation of the University of Granada|http://detic.ugr.es>
and L<Computer Science School|http://etsiit.ugr.es>.

=back

Other people not from Granada collaborating with the organization of
the event are Alex Muntada, Salvador FandiE<ntilde>o, Diego
Kuperman and other members of the Perl Mongers groups.

=head3 Contact

For the time being, the contact email for any matter related to this
proposal is L<email:dirosl@ugl.es> (OSL direction address).

In case we win the bid, we will setup a new address specifically for
supporting the event.

=head2 Venue

We will hold it at the most convenient place at the L<University of
Granada|http://www.ugr.es>, with campus all over the city of Granada
(Spain). Granada includes such beauties as the Alhambra, the Albayzin
neighborhood, natural park of Sierra Nevada and one of the most
beautiful grafittis in the country. Moreover, the beach is just 70
kilometers (35 minutes by highway) away from the city.

We have pre-acceptance for holding it at the
L<ETSIIT|http://etsiit.ugr.es> (Computer Science School), which is
in the outskirts of the city but has all the facilities needed for
a multi-room conference and is, anyways, well communicated with
the rest of the city. Close by there is a provincial government
building that could be used for keynotes. We will buy enough extension
cords for conference users; being a university building, it is well
prepared for lots of people using electricity. All university
buildings fulfill EC rules regarding accessibility,  including access
via wheelchairs or magnetic loops systems for people with hearing
difficulties. No wired connectivity is previewed, in principle, but if
needed a meeting room with access to the university network will be
arranged.

The IT services at the University of Granada provides an easy way to
set up WiFi guest access (limited to HTTP/HTTPS). EduRoam is also
available for those coming from an academic environment and it works
without a glitch. The network is able to support up to 5k devices
concurrently.

Regardless of the university building we choose, we will set up a
couple of additional rooms for organizational purposes, storage,
short meetings, BOFs and anything else that is needed.

We will use volunteers with good English skills (taken from our
student pool) to staff a help desk during the whole conference. We
have already done that successfully at previous events.

=head3 Audiovisual and other facts about the facilities

All rooms have projectors and a computer, plus the usual audiovisual
equipment. However, the Free Software Office has not been able to
conquer all of them for free software, so the free Software Office
will lend a few laptops just in case speakers want to use them. These
projectors usually have VGA plugs, so speakers using Macs and others
with HDMI or USB connection will have to provide their own converters
(we can have a few handy, though).

The keynote rooms are auditorium-style, usually, with seats
ascending. Other plenary rooms we could use (which, as we said, will
depend on total attendance) have the same arrangement. Usual
classrooms for parallel sessions are usually flat.

The end of August is outside the classes period, but we will have to
go to the very end since the university is shuttered for the rest of
the month. That implies also that there will be few, if any,
restrictions in the use of classes and other spaces, since there will
be no students.

=head3 Alternatives

In the very rare case that we would exceed 600 people attending the
conference, Granada can offer many places: Parque de las Ciencias and
(a science museum) and a Conference Center. However, the L<University
of Granada has many campuses (three in the city of Granada) and
buildings|http://www.ugr.es/pages/centros>, most of which can
accommodate any amount of people, including the Aula Magna at the
Facultad de Letras (literally, Letters Faculty, which includes
Philology and other Humanities-related degrees) which can sit 560
people. If the conference blown up to epic proportions, we could use
the conference center, which would also blow up the budget, since they
obviously charge for room occupation.  However, if there is such a big
amount of attendees budget might not be a problem.

=head3 Facilities at the UGR

Our preferred venue is the
L<ETSIIT|http://etsiit.ugr.es/pages/instalaciones_servicios/salas_aulas>,
whose biggest room can fit 196 people, with room for a few more in the
sidelines or standing up. If attendance is not more than 250 people,
we would go for this venue, since there is also a FSO storage room,
the offices of most organizers, and it is the obvious choice, being
the Computer Science School and all. There are air-conditioned
classrooms with 100 capacity, and other rooms with 78 seats, 8 of each
+ 3 more classrooms, also air conditioned with a capacity of 60. The
cafeteria can sit up to 120 people (but we would do the tapas crawl
for lunch, something we successfully did during EvoStar). This place
is conveniently linked by public transport to the city center, and not
far away from a tram stop.

Second choice for over 250 people would be L<the Polytechnic
building|http://etsiccp.ugr.es/>, closer to downtown, but home to
people that get their hands dirty building bridges and houses (not any
more in Spain) and harbours. The
L<conference room|http://etsiccp.ugr.es/pages/reserva_aulas_medios/capacidades_aulas_y_medios>
can hold 0b100000000 persons which would actually be a nicer capacity for the
CS school, but it is not, so up to 300 including standing room.

Third choice would be the Aula Magna at the L<Sciences
Faculty|http://fciencias.ugr.es> which has been recently renovated and
includes can hold up to 506 seats. This campus is right next to the
second one and also practically in the city center. It has many
classrooms with capacity from 60 to 150. These two places actually
have a tram stop nearby. That the tram will be actually passing by it
next year is a riddle. It might, having been under construction for 9
years and all, but then it might not. 

As you see, the main factor for choosing venue is the number of
simultaneous people it can hold at the same time. Since this will
depend on the attendance of the current YAPC as well as how many
people we are going to attract, we cannot commit ourselves to one of
them right now and we keep our options open. At any rate, any "big
room" mentioned before is inside the university building with the rest
of the rooms used close by, in the same building a few meters away. 

=head3 Refreshments

In previous experiences (EvoStar 2014) we have organized a Tapas
Experience for lunch and our intention is to organize it again this
time.

We will provide every attendee with several "tapa" vouchers that they
can exchange in the pubs and bars around the venue (5 minutes away,
top) at their discretion in order to have lunch, dinners or well, at
any time they feel hungry.

"Tapas" are a small dish usually served accompanied by a drink. Every
bar has its own specialties and style and as a whole they provide
great variety. Granada's tapas are quite remarkable. They form part of
the city essence and are at the center of its social life.

This is also a good way to increase networking, since you usually have
a "tapa" in some bar, then go to another one where you can meet a
different set of people, etc. The experience in EvoStar 2014 was very
positive in terms of quality and satisfaction.

Besides, we would ensure that we have enough bars to cope with the
attendance and that they all have offerings covering special needs,
specifically vegan dishes.

=head3 Getting here

L<Granada is linked to Madrid, London and
Barcelona|http://www.skyscanner.es/vuelos-a/grx/companias-aereas-que-vuelan-a-granada-aeropuerto.html>
by regular daily and frequent flights and also to Mallorca and to
other places (but flight frequencies vary often and are sometimes
seasonal). L<ME<aacute>laga is roughly one hour away by car or 2 hours
by bus and is linked to all major European
cities|http://en.wikipedia.org/wiki/M%C3%A1laga_Airport#Airlines_and_destinations>
(and many minor, as long as they have enough sun-and-party-hungry
punters). There are also buses and trains to Madrid and Seville, but
coach is always the best option outside the plane.

The ETSIIT is linked to the city center (with many lodging options) by
several bus lines. Depending on the date, student residences might
also be available (July is the best date for that, since usual guests
will be on holiday). It is possible, but not likely, that Granada
light rail system will be working by August 2015. There is a station
close to the ETSIIT that would link it in minutes to the railway
station and other points of interest in the city, including hotel
areas.


=head3 Catering

The school has a relatively large cafeteria and a university canteen,
which can accommodate a good amount of hungry students. There are 250
sitting places, but buffer-style eating with sitting space outside the
cafeteria, including common areas and classes, can also be
arranged. The school cafeteria can provide the food itself for a good
price (from 17E<euro> a sitting, served menu to less if it's standing around
tables).

Cheap beer-and tapas can be had around the school for 2E<euro> beer + tapa
of choice, four  beers (or non-alcoholic drinks) are enough for any hungry
monger.

=head2 Conference Details

It's going to be, AFAIK, the YAPC::EU southernmost conference, so this
fact will have to be taken into account in the details of the
conference... not. 

=head3 Dates

Due to budgetary reasons, the university is locked during August;
closest we can make it is late July or early September. Late July has
the advantage of empty classrooms with no problem to schedule the
conference, while early September would be a little more problematic
(but no big deal for a 2 or 3 day conference), milder weather (but you
can still go to the beach) and good choice of accommodations (lower
touristic season, if there's such a thing in Granada).

=head3 Theme

We propose I<The art of Perl> as a theme for this conference. Perl
poetry has been all but forgotten, and, besides, the Fine Arts school
is close to the Computer Science school, so we might even attract some
of those free spirits to our conference.

=head2 Website

We will host it at the OSL servers, using Act! or other Perl toolkit
for creating websites. No PHP. I promise.

=head2 Amusements

We'll consider having for early birds a perl golf contest or a Perl
quiz; for those staying late we could also consider that. It is also
an option to consider during cocktail parties the first day. Any
suggestion will also be welcome.

=head2 Promotion

The OSL maintains a presence in social networks (identi.ca, Twitter,
Facebook), and the people in the organization do have that too.
We would use email, local free software events (the Libre
Software World Conference is held yearly in Fall somewhere in
Spain), FOSDEM devrooms, and a sandwich man walking around
inconspicuously around PHP and Python developer conferences.

=head2 Survey

We are developing an app for creating a personal schedule, and as in
past conferences, we'll use whatever people have scheduled
for creating a preference. Which will be probably for those
dressed as Star Trek fleet ensigns or anything that is not
simultaneous to talks by Damian, brian or Mark.

=head2 Additional Program

Granada offers a great amount of options for people from 2 to
22. Sorry, to 222. We'll organize a tapas crawl in the
best watering holes of Granada, artistic trips through the
grafitti art in Granada streets. And, yes, also Alhambra
and all those things. We'll also organize courses for those
interested and beginning courses in Spanish.

=head3 Courses and tutorials

No innovation here. We'll provide space during or preferably before
the conference so that people that want to give tutorial or
courses can pay trip expenses giving them. The organization
will only collect a racket, sorry, a cut for, you know,
protection.

For a boost of visibility (or outing) of the Spanish Perl community,
we'll also provide courses in several levels in Spanish. Any
other languages can also be arranged.

=head3 Side Trips

Anything can be arranged; we will contact a travel agency so that they
can offer packages for a good price. But the usual thing is:

=over 4

=item *

Alhambra and Generalife.

=item *

Sunset in front of the Alhambra, through the world heritage quarter
called Generalife.

=item *

Renaissance in Granada: cathedral and other churches and palaces.

=item *

Tapas crawl including fried fish, meat and everything you can include
in a little dish.

=back

=head2 Budget

Now we're talking business. We will stick to the same registration costs as the
last conference. Venue is free, since it's organized as an
institutional (meaning university, as belonging to the
University of Granada) event. The University of Granada covers insurance costs too.
This will leave us some
leeway to give a better attendees dinner.

We are talking of a ballpark of 30K E<euro>. We will also apply to local
science funding agencies and the university to defray part of the
cost. The Free Software Office will absorb any deficit (or, for
that matter, surplus) if there is one, although this will depend on
the source of the surplus. Since funding agencies pay the grant after
expenses have been incurred, in some cases years later, temporary
deficit will have to be absorbed by the free software office and any
surplus that is obtained after the conference also will go to the Free
Software Office operating costs and a  local L<Free Software
Prize|http://concursosoftwarelibre.org> to fund a special Perl-based
application prize. Any surplus obtained from
registration fees and sponsors will be returned to the YAPC::Europe
Foundation. The full budget is published in L<a Google Drive
document|http://goo.gl/xXJ1Y>. 

=head3 Income

Main income will be levied on attendees. Planned attendance fees are
in the same ballpark as previous events:

=over 4

=item Guests, Speakers, Organizers : 0E<euro>

=item Full-time students: 50E<euro>

=item Early-bird: 80E<euro>

=item Regular price: 110E<euro>

=item Corporate tariff: 900E<euro>

=back

Final fees have to be announced.  We will request any amount from
sponsors, and past events have gathered around 6000E<euro> exclusively from local
sponsors. A minimum of 1000E<euro> can be expected from those sources.

=head3 Costs

There are several costs per attendee:

=over 4

=item T-shirt: 4.5E<euro>

=item Catering: 35E<euro>

=item Bag: 4E<euro>

=item badge, and other stuff: 2E<euro>

=item Attendees dinner: 30E<euro>

=back

This is around 75E<euro> per attendee, fully covered by early-bird fee
and with a deficit for speakers and students.  This will be balanced
with the surplus provided by late arrivals, corporate fees and
sponsors.

The budget will be adjusted mainly by changing the number of lunches and coffee breaks.

In this scenario the only fixed cost is covering the costs of the 75
speakers which is 5625E<euro>. Realistically, that cost can be
covered only by sponsorship and company fees.

If we add the cost of one or two invited speakers at around 
2000E<euro> per speaker, there is a deficit of 9625E<euro>. 
If sponsorship is not reached (or corporate-fee paying, which it's
not clear how many are there) lunches would be eliminated, 
allowing us to basically have every paying person cover the cost of a
single speaker; in that case we would need as few as 75 attendees to
cover the 75 speakers.

The L<spreadsheet|http://goo.gl/xXJ1Y> provides a second sheet with a
worst case scenario in which official sponsorship goes down to 0 and
there are no corporate fees. A reduction of 15E<euro> in the catering
(which can be changed in the last minute) provides a result of around
250E<euro> of deficit which can be absorbed by the Free Software
Office.

 
=head2 Sponsors

Granada is being pushed as a technological city by consortiums such as
L<On Granada Tech City|http://www.ongranada.com/>
which is supported by major technological companies and
local institutions.

We have contact with local tech companies will will be willing to help
with small amounts; we will have no minimum requirement for
sponsorship. Companies such as L<Codeko.com|http://codeko.com>
or L<Blulabs|http://blulabs.es> have supported OSL events in the
past. We will mainly look for direct support of tchotchkes such
as t-shirts or bags. Other companies contacted after the initial
proposal such as L<Yaco|http://yaco.es>, L<ElasticSearch|http://elasticsearch.com'> and
L<Zentyal|http://zentyal.com> have answered positively to our support
requests. These offers have been included in the current version of
the budget. 

Support will also be requested from institutions of all kinds. Being
the economy of Spain in the shape that it is, we don't
expect much from that, but we will do it anyways and have
obtained support in the past.

=head2 About Granada

Granada is a student city which has been the L<preferred destination of
Erasmus students
|http://elpais.com/elpais/2012/11/28/inenglish/1354114165_335994.html>
for a long time, and that accounts for something. 
It's a lively city with many services for visitors.

=head3 Getting here

Granada has an international airport, but easiest way to reach it is
to connect at Madrid or Barcelona. From July 2013, there is a L<five
times a week direct British Airways flight to London
City|http://www.britishairways.com/travel/fx/public/en_gb?to=Granada&fromPkg=LCY>,
which can be also used as a hub to reach us, although it is not the
cheapest or even the fastest way to get here (maybe cheaper if used as
a connection).

Some price for return tickets to Granada; these are for next September 2013.

=over 4

=item London: 246E<euro>

=item Paris: 227E<euro>

=item Rome: 271E<euro>

=item Frankfurt: 364E<euro>

=item Moscow: 505E<euro>

=item Vienna: 326E<euro>

=back

Most flights are in the 200-500E<euro> range. 

There are many more options to Malaga, which is a big airport, 
including low-cost flights, but then you have to take a bus or 
train to the bus station and another bus (two hours) from there. 

The local bus company provides also a direct bus to Granada 
from Madrid airport, with two frequencies a day and a low price. 
Check out the L<ALSA|http://alsa.es> site for timetable.

Some sample prices and itineraries:

=over 4

=item Malaga Airport - Granada: 1.30h, 10E<euro>, 4 buses a day.

=item Malaga Bus Station-Granada, every hour on the hour 
(roughly, some exceptions) until
21:30. There are buses and trains from the airport to the bus station.

=item Madrid (Estacion Sur) - Granada : 5 horas (ALSA), 
several buses a day, roughly every hour; normal 5h and 17.53 
E<euro>, supra economy 4.5h and 26.81E<euro> and supra+ (with WiFi)
4.5h and 35-87E<euro> (only two of these 13:30, 19:30).

=item Seville-Granada and back, 41E<euro>, 3h15m. 

=back

Train trips:

=over 4

=item Madrid - Granada:  4.5h, 62E<euro>

=item Barcelona - Granada: 12h, 56E<euro>, really a long trip 
in wagon-lit, but an inexpensive option. Barcelona has rail links to
major European cities. 

=item Seville - Granada: 3h, 22E<euro>

=back 

There are buses also from Granada airport to the city center, 
costing 3E<euro>; taxis are roughly 10 times more expensive. 
We can organize taxi pools if needed.



=head3 Sightseeing

Granada includes the Alhambra and Albayzin, an ensemble that has been  declared
world heritage site by the UNESCO. That is only part of its patrimony,
that includes also Renaissance palaces, Gothic, Renaissance and
Baroque churches, and a rather unknown but no less beautiful set of modernist buildings.

Organized or self-organized options are available all year round. 
The beach is 70 kms and a bus run away. There are also many
opportunities for trekking up in Sierra Nevada or in the Alpujarras.

=head2 Summary

The bid from the Free Software Office at the University of Granada 
is organized by a group of persons with experience in organizing
events, some experience attending YAPC::Europe events (including,
possibly, this next Kiev YAPC::Europe), will take place in an incredibly
nice city, easily accessible by plane, in a venue (the University of
Granada) with all needed facilities and with support from local
university government, local free software SMEs and enthusiastic Perl
Mongers which, so far, have not seen a single YAPC in Spain.

=head2 Questions and Answers

While we haven't been asked these questions by the organization, 
they were made to other proposers, so here are the questions that 
have not been answered before.

=over

=item B<What's the price of beer?>

In the bars around the ETSIIT and in town, average price this year
(2014) is a bit over 2E<euro> and that includes the tapa, that is, a small dish
with usually warm food. That's the price of a tubo (1/3
liter). It's not usual in Spain to have bigger portions; you just
order a second one.

=item B<What's the weather like in July, August, September?>

It's definitely hot, with maximum that can go up to 40 degrees; it
goes down in September, but daily maximum are always over 30E<deg>.
September is milder, anyways.

=item B<Can people get receipts?>

Whether we choose a professional services company to organize
registration or the university itself, there is no problem with
providing receipts. We will see what is the more convenient option in
terms of work needed, but also financially; the University can provide
VAT-free registration while the external company can not.

=item B<How easy is it for people to navigate the city without speaking
Spanish?>

If you have a good map and can orient yourself, it is pretty easy. In
pure geographical terms, Granada is not a difficult place; on the
other hand, the Spanish educational system has made sure that very
few, if any, speak other than the mother tongue. However, they will
speak loudly and kindly to you until they make themselves understood.

=item B<It would be nice to have more details on accommodation, with a
range of the prices that can be expected for different levels of
accommodation. Can most attendees fit in one hotel? Is Internet access
widely available in accommodations?>

I<This is taken almost verbatim from L<CIG 2012 site|http://geneura.ugr.es/cig2012/acommodation.html>,
which Antonio Mora organized too.>

Granada is a city accustomed to a large touristic inflow, so its
offers a huge number of accommodation options for all budgets. In
addition, due to the number of students living in the city (more than
60000 during the year), there are a big amount of visitors in these
ages, so there are several economical lodgings.  So the city provides
dozens of hotels ranging from 5-star to 1-star ones. It must be noted
that hotels in Spain (and maybe more in Granada) are usually well
priced due to the competition among them. Thus, a 4-star hotel may
often be in the 75-100E<euro> range and a 3-star hotel in the 50-75
E<euro> range.  Of course, some fluctuations can happen depending on
the particular hotel and the zone where it is, but special packages
are also possible, allowing more economic prices.

=over

=item Recommended hotels (approximate prices)

=over

=item AC Palacio de Santa Paula - 5* (130E<euro> by night)

The best hotel in the city. Located at the main street (Gran
VE<iacute>a) in the city centre. Well communicated to reach the
ETSIIT.

=item Abba - 4* (75E<euro> by night)

New hotel (less than 3 years old), close to the train station and near
the city centre. Well placed to get to the ETSIIT (Avda
ConstituciE<oacute>n). Usually has good offers for University events,
including free WiFi and access to the spa, which we would arrange.

=item Vincci - 4* (80E<euro> by night)

Well-considered hotel in the city, close to the train station and near
the city centre. Well placed to get to the ETSIIT (Avda
ConstituciE<oacute>n).

=item Granada Center - 4* (65E<euro> by night)

Good hotel, not very expensive and close to a good tapas area (Severo
Ochoa street, in front of the Faculty of Sciences).

=item Carmen - 4* (55E<euro> by night)

Cheap hotel, but with good quality. It is near to the city centre.

=item Macia Gran Via - 3* (50E<euro> by night)

In the main street and quite cheap.

=item Puerta de las Granadas - 3* (70E<euro> by night)

Just below the Alhambra. Smack in the middle of the  tourist area. Only 14 rooms.

=item Juan Miguel - 3* (45E<euro> by night)

Cheap hotel in the city centre, close to the city hall.

=back

=item Student accommodation

As you can check, the prices are quite cheap even in four star hotels,
but there are a L<huge amount of guest houses (Pensiones in Spanish)
in the
city|http://geneura.ugr.es/cig2012/brochures/guest_houses_granada.pdf>. Or
if you prefer, there is also a Youth hostel (Albergue in
Spanish). During July, student dorms might offer also cheap
accommodation; in September it is less likely. There are also two
university residences, which are very nice, but not so conveniently
located for accessing the ETSIIT (or other university venue we might
choose. However, they might be used for invited speakers, mainly if we
manage to pay them from university budget.

=back

=item B<Are there any plans to stream or record talks? If so, how will the recordings be made and
how will authorization be sought?>

The assets are there, and it would be possible to record at least one
of the tracks. That would be free for the conference, since the OSL is
part of the IT dept of the university which includes the virtual
department too. The ETSIIT includes also self-recording facilities in
some classes, which we could use for some tracks.

=item B<Are any social events planned, other than the partner's program?>

We plan to do a pre-conference drink-up and post-conference
excursions. If sponsorship allows, we will organize a speakers' dinner
the first conference day.


=item B<Do you have any plans for an associated hackathon?>

In the OSL we organize hackathons to the tune of several every
year. We would love to organize one and try and attract local talent
to Perl. Our experience says that it's better to organize them with at
least one day and a half, which we would prefer to do before the
conference. The venue could change, since we have contact with local
coworking spaces that would provide the site and the connectivity, as
well as in some cases free drinks and coffee. They can even be used
overnight if needed.

=item B<Do you plan to provide anything to speakers? (Such as water, a person to time things and
keep the schedule on track, etc)>

We will have volunteers (students, GeNeura or OSL people) in every
room to fix any problem that can arise, from lack of electrical
outlets to swooning fans. Water will be provided for speakers, and
they will be heartily patted in the back after they finish. The
volunteer will also take care of time overruns by dancing a Spanish
jitterbug when the speaker has spent the allotted slot.

=item B<How many tracks are planned?>

Difficult to know in advance. Will depend on the number of
papers. ETSIIT being a CS school, we can provide for up to 20
tracks. Our YAPC::Eu experience dictates that there should be a part
of the day with single track (we can call them keynotes) to prevent
people giving a presentation at the same time than Damian or MJD to be
delivering it to the bleachers.

=item B<How many days do you expect the event to run, and what days of the week are you
considering?>

Wednesday until Friday, with Monday and Tuesday reserved for
hackaton. Weekend for social events.

=item B<What plans do you have concerning special diets for the coffee breaks (and lunch, if
sponsored)?>

The ETSIIT can sponsor a single coffee break. Coffee breaks, however,
are impersonal and there are excellent cafeterias around the ETSIIT
which can give inexpensive and local fare such as garlic olive-oil
toast, which no catering can provide and better quality coffee, as
well as fresh orange juice.

We will look for sponsors in any case, but will focus more on lunch
and merchandising instead of coffee breaks.

Any special needs will be catered for during lunch.

=item B<You mentioned organizing various "side trips", but it's not clear if
these are a Friend&Family program during the conference or if they are
targeted at the attendees and will therefore happen at a different
time/day than the ones of the conference.  Could you clarify this?>

Both can be done, of course. We will offer some of them I<during> the
conference for friends and family, and one after the conference for
everybody (attendees + friends & family)

=item B<While it sounds nice that the Free Software Office would cover a
loss, it is doubtful that the YAPC would generate a loss.  Will you
donate anything back to YEF / TPF / YAPC::Europe::2015 / sponsor other
Perl events if YAPC::Europe in Granada makes a profit?>

As indicated in the (changed) budget section, that will depend on the
source of surplus. If we have a surplus at the end of the conference
arising mainly from the fees and sponsorship, it will be donated back
to YEF/TPF/YAPC::Europe::2015. However, official sponsorship from
government/university/regional sources usually happen (and sometimes
is even known about) I<after>, sometimes much after, the
conference. If we I<know> about these sources I<before> the
conference, we will apply it to the conference and absorb that loss or
apply it to (additional) operating expenses by the FSO, to be recouped
when (and sometimes I<if>) it is eventually paid back. Depending on
the amount, and as said above, we will create a prize for Perl
software development, which is underrepresented in the local
University Free Software contest. 

Besides, some grants can only be requested after the conference in
order to finance the deficit of the event, with concession and
eventual payment a long time after the conference. In those cases, as
told above, the deficit will be paid initially by the budget of the
FSO and if there is a small surplus after the grant, it will remain in
the FSO to cover administrative costs.

In a nutshell: surplus during or immediately after the conference:
back to the YEF. Surplus months or years after the conference: FSO and
funding of local (Spanish level) Perl initiatives. 

=item B<Would you consider cheaper corporate tickets, say, of 400-500 Euro
instead of 900?>

Absolutely no problem. Any amount above the regular fee would be OK.

=item B<Did you already approach to any potential sponsors?>

We have approached the sponsors included in the new budget. Some of
them have already expressed their will to support with an amount, some
of them just their will. 

=item B<What is included in 35 euro catering costs for the attendee?>

Two meals, one coffee break. We will try to obtain the rest of the
meals directly from sponsors such as the Computer Science School (if
it is eventually held there) or corporate sponsors.

=item B<How many meals a day are you going to provide?>

One meal, one coffee break, but as shown in the budget section, meals
and coffees will be reserved when budget is secured for them.

=item B<Will catering be organized inside the venue or outside?>

Inside. All faculties in the UGR include a cafeteria and other common
areas for students.

=item B<What is the deadline for early bird registrations?>

The usual four months before the conference.

=item B<How many attendees do you expect?>

Our experience hosting other I<serial> conferences in Granada is that
the appeal of the city usually commands a small 10-20% increase over
other in the series. Since you mention in another question around 300
attendees, we have budgeted for approximately that increase in the
number of attendees, so around 330-350.

=item B<Can you roughly estimate the portion of speakers (you mention 75 of
them), early bird registrations, students, regular and business
attendees?>

The new budget estimates that amount, with 75 speakers, 65 students, 2
corporate sponsors, 150 early bird (about half) and 40 regular. It is
difficult to put a definite amount there, but we think these number,
according to our past experience, are realistic. Even accounting for a
very bad worst case scenario (L<third sheet in the
spreadsheet|http://goo.gl/xXJ1Y>) we would adjust budget mainly using
lunches and coffee breaks.

=item B<In case you have no money for lunches, will it be possible for
everybody to find food on their own in a reasonable time (90 minutes for
outside lunches seems to be the optimum)? You mentioned a few cafes
outside but are they capable to serve 300 people at the same time?>

Glad you ask that question, because whatever the place in the
University of Granada we celebrate YAPC, there are literally dozens of
places where you can have a quick snack, sandwiches or even a sit-down
proper lunch for old geezers. Even at the
L<ETSIIT|http://etsiit.ugr.es> there is a cafeteria and university
lunchroom, with the Fine Arts School cafeteria nearby and a
supermarket where you can buy salads or sandwiches and several bars
one block away, each one from 40 to 60 persons. 

=item B<How many local attendees might appear at the conference?>

The usual Spanish crowd at the YAPCs amounts to around one dozen
people. There are two strong Perl Monger groups in Spain whose attendance
would be boosted. To help attendance by local students we would apply
for course credits to the university, so that they can get ECTS credits for
attending and/or volunteering at the conference. All in all, 50 is a
reasonable number.

=item B<How many talk tracks are you going to have?>

As many as needed. The venue can accommodate any amount of them, and
its presence will be limited basically by budget (for full-length
talks, since they do not pay registration). We can fit as many
lightning talks sessions (by paying attendees) as needed.

=item B<Is there a room in the venue that can hold all the attendees at the
same time?>

See above, of course that's provided for. Up to 500 attendees there
are several places in the University of Granada that can fit
them. Beyond that, we would have to hire the palace of Congresses.

=item B<Do you have enough team members to organize a countdown
service in each of the talk rooms?>

Yes. We have enough persons providing services to the FSO and we'll
accept volunteers for credit, as has been done in other conferences in
the UGR.

=item B<Where are you going to find volunteers for this and how will you
motivate them?>

University students, motivated by what's more dear to them: free food
(in plausible scenario) and course credit.

We'll also pay (bus) trip and (student) lodging, as well as
registration, for those coming from outside Granada. 

=item B<Are there any other big events known to happen in Granada in late
July of early September 2015 which may cause problems with finding cheap
(or any) hotels?>

Neither the province fairgrounds nor the conference center plan so
much in advance, so it is difficult to know. However, there are no
recurring events either at the end of July or beginning of September,
and in any case, the L<Spanish convention bureau lists around 7000
rooms in hotels or 4 or 5
stars|http://www.scb.es/DESTINOS/GRANADA/tabid/78/language/es-ES/Default.aspx>,
with an even bigger number available below that. Most hotels offer the
university the possibility of pre-booking rooms at fixed price,
although we understand that most people will want to use the
Perl-driven Booking site for reservations.

=item B<What kind of a hackathon are you planning for the days preceding the
conference?>

We'll make a call for proposals so that CPAN authors can submit their
modules for enhancements or bug quashing. We'll contact authors of
major Perl projects such as perl5i or parrot in case they are
interested. This will be held either in the same place or, depending
on the number of people, in smaller venues such as the L<Free Software
Office|http://osl.ugr.es>, which is in a building with rooms varying
in capability from 12 to 40 persons.

=item B<Are there microphones for questions from the audience in the
talk rooms (at least in the big room)?> =back

Only in the big rooms, in fact.

=item B<Is it possible to quickly replace projectors in case they
happen to turn out of order in the middle of the talk?> =back

We'll have a pool of projectors handy in case any of them breaks
down. The FSO runs a hardware refurbishing project and we usually have
projectors for donation, so we'll keep two or three of them. In case
we have it at the ETSIIT, every department has its own projector which
we can use in case of emergency.

=item B<Is Wi-Fi connection open for SSH ports and such?> 

C<eduroam> is available throughout the university for all people whose
institution is a participant. IT services provide L<free conferences
WiFi|http://csirc.ugr.es/informatica/RedUGR/CVI/wifi-congresos.html>
with all ports open. We can arrange also physical connections, at
least for speakers.

=item B<In case it is not possible to use auditorium-style rooms
for each of the talk tracks, will the lack of air conditioning cause
any problems for the attendees?>

That's indeed a good question. In principle, those rooms would be used
just in case it's absolutely necessary (say, we have 5 or 6 parallel
tracks). The university has portable air conditioning that can be
requested in that possibility; we have used it for the free software
children's campus.

=item B<Your proposal is publically hosted on github, including the budget
calculations.  Do we have your permission to link to it from
http://www.yapceurope.org/ ?  If not, then maybe there is a sanitized
version of the proposal without the financials somewhere?>

I have requested permission from the probable sponsors to include
their names and quantities. Other that that, I don't see a problem in
revealing the budget. We were thinking actually about releasing the
whole setup as a CPAN module so that anybody else can use the
framework for their own proposals. The setup includes tests and
continuous integration using Travis.

If you think that financials are sensitive anyways we can just
make the spreadsheet unavailable for the general public.

=back

=cut


