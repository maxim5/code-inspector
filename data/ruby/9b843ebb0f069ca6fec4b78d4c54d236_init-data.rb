DATA = [
  { '@class' => 'User', :key => 'veny', 'username' => 'veny', 'password' => 'SHA:40bd001563085fc35165329ea1ff5c5ecbdbbeef', 'fullname' => 'Vaclav Novy', 'root' => true },
  { '@class' => 'User', :key => 'max', 'username' => 'max', 'password' => 'SHA:40bd001563085fc35165329ea1ff5c5ecbdbbeef', 'fullname' => 'Max Mustermann' },

  { '@class' => 'Unit', :key => 'foo', 'name' => 'Foo', 'description' => 'Desc of Foo', 'type' => 'PATIENT' },
  { '@class' => 'Unit', :key => 'bar', 'name' => 'Bar', 'description' => 'Desc of Bar', 'type' => 'CUSTOMER' },

  { '@class' => 'Membership', 'user' => 'veny', 'unit' => 'foo', 'role' => 'ADMIN', 'significance' => 10 },
  { '@class' => 'Membership', 'user' => 'veny', 'unit' => 'bar', 'role' => 'MEMBER', 'significance' => 20 },
  { '@class' => 'Membership', 'user' => 'max', 'unit' => 'foo', 'role' => 'MEMBER', 'significance' => 20 },
  { '@class' => 'Membership', 'user' => 'max', 'unit' => 'bar', 'role' => 'ADMIN', 'significance' => 40 },

  { '@class' => 'Patient', :key => 'JanNovak', 'unit' => 'foo', 'firstname' => 'Jan', 'surname' => 'Novák', 'asciiFullname' => 'JAN NOVAK', 'birthNumber' => '7001012000', 'phoneNumber' => '606123123' },
  { '@class' => 'Patient', 'unit' => 'foo', 'firstname' => 'Petr', 'surname' => 'Žluťoučký', 'asciiFullname' => 'PETR ZLUTOUCKY', 'birthNumber' => '7002023000', 'phoneNumber' => '606123123' },
  { '@class' => 'Patient', 'unit' => 'foo', 'firstname' => 'Lída', 'surname' => 'Modrá', 'asciiFullname' => 'LIDA MODRA', 'birthNumber' => '7051011000', 'phoneNumber' => '606123123' },
  { '@class' => 'Patient', :key => 'me', 'unit' => 'bar', 'firstname' => 'veny', 'surname' => 'mustermann', 'asciiFullname' => 'VENY MUSTERMANN', 'birthNumber' => '7004045000', 'phoneNumber' => '606146177' },

  { '@class' => 'Procedure', :key => 'beleni', 'unit' => 'foo', 'name' => 'Bělení', 'messageText' => 'Prijdte na beleni', 'type' => 'IN_CALENDAR', 'color' => 'FF0000', 'time' => 30 },
  { '@class' => 'Procedure', 'unit' => 'foo', 'name' => 'Extrakce', 'messageText' => 'Prijdte na trhani', 'type' => 'IN_CALENDAR', 'color' => '00FF00', 'time' => 60 },
  { '@class' => 'Procedure', 'unit' => 'foo', 'name' => 'Dovolená', 'messageText' => 'Mame dovolenou', 'type' => 'IMMEDIATE_MESSAGE' },

  { '@class' => 'Event', 'author' => 'veny', 'patient' => 'JanNovak', 'procedure' => 'beleni', 'text' => 'Message text', 'startTime' => '2012-10-30 10:10:00:000', 'length' => 30 },
  { '@class' => 'Event', 'author' => 'veny', 'patient' => 'me', 'procedure' => 'beleni', 'text' => 'Message text', 'startTime' => '2013-06-20 15:00:00:000', 'length' => 60 }
]
