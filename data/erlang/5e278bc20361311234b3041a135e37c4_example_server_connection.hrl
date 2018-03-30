% example of client state (here for IRC connection)

-record(client_state, {
	state=notlogged, % logged, quiting
	welcomed=no,
	reason=noreason, % quit reason
	username=undefined,
	realname=undefined,
	servername=undefined,
	hostname0=undefined,
	nick=undefined,
	fullnick=undefined, % nick!user@host
	hostaddr,
	hostname,
	port, % client side port
	channels,
	nickshandler=undefined
}).
