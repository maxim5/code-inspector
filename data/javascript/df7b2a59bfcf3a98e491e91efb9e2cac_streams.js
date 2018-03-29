stream.permissions.write(function(eventName) {
	return eventName == 'events' && this.userId;
});

stream.permissions.read(function(eventName) {
	return this.userId == eventName;
});

stream.on('events', function(message) {
	var game = Games.findOne(message.game_id, {
		fields: {
			players: 1
		}
	});
	if (game.players.white === this.userId)
		stream.emit(game.players.black, message);
	else if (game.players.black === this.userId)
		stream.emit(game.players.white, message);
});