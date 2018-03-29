Interface = Class.extend({
	init: function(server){
		this.server = server
		
		this.form = new Form(this)
		$('body').append(this.form.node)
		
		this.list = new List()
		$('body').append(this.list.node)
	},
	load: function(names, days){
		this.form.disabled(true)
		this.server.getActivity(
			names, days,
			this.loadUsers.bind(this),
			this.error.bind(this)
		)
	},
	loadUsers: function(json){
		this.form.disabled(false)
		this.list.clear()
		this.list.append(json.map(User.fromJSON))
	},
	error: function(message){
		this.form.disabled(false)
		alert(message)
	}
})
