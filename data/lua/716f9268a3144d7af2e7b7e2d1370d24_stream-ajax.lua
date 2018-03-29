local stream_id = util.Decrypt(form.token)
stream_id = tonumber(stream_id) or stream_id
if generic.Permitted(stream_id,form.permit) then
	activity.Stream{ stream_id,
		token = form.token,
		permit = form.permit,
		name = form.name,
		before = form.before,
		start = form.start,
		after = form.after,
		maximum = form.maximum,
		paged = true,
		 -- although security sensitive, we do not need to check if the following are permitted as the target of those functions should perform that check
		reply = form.reply,
		post = form.post,
		remove = form.remove,
	}
end