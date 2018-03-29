package org.mix3.twitter_bot_gae_wicket.page;

import org.apache.wicket.PageParameters;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ExternalLink;

import com.google.appengine.api.users.User;
import com.google.appengine.api.users.UserService;
import com.google.appengine.api.users.UserServiceFactory;

public class Manage extends MyAbstractWebPage{
    public Manage(PageParameters parameters) {
    	super(parameters);
    	
		UserService userService = UserServiceFactory.getUserService();
		User user = userService.getCurrentUser();
		add(new Label("label", user.getNickname()));
		add(new ExternalLink("logout", userService.createLogoutURL("/manage")));
    }
}
