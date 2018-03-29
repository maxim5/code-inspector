package science.hack.dispatcher;

import java.io.IOException;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import science.hack.controller.ChatController;
import science.hack.service.ConversationService;
import science.hack.service.LocationService;
import science.hack.service.SatelliteService;
import science.hack.service.UserService;

@SuppressWarnings("serial")
public class ChatDispatcher extends HttpServlet {
    private static final ChatController controller = new ChatController(new ConversationService(new UserService(), new LocationService(), new SatelliteService()));
    
    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        controller.processChat(request, response);
    }

    @Override
    public void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        controller.processChat(request, response);
    }
}
